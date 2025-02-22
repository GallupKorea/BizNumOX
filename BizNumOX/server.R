server <- function(input, output, session) {
  
  # ReactiveVal to store uploaded data
  uploaded_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  final_results <- reactiveVal(NULL)
  invalid_crn_data <- reactiveVal(NULL)
  
  # 파일 업로드 후 컬럼 선택 옵션 업데이트
  observeEvent(input$file1, {
    req(input$file1)
    data <- read_xlsx(input$file1$datapath)
    uploaded_data(data)
    
    # 열 선택 드롭다운 생성
    output$column_selector_preprocess <- renderUI({
      selectInput("preprocess_column", "전처리할 열 선택", choices = names(data), selected = NULL)
    })
    
    output$column_selector_api <- renderUI({
      selectInput("api_column", "API 조회할 열 선택", choices = names(data), selected = NULL)
    })
  })
  
  # 파일 미리보기
  output$file_preview <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), editable = TRUE, options = list(
      pageLength = 100,
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel'),
      filter = "top"  # 필터 기능을 테이블 상단에 추가
    ))
  })
  
  # 전처리 수행 (숫자만 남기기)
  observeEvent(input$process_button, {
    req(uploaded_data(), input$preprocess_column)
    data <- uploaded_data()
    
    column <- input$preprocess_column
    data[[column]] <- gsub("[^0-9]", "", data[[column]])
    
    # 저장된 데이터 업데이트
    processed_data(data)
    
    # 잘못된 사업자등록번호 목록 추출
    invalid_crn_data <- data[which(nchar(data[[column]]) != 10), ]
    invalid_crn_data(invalid_crn_data)  # reactive에 저장
    
    # 잘못된 사업자등록번호 보기 버튼 활성화
    output$show_invalid_crn_button <- renderUI({
      actionButton("show_invalid_crn", "잘못된 사업자등록번호 보기")
    })
    
    # 원래 테이블 보기 버튼 활성화
    output$show_original_data_button <- renderUI({
      actionButton("show_original_data", "원래 테이블 보기")
    })
    
    valid_count <- sum(nchar(data[[column]]) == 10)
    invalid_count <- sum(nchar(data[[column]]) != 10)
    
    output$preprocessing_status <- renderText("전처리 완료: 숫자만 남김.")
    output$valid_crn_count <- renderText(paste("유효한 사업자등록번호:", valid_count))
    output$invalid_crn_count_summary <- renderText(paste("잘못된 사업자등록번호:", invalid_count))
  })
  
  # "잘못된 사업자등록번호 보기" 버튼 클릭 시 잘못된 사업자등록번호만 필터링
  observeEvent(input$show_invalid_crn, {
    output$file_preview <- renderDT({
      datatable(invalid_crn_data(), editable = TRUE, options = list(
        pageLength = 100,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        filter = "top"  # 필터 기능을 테이블 상단에 추가
      ))
    })
  })
  
  # "원래 테이블 보기" 버튼 클릭 시 원래 데이터로 돌아가기
  observeEvent(input$show_original_data, {
    output$file_preview <- renderDT({
      datatable(processed_data(), editable = TRUE, options = list(
        pageLength = 100,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        filter = "top"  # 필터 기능을 테이블 상단에 추가
      ))
    })
  })
  
  # 셀 편집 시 수정된 값 반영 (이 부분이 중요!)
  observeEvent(input$file_preview_cell_edit, {
    info <- input$file_preview_cell_edit
    str(info)  # For debugging
    
    row <- info$row
    col <- info$col
    value <- info$value
    
    # 데이터 수정
    data <- processed_data()
    data[row, col] <- value
    
    processed_data(data)  # 업데이트된 데이터 반영
  })
  
  # 업데이트 버튼을 눌렀을 때
  observeEvent(input$update_button, {
    req(processed_data())
    updated_data <- processed_data()  # 수정된 데이터
    
    # 원본 데이터에 수정된 값 반영
    uploaded_data(updated_data)  # uploaded_data는 원본 데이터
    
    # 확인 메시지 표시
    showNotification("데이터가 업데이트되었습니다.", type = "message")
    
    # 원본 데이터를 재표시
    output$file_preview <- renderDT({
      datatable(updated_data, editable = TRUE, options = list(
        pageLength = 100,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        filter = "top"  # 필터 기능을 테이블 상단에 추가
      ))
    })
  })
  
  # API 호출 및 데이터 조회
  get_api_data <- function(crn_chunk, api_key) {
    data <- toJSON(list(b_no = crn_chunk))
    response <- POST(
      url = sprintf("https://api.odcloud.kr/api/nts-businessman/v1/status?serviceKey=%s", api_key),
      body = data,
      encode = "json",
      config = add_headers(Accept = "application/json", `Content-Type` = "application/json")
    )
    
    content <- fromJSON(content(response, "text"), flatten = TRUE)
    
    if (!is.null(content$data)) {
      data_frame <- data.frame(content$data, stringsAsFactors = FALSE)
      data_frame$CRN <- crn_chunk
      return(data_frame)
    } else {
      return(data.frame(CRN = crn_chunk, stringsAsFactors = FALSE))
    }
  }
  
  observeEvent(input$check_button, {
    req(processed_data(), input$api_column)
    data <- processed_data()
    
    column <- input$api_column
    crn_list <- data[[column]]
    
    api_key <- "YgB%2F8EYn%2BBeebIgfD6jibP30%2FjQL8dFcDyEZoHqCWhiepCW4OWTgFPCNhTK63I8FOI9qPsJdE8tFk4bStzmHQQ%3D%3D"
    
    crn_chunks <- split(crn_list, ceiling(seq_along(crn_list) / 100))
    
    progress <- Progress$new(session)
    progress$set(message = "API 호출 중...", value = 0)
    
    results <- bind_rows(lapply(crn_chunks, function(chunk) {
      progress$inc(1 / length(crn_chunks))
      get_api_data(chunk, api_key)
    }))
    
    final_df <- cbind(data, results)
    final_results(final_df)
    
    output$result_table <- renderDT({
      datatable(final_df, options = list(pageLength = 100))
    })
    
    output$status_message <- renderText({
      paste("조회가 완료되었습니다. 영업 중인 사업체:", nrow(final_df))
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        paste("사업자_상태_조회결과_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(final_df, file, encoding = "UTF-8")
      }
    )
    
    progress$close()
  })
  
  # 데이터 유지
  output$result_table <- renderDT({
    req(final_results())
    datatable(final_results(), options = list(pageLength = 100))
  })
}
