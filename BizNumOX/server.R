# app_server.R

library(shiny)
library(DT)
library(shinyjs)
library(openxlsx)
library(dplyr)
library(httr)
library(jsonlite)
library(readxl)

server <- function(input, output, session) {
  
  # ReactiveVal
  uploaded_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  final_results <- reactiveVal(NULL)
  invalid_crn_data <- reactiveVal(NULL)
  
  # 파일 업로드
  observeEvent(input$file1, {
    req(input$file1)
    
    data <- read_xlsx(input$file1$datapath)
    uploaded_data(data)
    
    # 전처리/조회 열 선택
    output$column_selector_preprocess <- renderUI({
      selectInput("preprocess_column", "전처리할 열 선택 (사업자등록번호)",
                  choices = names(data), selected = NULL)
    })
    output$column_selector_api <- renderUI({
      selectInput("api_column", "API 조회할 열 선택 (사업자등록번호)",
                  choices = names(data), selected = NULL)
    })
    
    # 업로드된 데이터 표시
    output$file_preview <- renderDT({
      datatable(
        data,
        editable = TRUE,
        # 버튼/필터/스크롤X 설정
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',               # B:Buttons, f:filter, r:processing, t:table, i:info, p:pagination
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  # 전처리 수행
  observeEvent(input$process_button, {
    req(uploaded_data())
    req(input$preprocess_column)
    
    data <- uploaded_data()
    column <- input$preprocess_column
    
    # 숫자만 남기기
    data[[column]] <- gsub("[^0-9]", "", data[[column]])
    processed_data(data)
    
    # 잘못된 사업자등록번호 목록
    invalid_df <- data[which(nchar(data[[column]]) != 10), ]
    invalid_crn_data(invalid_df)
    
    # 유효/잘못된 개수
    valid_count <- sum(nchar(data[[column]]) == 10)
    invalid_count <- sum(nchar(data[[column]]) != 10)
    
    output$preprocessing_status <- renderText("전처리가 완료되었습니다. (숫자만 남김)")
    output$valid_crn_count <- renderText(paste("유효한 사업자등록번호 개수:", valid_count))
    output$invalid_crn_count_summary <- renderText(paste("잘못된 사업자등록번호 개수:", invalid_count))
    
    showNotification("전처리가 완료되었습니다.", type = "message")
    
    # 전처리된 데이터 표시
    output$file_preview <- renderDT({
      datatable(
        data,
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE
        )
      )
    })
    
    # 잘못된/전체 보기 버튼 활성화
    if (invalid_count > 0) {
      output$show_invalid_crn_button <- renderUI({
        actionButton("show_invalid_crn",
                     paste0("잘못된 사업자번호만 보기 (", invalid_count, "건)"),
                     class = "btn btn-primary")
      })
    } else {
      output$show_invalid_crn_button <- renderUI({NULL})
    }
    
    output$show_original_data_button <- renderUI({
      actionButton("show_original_data", "전체 테이블 보기", class = "btn btn-primary")
    })
  })
  
  # 잘못된 사업자번호만 보기
  observeEvent(input$show_invalid_crn, {
    req(processed_data())
    dt_invalid <- invalid_crn_data()
    
    showNotification("잘못된 사업자등록번호만 표시합니다.", type = "message")
    output$file_preview <- renderDT({
      datatable(
        dt_invalid,
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  # 전체 테이블 보기
  observeEvent(input$show_original_data, {
    req(processed_data())
    dt <- processed_data()
    
    showNotification("전체 테이블을 표시합니다.", type = "message")
    output$file_preview <- renderDT({
      datatable(
        dt,
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  # 셀 편집 시 실시간 반영
  observeEvent(input$file_preview_cell_edit, {
    info <- input$file_preview_cell_edit
    if (is.null(info)) return()
    
    row <- info$row
    col <- info$col
    value <- info$value
    
    dt <- processed_data()
    dt[row, col] <- value
    processed_data(dt)
  })
  
  # 수정 내용 저장 (원본 데이터에 반영)
  observeEvent(input$save_changes_button, {
    req(processed_data())
    showNotification("수정 내용을 원본 데이터에 반영합니다.", type = "message")
    
    uploaded_data(processed_data())
    
    # 저장 후 테이블 다시 표시
    output$file_preview <- renderDT({
      datatable(
        processed_data(),
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  # API 호출 함수
  get_api_data <- function(crn_chunk, api_key) {
    body_data <- toJSON(list(b_no = crn_chunk))
    response <- POST(
      url = sprintf("https://api.odcloud.kr/api/nts-businessman/v1/status?serviceKey=%s", api_key),
      body = body_data,
      encode = "json",
      config = add_headers(Accept = "application/json", `Content-Type` = "application/json")
    )
    
    content_json <- fromJSON(content(response, "text"), flatten = TRUE)
    if (!is.null(content_json$data)) {
      df <- data.frame(content_json$data, stringsAsFactors = FALSE)
      df$CRN <- crn_chunk
      return(df)
    } else {
      return(data.frame(CRN = crn_chunk, stringsAsFactors = FALSE))
    }
  }
  
  # API 조회 실행
  observeEvent(input$check_button, {
    req(processed_data())
    req(input$api_column)
    
    data <- processed_data()
    col_api <- input$api_column
    
    if (!col_api %in% colnames(data)) {
      showNotification("API 조회할 열을 올바르게 선택해주세요.", type = "error")
      return()
    }
    
    crn_list <- data[[col_api]]
    api_key <- "YgB%2F8EYn%2BBeebIgfD6jibP30%2FjQL8dFcDyEZoHqCWhiepCW4OWTgFPCNhTK63I8FOI9qPsJdE8tFk4bStzmHQQ%3D%3D"
    
    progress <- Progress$new(session)
    progress$set(message = "API 조회 중...", value = 0)
    
    crn_chunks <- split(crn_list, ceiling(seq_along(crn_list) / 100))
    
    results <- bind_rows(lapply(seq_along(crn_chunks), function(i) {
      progress$inc(1 / length(crn_chunks),
                   detail = paste("조회 중...", i, "/", length(crn_chunks)))
      chunk <- crn_chunks[[i]]
      get_api_data(chunk, api_key)
    }))
    
    progress$close()
    
    final_df <- cbind(data, results)
    final_results(final_df)
    
    output$result_table <- renderDT({
      datatable(
        final_df,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          scrollX = TRUE
        )
      )
    })
    
    output$status_message <- renderText({
      paste("조회 완료. 총", nrow(final_df), "건의 사업체 정보가 확인되었습니다.")
    })
    
    showNotification("API 조회가 완료되었습니다.", type = "message")
    
    # 다운로드 핸들러
    output$download_data <- downloadHandler(
      filename = function() {
        paste("사업자_상태_조회결과_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(final_df, file, encoding = "UTF-8")
      }
    )
  })
  
  # 조회 결과 탭
  output$result_table <- renderDT({
    req(final_results())
    datatable(
      final_results(),
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE
      )
    )
  })
}
