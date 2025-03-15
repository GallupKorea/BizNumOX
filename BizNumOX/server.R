library(shiny)
library(DT)
library(shinyjs)
library(openxlsx)
library(dplyr)
library(httr)
library(jsonlite)
library(readxl)
library(future.apply)

# 서버 측 파일 크기 제한 설정 (100MB)
options(shiny.maxRequestSize = 100 * 1024^2)

# 병렬 처리 시 전역 변수 직렬화 최대 크기 (3GB)
options(future.globals.maxSize = 3 * 1024^3)

# 병렬 플랜 설정
plan(multisession, workers = 4)

##########################################################
# API 호출 함수
##########################################################
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

##########################################################
# 변수명 변경 함수 (원하는 경우에만 사용)
##########################################################
rename_api_vars <- function(df) {
  # 1) 먼저 컬럼명을 전부 소문자로 변환
  names(df) <- tolower(names(df))
  
  # 2) CRN(대소문자 혼합 가능) → crn → 제거
  if ("crn" %in% names(df)) {
    df$crn <- NULL
  }
  
  # 3) 변수명 매핑: 소문자 기준
  var_map <- c(
    b_no               = "사업자번호 확인",
    b_stt              = "사업 운영 여부",
    b_stt_cd           = "사업 운영 여부 코드",
    tax_type           = "과세 유형",
    tax_type_cd        = "과세 유형 코드",
    end_dt             = "폐업일",
    utcc_yn            = "단위과세전환폐업여부",
    tax_type_change_dt = "최근 과세 유형 전환 일자",
    invoice_apply_dt   = "세금계산서 적용 일자",
    rbf_tax_type       = "직전 과세 유형",
    rbf_tax_type_cd    = "직전 과세 유형 코드"
  )
  
  # 4) 실제 존재하는 열만 rename
  for (old_col in names(var_map)) {
    if (old_col %in% names(df)) {
      new_col <- var_map[[old_col]]
      names(df)[names(df) == old_col] <- new_col
    }
  }
  return(df)
}

##########################################################
# server 함수
##########################################################
server <- function(input, output, session) {
  
  uploaded_data <- reactiveVal(NULL)    # 업로드된 원본 데이터
  processed_data <- reactiveVal(NULL)   # 전처리/수정된 데이터
  final_results <- reactiveVal(NULL)    # API 조회 최종 결과
  invalid_crn_data <- reactiveVal(NULL) # 잘못된 사업자등록번호 목록
  
  ##########################################################
  # 파일 업로드
  ##########################################################
  observeEvent(input$file1, {
    req(input$file1)
    
    if (grepl(".xlsx$", input$file1$name)) {
      data <- read_xlsx(input$file1$datapath)
    } else if (grepl(".csv$", input$file1$name)) {
      data <- read.csv(
        input$file1$datapath,
        stringsAsFactors = FALSE,
        fileEncoding = "CP949",  # 한글 CSV는 CP949
        check.names = FALSE
      )
    }
    
    uploaded_data(data)
    
    # 전처리/조회 열 선택
    output$column_selector_preprocess <- renderUI({
      selectInput("preprocess_column", "전처리할 열 선택 (사업자등록번호)",
                  choices = names(data), selected = NULL)
    })
    output$column_selector_api <- renderUI({
      selectInput("api_column", "운영 여부 조회할 열 선택 (사업자등록번호)",
                  choices = names(data), selected = NULL)
    })
    
    # 업로드된 데이터 표시
    output$file_preview <- renderDT({
      datatable(
        data,
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy','csv','excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  ##########################################################
  # 전처리 수행 (선택적)
  ##########################################################
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
    
    # 전처리된 데이터 테이블 표시
    output$file_preview <- renderDT({
      datatable(
        data,
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy','csv','excel'),
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
      output$show_invalid_crn_button <- renderUI(NULL)
    }
    
    output$show_original_data_button <- renderUI({
      actionButton("show_original_data", "전체 테이블 보기", class = "btn btn-primary")
    })
  })
  
  ##########################################################
  # 잘못된 사업자번호만 보기
  ##########################################################
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
          buttons = c('copy','csv','excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  ##########################################################
  # 전체 테이블 보기
  ##########################################################
  observeEvent(input$show_original_data, {
    # 전처리하지 않았다면 processed_data()가 NULL일 수 있음
    dt <- processed_data()
    if (is.null(dt)) {
      dt <- uploaded_data()  # 전처리하지 않은 원본 데이터 사용
    }
    
    showNotification("전체 테이블을 표시합니다.", type = "message")
    output$file_preview <- renderDT({
      datatable(
        dt,
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy','csv','excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  ##########################################################
  # 셀 편집 시 실시간 반영
  ##########################################################
  observeEvent(input$file_preview_cell_edit, {
    info <- input$file_preview_cell_edit
    if (is.null(info)) return()
    
    # 전처리하지 않았다면 processed_data()가 NULL일 수 있음
    dt <- processed_data()
    if (is.null(dt)) {
      dt <- uploaded_data()
    }
    
    row <- info$row
    col <- info$col
    value <- info$value
    
    dt[row, col] <- value
    processed_data(dt)
  })
  
  ##########################################################
  # 수정 내용 저장 (원본 데이터에 반영)
  ##########################################################
  observeEvent(input$save_changes_button, {
    dt <- processed_data()
    if (is.null(dt)) {
      dt <- uploaded_data()
    }
    
    showNotification("수정 내용을 원본 데이터에 반영합니다.", type = "message")
    uploaded_data(dt)
    
    # 테이블 다시 표시
    output$file_preview <- renderDT({
      datatable(
        dt,
        editable = TRUE,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy','csv','excel'),
          scrollX = TRUE
        )
      )
    })
  })
  
  ##########################################################
  # 병렬 처리로 운영 여부 조회
  ##########################################################
  observeEvent(input$check_button, {
    
    # 1) 전처리 데이터가 있으면 그걸 사용, 없으면 업로드된 원본 데이터 사용
    data <- processed_data()
    if (is.null(data)) {
      data <- uploaded_data()
    }
    
    req(data)  # 데이터가 반드시 존재해야 함
    
    # 2) 운영 여부 조회할 열이 유효한지 확인
    req(input$api_column)
    col_api <- input$api_column
    if (!col_api %in% colnames(data)) {
      showNotification("운영 여부 조회할 열을 올바르게 선택해주세요.", type = "error")
      return()
    }
    
    # 3) 조회 시작 시간 기록
    start_time <- Sys.time()
    
    crn_list <- data[[col_api]]
    api_key <- "YgB%2F8EYn%2BBeebIgfD6jibP30%2FjQL8dFcDyEZoHqCWhiepCW4OWTgFPCNhTK63I8FOI9qPsJdE8tFk4bStzmHQQ%3D%3D"
    
    # 4) 청크 분할
    crn_chunks <- split(crn_list, ceiling(seq_along(crn_list) / 100))
    
    # 5) 진행 상태 표시 (병렬 작업)
    progress <- Progress$new(session)
    progress$set(message = "운영 여부 조회 중...", value = 0)
    
    # 6) 병렬 처리로 API 호출
    results_list <- future_lapply(seq_along(crn_chunks), function(i) {
      chunk <- crn_chunks[[i]]
      get_api_data(chunk, api_key)
    })
    
    progress$close()
    
    # 7) 결과 합치기
    results <- bind_rows(results_list)
    
    # 8) 변수명 변경 (원하면 주석 해제)
    results <- rename_api_vars(results)
    
    final_df <- cbind(data, results)
    final_results(final_df)
    
    # 9) 운영 여부 조회 소요 시간 계산
    end_time <- Sys.time()
    elapsed_sec <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
    
    # 10) 테이블 업데이트
    output$file_preview <- renderDT({
      datatable(
        final_df,
        extensions = c("Buttons"),
        options = list(
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy','csv','excel'),
          scrollX = TRUE
        )
      )
    })
    
    # 11) 상태 메시지 (소요 시간 포함)
    output$status_message <- renderText({
      paste0("조회 완료. 총 ", nrow(final_df),
             "건의 사업체 정보가 확인되었습니다. (소요 시간: ", elapsed_sec, "초)")
    })
    
    # 12) 알림
    showNotification(
      paste0("운영 여부 조회가 완료되었습니다. (총 ", elapsed_sec, "초 소요)"),
      type = "message"
    )
    
    # 13) 다운로드 핸들러
    output$download_data <- downloadHandler(
      filename = function() {
        paste("사업자_상태_조회결과_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(final_df, file, encoding = "UTF-8")
      }
    )
  })
}