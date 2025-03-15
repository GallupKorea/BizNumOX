library(shiny)
library(shinyjs)
library(bslib)  # Bootstrap 5 테마
library(DT)
library(showtext)  # 구글 폰트 사용

font_add_google("Nanum Gothic", "nanumgothic")
showtext_auto()

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",   # 원하는 bootswatch 테마
    primary = "#18BC9C",
    secondary = "#2C3E50"
  ),
  
  useShinyjs(),
  
  # 추가 CSS
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'nanumgothic', sans-serif;
        font-size: 14px;
        background-color: #f8f9fa;
        color: #333;
      }
      .nav-tabs .nav-link.active {
        background-color: #18BC9C !important;
        color: #fff !important;
      }
      .nav-tabs .nav-link {
        color: #2C3E50 !important;
        font-weight: 500;
        font-size: 14px;
      }
      .btn-primary {
        background-color: #18BC9C !important;
        border-color: #18BC9C !important;
        font-size: 13px;
        padding: 6px 10px;
        border-radius: 0.4rem;
      }
      .btn-primary:hover {
        background-color: #149e82 !important;
        border-color: #149e82 !important;
      }
      #download_data {
        background-color: #2C3E50 !important;
        border: none;
        color: #fff;
        margin-top: 10px;
        border-radius: 0.4rem;
        font-size: 13px;
        padding: 6px 10px;
      }
      #download_data:hover {
        background-color: #1f2c36 !important;
      }
      .dataTables_filter input {
        border-radius: 0.3rem;
        border: 1px solid #ccc;
        padding: 5px;
        font-size: 13px;
      }
    "))
  ),
  
  titlePanel("사업자등록번호 조회 시스템"),
  
  # 탭 구조로 안내/메인 분리
  tabsetPanel(
    # 사용 안내 탭
    tabPanel("사용 안내", icon = icon("info-circle"),
             fluidRow(
               column(
                 width = 12,
                 br(),
                 wellPanel(
                   h4("작업 순서 안내"),
                   tags$ul(
                     tags$li("1) 엑셀 파일 업로드"),
                     tags$li("2) 전처리할 열 선택 후 '전처리 수행'"),
                     tags$li("3) '잘못된 사업자번호만 보기' → 필요한 경우 셀 수정"),
                     tags$li("4) '수정 내용 저장'으로 원본 데이터에 반영"),
                     tags$li("5) '전체 테이블 보기'로 전체 데이터 확인"),
                     tags$li("6) '운영 여부 조회' 열 선택 후 '조회 실행'"),
                     tags$li("7) '결과 다운로드'로 Excel 파일 다운로드")
                   ),
                   p("전처리 후 운영 여부 조회를 진행해야 올바른 결과를 얻을 수 있습니다.")
                 )
               )
             )
    ),
    # 메인 화면 탭
    tabPanel("메인 화면", icon = icon("home"),
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h5("1단계: 파일 업로드"),
                   fileInput("file1", "엑셀 파일 또는 CSV 업로드", accept = c(".xlsx", ".csv"),
                             multiple = FALSE) 
                 ),
                 wellPanel(
                   h5("2단계: 전처리"),
                   uiOutput("column_selector_preprocess"),
                   actionButton("process_button", "전처리 수행", class = "btn btn-primary"),
                   
                   uiOutput("show_invalid_crn_button"),
                   uiOutput("show_original_data_button"),
                   
                   actionButton("save_changes_button", "수정 내용 저장", class = "btn btn-primary")
                 ),
                 wellPanel(
                   h5("3단계: 운영 여부 조회"),
                   uiOutput("column_selector_api"),
                   actionButton("check_button", "조회 실행", class = "btn btn-primary")
                 ),
                 downloadButton("download_data", "결과 다운로드"),
                 
                 tags$hr(),
                 strong("상태 메시지"),
                 textOutput("preprocessing_status"),
                 textOutput("valid_crn_count"),
                 textOutput("invalid_crn_count_summary"),
                 textOutput("status_message")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("전체 테이블", DTOutput("file_preview"))
                 )
               )
             )
    )
  )
)