library(shiny)
library(DT)  # For rendering tables
library(shinyjs)  # For progress bar control
library(openxlsx)
library(dplyr)
library(httr)
library(jsonlite)
library(readxl)

ui <- fluidPage(
  useShinyjs(),  # For JS functionality
  
  titlePanel("사업자등록번호 조회 시스템"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "엑셀 파일 업로드 (xlsx)", accept = ".xlsx"),
      
      uiOutput("column_selector_preprocess"),
      actionButton("process_button", "전처리 수행"),
      
      uiOutput("column_selector_api"),
      actionButton("check_button", "API 조회 시작"),
      
      downloadButton("download_data", "결과 다운로드"),
      
      textOutput("preprocessing_status"),
      textOutput("valid_crn_count"),
      textOutput("invalid_crn_count_summary"),
      textOutput("status_message"),
      uiOutput("show_invalid_crn_button"),  # 버튼을 동적으로 추가할 수 있도록 설정
      uiOutput("show_original_data_button"),  # 원래 테이블로 돌아가기 버튼
      actionButton("update_button", "업데이트")  # 업데이트 버튼 추가
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("파일 미리보기", DTOutput("file_preview")),
        tabPanel("조회 결과", DTOutput("result_table"))
      )
    )
  )
)
