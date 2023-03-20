# Load libraries
library(tidyverse)
library(lubridate)
library(shiny)

# Load functions
lapply(list.files(here::here("R/"), full.names = TRUE), source)


perDiemCalculatorApp <- function(filter = NULL) {
  ui <- fluidPage(
    titlePanel("Work travel per-diem calculator"),
    perDiemCalculatorUI("per-diem-calculator1")
  )
  
  server <- function(input, output, session) {
    perDiemCalculatorServer("per-diem-calculator1")
    
  }
  shinyApp(ui, server)
}

perDiemCalculatorApp()