# Calculate remaining cost to me after per-diems for the CEBP Symposium

# Load libraries
library(tidyverse)
library(lubridate)
library(shiny)
as.numeric(str_extract(days(ymd("2023-01-01") - ymd("2023-01-05")), "^\\d+"))
ymd("2023-01-01") > ymd("2023-01-05")

calculate_per_diems <- function(depart_time, return_time, start_date, end_date) {
  

  if (end_date < start_date) {
    stop("Return date must be after departing date")
  } else {
    # Get the number of travel days (minus first and last)
    ndays <- as.numeric(str_extract(days(end_date - start_date), "^\\d+")) - 1
    
    middle_day_breakfast <- rep(TRUE, ndays)
    middle_day_lunch <- rep(TRUE, ndays)
    middle_day_dinner <- rep(TRUE, ndays)
  }

  first_day_breakfast <- switch(
    depart_time,
    "Before breakfast" = TRUE,
    "After breakfast, before lunch" = FALSE,
    "After lunch, before dinner" = FALSE,
    "After dinner" = FALSE)
  
  first_day_lunch <- switch(
    depart_time,
    "Before breakfast" = TRUE,
    "After breakfast, before lunch" = TRUE,
    "After lunch, before dinner" = FALSE,
    "After dinner" = FALSE)
  
  first_day_dinner <- switch(
    depart_time,
    "Before breakfast" = TRUE,
    "After breakfast, before lunch" = TRUE,
    "After lunch, before dinner" = TRUE,
    "After dinner" = FALSE)
  
  last_day_breakfast <- switch(
    return_time,
    "Before breakfast" = FALSE,
    "After breakfast, before lunch" = TRUE,
    "After lunch, before dinner" = TRUE,
    "After dinner" = TRUE)
  
  last_day_lunch <- switch(
    return_time,
    "Before breakfast" = FALSE,
    "After breakfast, before lunch" = FALSE,
    "After lunch, before dinner" = TRUE,
    "After dinner" = TRUE)
  
  last_day_dinner <- switch(
    return_time,
    "Before breakfast" = FALSE,
    "After breakfast, before lunch" = FALSE,
    "After lunch, before dinner" = FALSE,
    "After dinner" = TRUE)
  
  return(
    data.frame(
      dates = as.character(seq.Date(from = start_date, to = end_date, by = "1 day")),
      breakfast = c(first_day_breakfast, middle_day_breakfast, last_day_breakfast),
      lunch = c(first_day_lunch, middle_day_lunch, last_day_lunch),
      dinner = c(first_day_dinner, middle_day_dinner, last_day_dinner)
      )
  )
}


total_per_diems <- function(daily_per_diems, breakfast, lunch, dinner) {
  daily_per_diems %>% 
  pivot_longer(cols = where(is.logical), names_to = "Meal", values_to = "covered") %>% 
  group_by(Meal) %>% 
  summarize(`Number of meals` = sum(covered)) %>% 
  mutate(
    Meal = factor(Meal, 
                  levels = c("breakfast", "dinner", "lunch"), 
                  labels = c("Breakfast", "Lunch", "Dinner")),
    `Per diem` = c(breakfast, lunch, dinner),
    Total = `Number of meals` * `Per diem`) %>% 
  add_row(
    Meal = "Total",
    `Number of meals` = sum(.$`Number of meals`),
    `Per diem` = sum(.$`Per diem`),
    Total = sum(.$Total)
  )
}

perDiemCalculatorUI <- function(id) {
  
    sidebarLayout(
      sidebarPanel(
        # Select a range of dates between which travel will take place
        shiny::dateRangeInput(
          inputId = NS(id, "date_range"), 
          label = "When are you travelling?",
          start = Sys.Date(),
          end = Sys.Date() + days("7"),
          min = "2023-01-01",
          max = "2025-01-01",
          format = "dd/mm/yyyy",
          separator = " - "),
        
        # Select the time of departure
        shiny::selectInput(
          inputId = NS(id, "depart_time"), 
          label = "When are you departing?", 
          choices = c("Before breakfast", 
                      "After breakfast, before lunch", 
                      "After lunch, before dinner",
                      "After dinner")),
        
        # Select the time of returning
        shiny::selectInput(
          inputId = NS(id, "return_time"), 
          label = "When are you returning?", 
          choices = c("Before breakfast", 
                      "After breakfast, before lunch", 
                      "After lunch, before dinner",
                      "After dinner")),
        
        # How much do you get for breakfast?
        shiny::numericInput(
          inputId = NS(id, "breakfast_per_diem"),
          label = "Breakfast per diem",
          value = 10),
        
        # How much do you get for lunch?
        shiny::numericInput(
          inputId = NS(id, "lunch_per_diem"),
          label = "Lunch per diem",
          value = 15),
        
        # How much do you get for dinner?
        shiny::numericInput(
          inputId = NS(id, "dinner_per_diem"),
          label = "Dinner per diem",
          value = 25),
        
      ),
      mainPanel(
        fluidRow(
          column(
            width = 5,
            h4("Daily breakdown of meals"),
            tableOutput(NS(id, "table"))
          ),
          column(
            width = 5,
            h4("Summary total of meals and cost"),
            tableOutput(NS(id, "totals"))
          )
        ),
        fluidRow(
          h4("Travel summary"),
          uiOutput(NS(id, "summary"))
        )
      )
    )
      
}

perDiemCalculatorServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    start_date <- reactive(input$date_range[1])
    end_date <- reactive(input$date_range[2])
    start_time <- reactive(input$depart_time)
    end_time <- reactive(input$return_time)
    breakfast_pd = reactive(input$breakfast_per_diem)
    lunch_pd = reactive(input$lunch_per_diem)
    dinner_pd = reactive(input$dinner_per_diem)
    
    output$summary <- renderUI({
      paste0("Travel commences on ", 
             start_date(), " ", str_to_lower(start_time()), 
             ", and finishes on ", 
             end_date(), " ", str_to_lower(end_time()), ".",
             " This gives a total of ", end_date() - start_date() + 1, " days of travel.")
      
    })
    
    output$table <- renderTable({
      calculate_per_diems(depart_time = start_time(), 
                          return_time = end_time(), 
                          start_date = ymd(start_date()),
                          end_date = ymd(end_date()))
    })

  
    output$totals <- renderTable({
      calculate_per_diems(depart_time = start_time(), 
                          return_time = end_time(), 
                          start_date = ymd(start_date()),
                          end_date = ymd(end_date())) %>% 
      total_per_diems(., 
                      breakfast = breakfast_pd(), 
                      lunch = lunch_pd(), 
                      dinner = dinner_pd())
    })
    
  })
  
}

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
#perDiemCalculatorApp()

