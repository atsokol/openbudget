# Shiny app for Open Budget - PRESS "RUN APP" button to launch

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, shiny, httr, jsonlite, readxl, writexl, here, DT)

# Load city codes
city_codes <- read_excel(here("data/Open Budget city budget codes.xlsx"))
current_year <- year(Sys.Date())

# Source required functions from other files
source(here("R code/shiny_app/download.R"))
source(here("R code/shiny_app/summarise.R"))

shinyApp(
  ui = fluidPage(
    titlePanel("Open Budget: Ukraine municipality financials"),
    sidebarLayout(
      sidebarPanel(
        tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
        selectInput("city", "Select city", 
                    choices = city_codes$city),
        numericInput("year_from", label = "Year from", value = 2022, min = 2021, max = current_year),
        numericInput("year_to", label = "Year to", value = 2023, min = 2021, current_year),
        actionButton("action", label = "Load data", icon = icon("paper-plane")),
        hr(),
        radioButtons("frac_period", "Select period", 
                     choices = list("Latest" = 1, "3 months" = 2, "6 months" = 3, "9 months" = 4, "FY" = 5),
                     selected = 1),
        radioButtons("button", label = "Display table",
                     choices = list("Financial summary" = 1, "Financial model" = 2), 
                     selected = 1),
        hr(),
        downloadButton("downloadData", "Download file"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage"))
      ),
      mainPanel( 
        DT::dataTableOutput("table")
      )
    )
  ),
  
  server = function(input, output, session) {
    BUDGETCODE <- reactive({
      city_chooser(input$city)
      })
    YEAR <- reactive({
      seq(input$year_from, input$year_to)
      })
    
    data <- eventReactive(input$action, {
      download_data(BUDGETCODE(), YEAR())
      })
    
    table <- reactive({
      summarise_data(data(), input$frac_period)
      })
    
    display_table <- reactive({
      if (input$button == 1) {
        table()[['SUMMARY_UPDATE']]
      } else {
        table()[['SUMMARY_MODEL']]
      }
    })

    output$table <- renderDataTable(datatable(
      display_table(), 
      options = list(pageLength = 20, autoWidth = TRUE)
      ) |> 
        formatRound(3:ncol(display_table()), interval = 3, digits = 0))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$city," ", input$year_from, "-", input$year_to, ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(table(), file)
      }
    )
    
  }
)

