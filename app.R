# Load packages
wants <- c('shiny','shinythemes','config','plotly','rjson','qdap','pivottabler')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(readr)
library(shiny)
library(shinythemes)
library(plotly)

library(pivottabler)

source('common.R')

period_choices <- get_choices('period')
assessment_value_choices <- get_choices("assessmentValue")

ui <- fluidPage(
  theme = shinytheme("lumen")
  , sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "period"
        , multiple = T
        , label = strong("Period:")
        , choices = period_choices
        , selected = as.vector(unlist(period_choices)))
      , verticalLayout(
        h4("Segmentation related to evidence")
        , verticalLayout(
          selectInput(
            inputId = "technologyType"
            , multiple = T
            , label = strong("Technology type:")
            , choices = get_choices("technologyType"))
          , checkboxInput('is_technology', 'is select by technology enabled?')
        )
        , htmlOutput("technologySelector")
      )
      , verticalLayout(
        h4("Segmentation related to criteria")
        , verticalLayout(
          selectInput(
            inputId = "dimension"
            , multiple = T
            , label = strong("Dimension:")
            , choices = get_choices("dimension"))
          , checkboxInput('is_criteria', 'is selection by criteria enabled?')
        )
        , htmlOutput("criteriaSelector")
      )
      , fluidRow(actionButton("updateDataGraphButton", "Update Data/Graph", class = "btn-primary")
                 , downloadButton("downloadCSVButton", "Download .csv"))
    )
    # Output: Description, lineplot, and reference
    , mainPanel(
      navbarPage(
        "Analytics"
        , id = "mainNavigation"
        , tabPanel(
          "Evidence-Based Assessment Result"
          , value = "assessment-result"
          , verticalLayout(
            checkboxGroupInput(
              "assessmentValue"
              , strong("Assessment value:")
              , assessment_value_choices
              , selected = as.vector(unlist(assessment_value_choices))
              , inline = T)
            , radioButtons(
              "assessmentFrom"
              , strong("Assessment from:")
              , list("all (todos)"="all", "from main appraiser (do consolidador)"="main", "without main appraiser"="wo-main")
              , inline = T)
            , tabsetPanel(
              type = "tabs", id = 'assessmentResultGraph'
              , tabPanel("Radar Chart", value = 'radar', htmlOutput("assessmentResultHTMLRadarChart"))
              , tabPanel("Stacked Bar Chart", value = 'stackedbar', htmlOutput("assessmentResultHTMLStackedBarChart"))
              , tabPanel("Pie Chart", value = 'pie', htmlOutput("assessmentResultHTMLPieChart"))
              , tabPanel("Table", value = 'table', dataTableOutput("assessmentResultTable"))
            )
          )
        )
      )
    )
    , position = "right"
  )
)

# Define server for app
server <- function(input, output) {
  
  # rendering UI for parameters
  output$criteriaSelector <- renderUI({
    if (input$is_criteria && length(input$dimension) > 0) {
      selectInput(
        choices = get_choices("criteria", list(dimension=input$dimension))
        , inputId = "criteria", label = strong("Criteria:"), multiple = T)
    }
  })
  
  output$technologySelector <- renderUI({
    if (input$is_technology && length(input$technologyType) > 0) {
      technology_choices <- get_choices("technology", list(technologyType=input$technologyType))
      names(technology_choices) <- paste0('Tec-',names(technology_choices))
      selectInput(
        choices = technology_choices
        , inputId = "technology", label = strong("Technology:"), multiple = T)
    }
  })
  
  # get params for the application
  app_params <- eventReactive(input$updateDataGraphButton, {
    validate(need((length(input$period) > 0), 'Error: At least one period must be selected!!'))
    filters <- list(period = input$period
                    , dimension = input$dimension
                    , technologyType = input$technologyType)
    
    if (length(input$criteria) > 0 && input$is_criteria) {
      filters[["criteria"]] <- input$criteria
    }
    if (length(input$technology) > 0 && input$is_technology) {
      filters[["technology"]] <- input$technology
    }
    
    options <- list()
    if (input$mainNavigation == 'assessment-result') {
      options[['assessmentFrom']] <- input$assessmentFrom
      options[['assessmentValue']] <- input$assessmentValue
      if (input$assessmentResultGraph == 'radar') {
        options[['measurement']] <- input$measurementRadarChart
        options[['limit']] <- input$limitRadarChart
        options[['orderBy']] <- input$orderByRadarChart
      } else if (input$assessmentResultGraph == 'stackedbar') {
        options[['measurement']] <- input$measurementStackedBarChart
        options[['formula']] <- input$formulaStackedBarChart
      } else if (input$assessmentResultGraph == 'pie') {
        options[['formula']] <- input$formulaPieChart
      }
    }
    list(dtype = input$mainNavigation, filters = filters, options = options)
  })
  
  # rendering RadarChart for evidence-based assessment result
  output$assessmentResultHTMLRadarChart <- renderUI({
    verticalLayout(
      flowLayout(
        radioButtons(
          "measurementRadarChart"
          , strong("Measurement:")
          , list('Percentage'='pct', 'Count'='count')
          , inline = F)
        , numericInput(
          "limitRadarChart"
          , label = strong("Limit of criteria per group:")
          , value=NA)
        , htmlOutput("orderByAssessmentResultRadarChartSelector")
      )
      , htmlOutput("titleAssessmentResultRadarChart")
      , plotlyOutput("assessmentResultRadarChart", height="1024px")
    )
  })
  
  output$orderByAssessmentResultRadarChartSelector <- renderUI({
    if (length(input$assessmentValue) > 0) {
      selectInput(
        inputId = "orderByRadarChart"
        , multiple = T
        , label = strong("Order by:")
        , choices = input$assessmentValue)
    }
  })
  
  output$titleAssessmentResultRadarChart <- renderUI({
    h3(paste0(
      ifelse(app_params()$options$measurement == 'pct','Percentage','Quantity')
      ,' of ','Evidences that support the assessment on each criterion'))
  })
  
  output$assessmentResultRadarChart <- renderPlotly({
    print(app_params())
    withProgress(message = 'Making radar chart...', {
      get_plotly(app_params()$dtype, 'scatterpolar', app_params()$filters, app_params()$options)
    })
  })
  
  # rendering StackedBarChart for evidence-based assessment result
  output$assessmentResultHTMLStackedBarChart <- renderUI({
    verticalLayout(
      flowLayout(
        radioButtons(
          "measurementStackedBarChart"
          , strong("Measurement:")
          , list('Percentage'='pct', 'Count'='count')
          , selected = 'none'
          , inline = F)
        , htmlOutput("formulaHTMLStackedBarChart")
      )
      , htmlOutput("titleAssessmentStackedBarChart")
      , plotlyOutput("assessmentResultStackedBarChart")
    )
  })
  
  output$formulaHTMLStackedBarChart <- renderUI({
    lchoices <- list('Mean of evidences'='mean')
    if (input$measurementStackedBarChart != 'pct') {
      lchoices <- list('Mean of evidences'='mean', 'Sum of evidences'='sum')
    }
    radioButtons(
      "formulaStackedBarChart"
      , strong("Assessment value calculated by:")
      , choices = lchoices
      , inline = F)
  })
  
  output$titleAssessmentStackedBarChart <- renderUI({
    h3(paste0(
      ifelse(app_params()$options$measurement == 'pct','Percentage','Quantity')
      , ' ', ifelse(app_params()$options$formula == 'mean','mean','')
      ,' of evidences that support the assessment on each group'))
  })
  
  output$assessmentResultStackedBarChart <- renderPlotly({
    withProgress(message = 'Making stacked bar chart...', {
      get_plotly(app_params()$dtype, 'stackedbar', app_params()$filters, app_params()$options)
    })
  })
  
  # rendering PieChart for evidence-based assessment result
  output$assessmentResultHTMLPieChart <- renderUI({
    verticalLayout(
      radioButtons(
        "formulaPieChart"
        , strong("Assessment value calculated by:")
        , choices = list('Mean of evidences'='mean', 'Sum of evidences'='sum')
        , inline = T)
      , h3('Evidence-based assessment result on each group')
      , plotlyOutput("assessmentResultPieChart", height="1024px")
    )
  })
  
  output$assessmentResultPieChart <- renderPlotly({
    withProgress(message = 'Making pie chart...', {
      get_plotly(app_params()$dtype, 'pie', app_params()$filters, app_params()$options)
    })
  })
  
  # rendering Table for evidence-based assessment result
  output$assessmentResultTable <- renderDataTable({
    withProgress(message = 'Making table...', {
      df <- get_data.as.df(app_params()$dtype, app_params()$filters, app_params()$options)
      validate(need((!is.null(df) & nrow(df) > 0), 'Error. There is not data. Use other filters'))
      df
    })
  })
  
  # setting other UI events: download, 
  output$downloadCSVButton <- downloadHandler(
    filename = paste0(input$mainNavigation,"-data.csv")
    , content = function(file) {
      df <- get_data.as.df(app_params()$dtype, app_params()$filters, app_params()$options)
      validate(need((!is.null(df) & nrow(df) > 0), 'Error. There is not data. Use other filters'))
      write.csv(df, file, row.names = F)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
