# Load packages
#wants <- c('shiny','shinythemes','config','plotly','rjson','qdap','pivottabler','brazilmaps','PTXQC', 'shinyjs') 
#has <- wants %in% rownames(installed.packages())
#if (any(!has)) install.packages(wants[!has])

library(readr)
library(shiny)
library(shinythemes)
library(plotly)

library(pivottabler)

source('common.R')

period_choices <- get_choices('period')
assessment_value_choices <- get_choices("assessmentValue")

ui <- fluidPage(
  shinyjs::useShinyjs()
  , theme = shinytheme("lumen")
  , sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "period", multiple = T
        , label = strong("Período:")
        , choices = period_choices
        , selected = as.vector(unlist(period_choices)))
      , selectInput(
        inputId = "technologyType", multiple = T
        , label = strong("Filtros das evidências por tipo de tecnologia:")
        , choices = get_choices("technologyType"))
      , htmlOutput("extraEvidenceSourceSidebar")
      , selectInput(
        inputId = "dimension", multiple = T
        , label = strong("Filtros das avaliações por dimensão:")
        , choices = get_choices("dimension"))
      , htmlOutput("extraCriteriaSourceSidebar")
      , h4('Informação da imagem')
      , numericInput("fontsize", "Tamanho do texto (em pt):", 16, min = 8)
      , numericInput("width", "Largura (em px, usar 0 para automático):", 0, min = 0)
      , numericInput("height", "Altura (em px, usar 0 para automático):", 800, min = 0)
      , fluidRow(downloadButton("downloadCSVButton", "Download .csv"))
      , shinyjs::hidden(textInput("tabSelector", NULL, value = ""))
      , shinyjs::hidden(textInput("barSelector", NULL, value = ""))
      , shinyjs::hidden(textInput("measurementSelector", NULL, value = ""))
      , shinyjs::hidden(textInput("formulaSelector", NULL, value = ""))
      , shinyjs::hidden(textInput("showAsPctSelector", NULL, value = ""))
      , shinyjs::hidden(textInput("limitCriteriaSelector", NULL, value = ""))
      , shinyjs::hidden(selectInput("orderBySelector", NULL, multiple = T, choices = assessment_value_choices))
    )
    # Output: Description, lineplot, and reference
    , mainPanel(
      navbarPage(
        "Analytics"
        , id = "mainNavigation"
        , header = htmlOutput('assessmentOptionSelector')
        , tabPanel(
          "Acerca das Evidências"
          , value = "about-evidences"
          , htmlOutput('aboutEvidencesContent')
        )
        , tabPanel(
          "Acerca das Avaliações"
          , value = "about-assessments"
          , htmlOutput('aboutAssessmentsContent')
        )
        , navbarMenu(
          "Resultado da Avaliação"
          , tabPanel(
            "Distribuição/comparação do resultado"
            , value = "result-distribution"
            , htmlOutput('resultDistributionContent')
          )
          , tabPanel(
            "Resultado em referência aos critérios"
            , value = "result-criteria"
            , htmlOutput('resultCriteriaContent')
          )
        )
      )
    )
    , position = "left"
  )
)

## Define server for app
server <- function(input, output, session) {
  
  observeEvent(input$tabSelector, {
    if (input$mainNavigation == 'result-distribution' || input$mainNavigation == 'result-criteria') {
      updateTabsetPanel(session, "resultTabset", selected = input$tabSelector)
    }
  })
  
  observeEvent(input$barSelector, {
    if (input$mainNavigation == 'result-distribution') {
      if (input$tabSelector == 'bar') { updateRadioButtons(session, 'typeResultDistributionBarChart', selected = input$barSelector) }
    }
  })
  
  observeEvent(input$measurementSelector, {
    if (input$mainNavigation == 'result-distribution') {
      if (input$tabSelector == 'bar') { updateRadioButtons(session, 'measurementResultDistributionBarChart', selected = input$measurementSelector) }
    }
  })
  
  observeEvent(input$formulaSelector, {
    if (input$mainNavigation == 'result-distribution') {
      if (input$tabSelector == 'bar') { updateRadioButtons(session, 'formulaResultDistributionBarChart', selected = input$formulaSelector) }
    }
  })
  
  observeEvent(input$showAsPctSelector, {
    if (input$mainNavigation == 'result-distribution') {
      if (input$tabSelector == 'bar') { updateCheckboxInput(session, 'is_show_as_percentage', value = TRUE) }    
    }
  })
  
  observeEvent(input$limitCriteriaSelector, {
    if (input$mainNavigation == 'result-criteria') {
      if (input$tabSelector == 'radar') { updateNumericInput(session, 'limitRadarChart', value = input$limitCriteriaSelector) }    
    }
  })
  
  
  observeEvent(input$orderBySelector, {
    if (input$mainNavigation == 'result-criteria') {
      if (input$tabSelector == 'radar') { updateSelectInput(session, 'orderByRadarChart', selected = input$orderBySelector) }  
    }
  })
  
  
  ## Display header in the menu selection
  
  output$assessmentOptionSelector <- renderUI({
    verticalLayout(
      checkboxGroupInput(
        "assessmentValue"
        , strong("Resultado da avaliação no análise:")
        , assessment_value_choices
        , inline = T, selected = as.vector(unlist(assessment_value_choices)))
      , radioButtons(
        "assessmentFrom"
        , strong("Avaliações de:")
        , list("Todos"="all", "Consolidador"="main", "Participantes excluindo o consolidador"="wo-main")
        , inline = T, selected = 'all')
    )
  })
  
  ## Display contents according to the menu selection
  
  output$aboutEvidencesContent <- renderUI({
    width <- ifelse(input$width == 0, 'auto', paste0(input$width,'px')) 
    height <- ifelse(input$height == 0, 'auto', paste0(input$height,'px'))
    verticalLayout(
      h3('Acerca das evidências')
      , tabsetPanel(
        type = "tabs", id = 'aboutEvidencesTabset'
        , tabPanel("Pie Chart", value = 'pie', plotlyOutput('aboutEvidencesPieChart', width = width, height = height))
        , tabPanel("Table", value = 'table', dataTableOutput('aboutEvidencesTable'))
      )
    )
  })
  
  output$aboutAssessmentsContent <- renderUI({
    width <- ifelse(input$width == 0, 'auto', paste0(input$width,'px')) 
    height <- ifelse(input$height == 0, 'auto', paste0(input$height,'px'))
    verticalLayout(
      h3('Acerca das avaliações')
      , tabsetPanel(
        type = "tabs", id = 'aboutAssessmentsTabset'
        , tabPanel("Pie Chart", value = 'pie', plotlyOutput('aboutAssessmentsPieChart', width = width, height = height))
        , tabPanel("Table", value = 'table', dataTableOutput('aboutAssessmentsTable'))
      )
    )
  })
  
  output$resultDistributionContent <- renderUI({
    width <- ifelse(input$width == 0, 'auto', paste0(input$width,'px')) 
    height <- ifelse(input$height == 0, 'auto', paste0(input$height,'px'))
    verticalLayout(
      h3('Distribuição e comparação do resultado da avaliação')
      , tabsetPanel(
        type = "tabs", id = 'resultTabset'
        , tabPanel(
          "Pie Chart", value = 'pie'
          , verticalLayout(
            radioButtons(
              "formulaResultDistributionPieChart", inline = T
              , strong("Resultado calculado como:")
              , choices = list('Número total de evidências'='sum', 'Número médio de evidências'='mean'))
            , plotlyOutput('resultDistributionPieChart', width = width, height = height)
            )
          )
        , tabPanel(
          "Bar Chart", value = 'bar'
          , verticalLayout(
            flowLayout(
              radioButtons(
                "typeResultDistributionBarChart", inline = F
                , strong("Tipo de barras:"), selected = 'stack'
                , choices = list('Empilhado'='stack', 'Agrupado'='group'))
              , radioButtons(
                "measurementResultDistributionBarChart", inline = F
                , strong("Medição calculada a partir de:"), selected = 'count'
                , list('Contagem'='count', 'Percentagem'='pct'))
              , radioButtons(
                "formulaResultDistributionBarChart", inline = F
                , strong("Resultado calculado nas barras como:")
                , choices = list('Número total da medição'='sum', 'Número médio da medição'='mean'))
              , checkboxInput('is_show_as_percentage', 'Apresentar como percentagem?'))
            , plotlyOutput('resultDistributionBarChart', width = width, height = height)
            )
          )
        , tabPanel("Table", value = 'table', dataTableOutput('resultDistributionTable'))
      )
    )
  })
  
  output$resultCriteriaContent <- renderUI({
    width <- ifelse(input$width == 0, 'auto', paste0(input$width,'px')) 
    height <- ifelse(input$height == 0, 'auto', paste0(input$height,'px'))
    verticalLayout(
      h3('Resultado em referência aos critérios de avaliação')
      , tabsetPanel(
        type = "tabs", id = 'resultTabset'
        , tabPanel(
          "Heatmap Chart", value = 'heatmap'
          , plotlyOutput('resultCriteriaHeatmapChart', width = width, height = height)
        )
        , tabPanel(
          "Radar Chart", value = 'radar'
          , verticalLayout(
            flowLayout(
              radioButtons(
                "measurementResultCriteriaRadarChart"
                , strong("Medição calculada a partir de:")
                , list('Contagem'='count', 'Percentagem'='pct'), inline = F)
              , numericInput(
                "limitRadarChart"
                , label = strong("Límite de criterios a mostrar:"), value=0)
              , selectInput(
                inputId = "orderByRadarChart", multiple = T
                , label = strong("Ordenar por:"), choices = input$assessmentValue)
            )
            , plotlyOutput("resultCriteriaRadarChart", width = width, height = height)
          )
        )
        , tabPanel("Table", value = 'table', dataTableOutput('resultCriteriaTable'))
      )
    )
  })
  
  ## Display extra UIs in sidebar
  output$extraCriteriaSourceSidebar <- renderUI({
    if (input$mainNavigation == 'result-criteria') {
      if (length(input$dimension) > 0) {
        verticalLayout(
          checkboxInput('is_criteria', 'Seleção por critérios?')
          , htmlOutput("criteriaSelector")
        )
      }
    }
  })
  
  output$extraEvidenceSourceSidebar <- renderUI({
    if (length(input$technologyType) > 0) {
      verticalLayout(
        checkboxInput('is_technology', 'Filtragem por tecnologia?')
        , htmlOutput("technologySelector")
      )
    }
  })
  
  output$technologySelector <- renderUI({
    if (input$is_technology && length(input$technologyType) > 0) {
      technology_choices <- get_choices("technology", list(technologyType=input$technologyType))
      names(technology_choices) <- paste0('Tec-',names(technology_choices))
      selectInput(
        inputId = "technology", multiple = T
        , label = strong("Tecnologia:")
        , choices = technology_choices)
    }
  })
  
  output$criteriaSelector <- renderUI({
    if (input$is_criteria && length(input$dimension) > 0) {
      selectInput(
        choices = get_choices("criteria", list(dimension=input$dimension))
        , inputId = "criteria", label = strong("Criteria:"), multiple = T)
    }
  })
  
  ## Display graphical analytics
  
  output$aboutEvidencesPieChart <- renderPlotly({
    withProgress(message = 'Making evidence information in pie chart...', {
      get_plotly(app_params()$dtype, 'pie', app_params()$filters, app_params()$options)
    })
  })
  
  output$aboutAssessmentsPieChart <- renderPlotly({
    withProgress(message = 'Making assessement information in pie chart...', {
      get_plotly(app_params()$dtype, 'pie', app_params()$filters, app_params()$options)
    })
  })
  
  output$resultDistributionBarChart <- renderPlotly({
    withProgress(message = 'Making assessment result distribution in bar chart ...', {
      get_plotly(app_params()$dtype, 'bar', app_params()$filters, app_params()$options)
    })
  })
  
  output$resultDistributionPieChart <- renderPlotly({
    withProgress(message = 'Making assessment result distribution in pie chart ...', {
      get_plotly(app_params()$dtype, 'pie', app_params()$filters, app_params()$options)
    })
  })
  
  output$resultCriteriaHeatmapChart <- renderPlotly({
    print(app_params())
    withProgress(message = 'Making assessment result criteria in heatmap chart ...', {
      get_plotly(app_params()$dtype, 'heatmap', app_params()$filters, app_params()$options)
    })
  })
  
  output$resultCriteriaRadarChart <- renderPlotly({
    withProgress(message = 'Making assessment result criteria in radar chart...', {
      get_plotly(app_params()$dtype, 'radar', app_params()$filters, app_params()$options)
    })
  })
  
  ## Display tables from analytics
  
  output$aboutEvidencesTable <- renderDataTable({
    withProgress(message = 'Making table to about evidences ...', {
      df <- get_data.as.df(app_params()$dtype, app_params()$filters, app_params()$options)
      validate(need((!is.null(df) & nrow(df) > 0), 'Error. There is not data. Use other filters'))
      df
    })
  })
  
  output$aboutAssessmentsTable <- renderDataTable({
    withProgress(message = 'Making table to about assessments ...', {
      df <- get_data.as.df(app_params()$dtype, app_params()$filters, app_params()$options)
      validate(need((!is.null(df) & nrow(df) > 0), 'Error. There is not data. Use other filters'))
      df
    })
  })
  
  output$resultDistributionTable <- renderDataTable({
    withProgress(message = 'Making table to about result distribution ...', {
      df <- get_data.as.df(app_params()$dtype, app_params()$filters, app_params()$options)
      validate(need((!is.null(df) & nrow(df) > 0), 'Error. There is not data. Use other filters'))
      df
    })
  })
  
  output$resultCriteriaTable <- renderDataTable({
    withProgress(message = 'Making table to about result in function of criteria ...', {
      df <- get_data.as.df(app_params()$dtype, app_params()$filters, app_params()$options)
      validate(need((!is.null(df) & nrow(df) > 0), 'Error. There is not data. Use other filters'))
      df
    })
  })
  
  ## Setting other UI events, such as download, read params from query and parameters for app
  
  output$downloadCSVButton <- downloadHandler(
    filename = paste0(input$mainNavigation,"-data.csv")
    , content = function(file) {
      df <- get_data.as.df(app_params()$dtype, app_params()$filters, app_params()$options)
      validate(need((!is.null(df) & nrow(df) > 0), 'Error. There is not data. Use other filters'))
      write.csv(df, file, row.names = F)
    }
  )
  
  observe({
    
    query <- parseQueryString(session$clientData$url_search, nested = T)
    
    if (!is.null(query[['main']]) && length(query[['main']]) > 0) {
      updateNavbarPage(session, 'mainNavigation', selected = query[['main']])
    }
    if (!is.null(query[['period']]) && length(query[['period']]) > 0) {
      updateSelectInput(session, 'period', selected = query[['period']])
    }
    if (!is.null(query[['technologyType']]) && length(query[['technologyType']]) > 0) {
      updateSelectInput(session, 'technologyType', selected = query[['technologyType']])
    }
    if (!is.null(query[['dimension']]) && length(query[['dimension']]) > 0) {
      updateSelectInput(session, 'dimension', selected = query[['dimension']])
    }
    
    if (!is.null(query[['width']]) && length(query[['width']]) > 0) {
      updateNumericInput(session, 'width', value = query[['width']])
    }
    if (!is.null(query[['height']]) && length(query[['height']]) > 0) {
      updateNumericInput(session, 'height', value = query[['height']])
    }
    if (!is.null(query[['fontsize']]) && length(query[['fontsize']]) > 0) {
      updateNumericInput(session, "fontsize", value = query[['fontsize']])
    }
    
    #
    if (!is.null(query[['tab']]) && length(query[['tab']]) > 0) {
      updateTextAreaInput(session, 'tabSelector', value = query[['tab']])
    }
    if (!is.null(query[['bar']]) && length(query[['bar']]) > 0) {
      updateTextAreaInput(session, 'barSelector', value = query[['bar']])
    }
    if (!is.null(query[['formula']]) && length(query[['formula']]) > 0) {
      updateTextAreaInput(session, 'formulaSelector', value = query[['formula']])
    }
    if (!is.null(query[['measurement']]) && length(query[['measurement']]) > 0) {
      updateTextAreaInput(session, 'measurementSelector', value = query[['measurement']])
    }
    if (!is.null(query[['showAsPct']]) && length(query[['showAsPct']]) > 0) {
      updateTextAreaInput(session, 'showAsPctSelector', value = 'TRUE')
    }
    if (!is.null(query[['assessmentValue']]) && length(query[['assessmentValue']]) > 0) {
      updateCheckboxGroupInput(session, 'assessmentValue', selected = query[['assessmentValue']])
    }
    
    #
    if (!is.null(query[['limitCriteria']]) && length(query[['limitCriteria']]) > 0) {
      updateTextAreaInput(session, 'limitCriteriaSelector', value = query[['limitCriteria']])
    }
    if (!is.null(query[['orderBy']]) && length(query[['orderBy']]) > 0) {
      updateSelectInput(session, 'orderBySelector', selected = query[['orderBy']])
    }
  })
  
  app_params <- reactive({ #app_params <- eventReactive(input$updateDataGraphButton, {
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
    
    options <- list(assessmentValue = input$assessmentValue
                    , assessmentFrom = input$assessmentFrom, fontsize = input$fontsize)
    if (input$mainNavigation == 'result-criteria') {
      print(input$resultTabset)
      options[['measurement']] <- 'count'
      if (input$resultTabset == 'radar') {
        options[['measurement']] <- input$measurementResultCriteriaRadarChart
        options[['limit']] <- input$limitRadarChart
        options[['orderBy']] <- input$orderByRadarChart
      }
    } else if (input$mainNavigation == 'result-distribution') {
      if (input$resultTabset == 'pie') {
        options[['formula']] <- input$formulaResultDistributionPieChart
      } else if (input$resultTabset == 'bar') {
        options[['typeBar']] <- input$typeResultDistributionBarChart
        options[['measurement']] <- input$measurementResultDistributionBarChart
        options[['formula']] <- input$formulaResultDistributionBarChart
        options[['showAsPct']] <- input$is_show_as_percentage
      }
    }
    
    list(dtype = input$mainNavigation, filters = filters, options = options)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
