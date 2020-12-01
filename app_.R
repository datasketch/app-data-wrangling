library(shinypanels)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(parmesan)
library(hotr)
library(tidyverse)
library(homodatum)



source("global_.R")

ui <- panelsPage(#useShi18ny(),
  showDebug(),
  panel(title = ui_("options"),
        width = 300,
        color = "chardonnay",
        body = uiOutput("controls")),
  panel(title = ui_("viz"),
        color = "chardonnay",
        can_collapse = FALSE,
        body = div(#langSelectorInput("lang", position = "fixed"),
          uiOutput("result_wrng"))))



server <- function(input, output, session) {
  
  # i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  # i18n <- list(defaultLang = "en", availableLangs = "en")
  # lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  # observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  # controls
  output$controls <- renderUI({
    div(div(class = "style_section", "Select columns"),
        select_moduleUI("sel0", in_table))
  })
  
  # initial table
  in_table <- read_csv("data/sampleData/pr.csv")
  observe({
    # select_moduleServer("sel0", in_table)
    
    # print(select_moduleServer("sel0", in_table))
  })
  
  # table after changes
  out_table <- reactive({
    t0 <- in_table
    t0 <- select_moduleServer("sel0", t0)
    t0
  })
  
  output$result_wrng <- renderUI({
    suppressWarnings(hotr("h_out-table", data = out_table(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
  })
  
}


shinyApp(ui, server)