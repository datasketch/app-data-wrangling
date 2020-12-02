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
    tagList(div(class = "style_section", "Select columns"),
            select_moduleUI("wr0", in_table),
            div(class = "style_section", "Filter by columns"),
            filter_moduleUI("wr1", in_table))
  })
  
  # initial table
  in_table <- read_csv("data/sampleData/pr.csv")
  in_table$Country[2] <- ""
  observe({
    # select_moduleServer("sel0", in_table)
    
    # print(select_moduleServer("sel0", in_table))
  })
  
  
  filter_moduleServer <- function (id, data, options_0 = NULL, options_1 = NULL) {
    
    moduleServer(id, function (input, output, session) {
      d0 <- data
      ns <- NS(id)
      opt_0 <- c("fil_00", "fil_01")
      opt_1 <- c("fil_10", "fil_11")
      names(opt_0) <- options_0 %||% c("in", "not in")
      names(opt_1) <- options_1 %||% c("Keep NA's", "Keep empty")
      
      observeEvent(input$filter_0,  {
        nm <- last(input$filter_0)
        vc <- data[[nm]]
        hd <- guess_hdType(vc)
        if (hd %in% c("Num", "Yea", "Dat")) {
          if (hd == "Dat") {
            vc <- as.Date(vc, "%Y-%m-%d")
          }
          in0 <- sliderInput(ns(paste0(nm, "-", "filter_1")),
                             "",
                             min = min(vc, na.rm = TRUE),
                             max = max(vc, na.rm = TRUE),
                             value = c(min(vc, na.rm = TRUE), max(vc, na.rm = TRUE)),
                             timeFormat = "%Y-%m-%d")
        } else {
          in0 <- selectizeInput(ns(paste0(nm, "-", "filter_2")),
                                "",
                                unique(vc), 
                                # unique(c("", na.omit(vc))), 
                                multiple = TRUE,
                                options = list("plugins" = list("remove_button")))
        }
        if (any(is.na(vc)) & any(!nzchar(vc))) {
          opt_1 <- opt_1
        } else if (any(is.na(vc))) {
          opt_1 <- opt_1[1]
        } else if (any(!nzchar(vc))) {
          opt_1 <- opt_1[2] 
        } else {
          opt_1 <- NULL
        }
        in1 <- ""
        if (!is.null(opt_1)) in1 <- checkboxGroupInput(ns(paste0(nm, "-", "filter_3")), "", choices = opt_1, selected = opt_1)
        dv <- tagList(div(style = "display: flex; justify-content: space-between;",
                          div(style = "font-weight: 600; padding: 0 39px 0 0;", paste0(nm, ":")),
                          # div(style = "font-size: 17px; font-weight: 400; padding: 0 17px 14px 0;", paste0(col, ":")),
                          # div(style = "padding: 0 17px 14px 0;", class = "control-label", paste0(col, ":")),
                          radioButtons(ns(paste0(nm, "-", "filter_4")), "", choices = opt_0)),
                      in0,
                      in1)
        insertUI("#filter_", "afterEnd", dv) 
      })
      d0
    })
    
  }
  
  # table after changes
  out_table <- reactive({
    t0 <- in_table
    t0 <- select_moduleServer("wr0", t0)
    t0 <- filter_moduleServer("wr1", t0)
    t0
  })
  
  output$result_wrng <- renderUI({
    suppressWarnings(hotr("h_out-table", data = out_table(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
  })
  
}


shinyApp(ui, server)