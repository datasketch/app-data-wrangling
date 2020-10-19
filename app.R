library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(V8)
library(dspins)
library(hotr)
library(tidyverse)
library(shinycustomloader)
library(homodatum)


source("global.R")

ui <- panelsPage(useShi18ny(),
                 showDebug(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       collapsed = TRUE,
                       body = uiOutput("table_input")),
                 panel(title = ui_("dataset"),
                       width = 300,
                       collapsed = TRUE,
                       body = uiOutput("data_preview")),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  withLoader(uiOutput("result"), type = "image", loader = "loading_gris.gif"))))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 label = "",
                 choices = choices,
                 # selected is important for inputs not be re-initialized
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  labels <- reactive({
    sm_f <- i_(c("sample_ch_0", "sample_ch_1"), lang())
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFile = sm_f,
         
         pasteLabel = i_("paste", lang()),
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5, 
         
         uploadLabel = i_("upload_lb", lang()), 
         uploadButtonLabel = i_("upload_bt_lb", lang()),
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()), 
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang()))
  })
  
  inputData <- eventReactive(list(labels(), input$`initial_data-tableInput`), {
    do.call(tableInputServer, c("initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  # parmesan_input <- parmesan_watch(input, parmesan)
  # parmesan_alert(parmesan, env = environment())
  # parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  # output_parmesan("controls", 
  #                 parmesan = parmesan_lang,
  #                 input = input,
  #                 output = output,
  #                 env = environment())
  
  observeEvent(lang(), {
    # ch0 <- as.character(parmesan$pagination$inputs[[5]]$input_params$choices)
    # names(ch0) <- i_(ch0, lang())
    # ch1 <- as.character(parmesan$size$inputs[[3]]$input_params$choices)
    # names(ch1) <- i_(ch1, lang())
    # 
    # updateRadioButtons(session, "page_type", choices = ch0, selected = input$page_type)
    # updateRadioButtons(session, "full_width", choices = ch1, selected = input$full_width)
  })
  
  dt <- reactive({
    req(input$hotr_input)
    hotr_table(input$hotr_input)
  })
  
  cols_nms <- reactive({
    req(dt())
    names(dt())
  })
  
  
  
  rv <- reactiveValues(sel_col = NULL, dt = NULL)
  
  output$controls <- renderUI({
    div(id = "select_columns_div",
        selectizeInput("select_columns", "Filter by column", choices = c("", cols_nms()), multiple = TRUE,  options = list("plugins" = list("remove_button"))))
  })
  
  observeEvent(input$select_columns, {
    sel_col <- input$select_columns
    remove <- setdiff(rv$sel_col, sel_col)
    insert <- setdiff(sel_col, rv$sel_col)
    if (length(remove) > 0) {
      remove_id <- gsub(" ", "_", tolower(remove))
      removeUI(paste0("#", remove_id))
    } else if (length(insert) > 0) {
      # insertUI("#select_columns + .selectize-control", "afterEnd", "filter_moduleUI(insert, dt(), insert, insert)")
      insert_id <- gsub(" ", "_", tolower(insert))
      ch_lg <- c("In", "Not in")
      ch_kp <- c("Keep NA's", "Keep empty cells")
      insertUI("#select_columns_div",
               "afterEnd", 
               ui = filter_moduleUI(insert_id, dt(), insert, insert, ch_lg, ch_kp))
    }
    rv$sel_col <- input$select_columns
  }, ignoreNULL = FALSE)
  
  dt_after <- reactive({
    # req(dt(), rv$sel_col)
    dt <- dt()
    map(rv$sel_col, function(d) {
      d0 <- gsub(" ", "_", tolower(d))
      dt <<- filter_moduleServer(d0, dt, d)
    })
    dt
  })
    
  
  
  
  
  
  
  output$download <- renderUI({
    lb <- i_("download_table", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = "html",
                 display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"))
  })
  
  # renderizando reactable
  output$result <- renderUI({
    suppressWarnings(hotr("hotr_input", data = dt_after(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
  })
  
  # url params
  par <- list(user_name = "test", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })
  
  # prepare element for pining (for htmlwidgets or ggplots)
  # función con user board connect y set locale
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    }
    dv <- dsviz(x,
                name = nm,
                description = input$`download_data_button-modal_form-description`,
                license = input$`download_data_button-modal_form-license`,
                tags = input$`download_data_button-modal_form-tags`,
                category = input$`download_data_button-modal_form-category`)
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }
  
  # descargas
  observe({
    downloadDsServer("download_data_button", element = reactive(dt()), formats = "html",
                     errorMessage = i_("gl_error", lang()),
                     modalFunction = pin_, reactive(dt()),
                     bkt = url_par()$inputs$user_name)
  })
  
}


shinyApp(ui, server)