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
                                  uiOutput("result_wrng"),
                                  withLoader(uiOutput("result"), type = "image", loader = "loading_gris.gif"))))
                                  # withLoader(dataTableOutput("result"), type = "image", loader = "loading_gris.gif"))))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  # i18n <- list(defaultLang = "en", availableLangs = "en")
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
  
  # observe({
  dt <- reactive({
    req(input$hotr_input)
    # rv$dt <- hotr_table(input$hotr_input)
    # rv$dt_after <- hotr_table(input$hotr_input)
    hotr_table(input$hotr_input)
  })
  
  
  rv <- reactiveValues(dt = NULL, dt_after = NULL,
                       result_wrng = NULL,
                       filter_sel_col = NULL, arrange_sel_col = NULL)
  
  output$controls <- renderUI({
    div(
      div(id = "select_select_columns_div",
          div(class = "style_section", "Select columns"),
          radioButtons("logical_columns_select", "Columns", choices = c("in" = "in", "not in" = "not_in")),
          selectizeInput("select_select_columns", "", choices = c("", names(dt())), multiple = TRUE, options = list("plugins" = list("remove_button"))),
          radioButtons("logical_contains_select", "Columns names that:", choices = c("contain" = "in", "do not contain" = "not_in")),
          textInput("contains_select_columns", "", placeholder = "Write text")),
      div(id = "filter_select_columns_div",
          div(class = "style_section", "Filter"),
          selectizeInput("filter_select_columns", "Columns", choices = c("", names(dt())), multiple = TRUE, options = list("plugins" = list("remove_button")))),
      div(id = "arrange_select_columns_div",
          div(class = "style_section", "Arrange"),
          selectizeInput("arrange_select_columns", "Columns", choices = c("", names(dt())), multiple = TRUE, options = list("plugins" = list("remove_button")))),
      div(id = "pivot_longer_div",
          div(class = "style_section", "Wide to long"),
          infomessage('Datasets with columns that measure the same thing and contain the same type of data can have
                      all this information stored in only two columns: one with the names of the columns
                      (which indicates what the data is measuring) and another one with the actual values of what is being measured', type = "info"),
          
          pivot_longer_moduleUI("pivot_longer_0", dt())),
      div(id = "pivot_wider_div",
          div(class = "style_section", "Long to wide"),
          infomessage("Wide to long inverse process", type = "info"),
          pivot_wider_moduleUI("pivot_wider_0", dt()))
    )
  })
  
  # data with only selected columns
  dt_sel_columns <- reactive({
    req(input$logical_columns_select, input$logical_contains_select)
    sel <- sel_n <- NULL
    if (input$logical_columns_select == "in") sel <- input$select_select_columns
    if (input$logical_columns_select == "not_in") sel_n <- input$select_select_columns
    if (input$logical_contains_select == "in") sel <- c(sel, input$contains_select_columns)
    if (input$logical_contains_select == "not_in") sel_n <- c(sel_n, input$contains_select_columns)
    sel <- sel[nzchar(sel)]
    sel_n <- sel_n[nzchar(sel_n)]
    if (sum(nchar(sel)) > 0 & sum(nchar(sel_n)) > 0) dt <- dt %>% dplyr::select(contains(sel), !contains(sel_n))
    if (sum(nchar(sel)) == 0 & sum(nchar(sel_n)) > 0) dt <- dt %>% dplyr::select(!contains(sel_n))
    if (sum(nchar(sel)) > 0 & sum(nchar(sel_n)) == 0) dt <- dt %>% dplyr::select(contains(sel))
  })
  
  # update filter, arrange, pivot long column choices
  
  
  
  # filter module ui
  observeEvent(input$filter_select_columns, {
    sel_col <- input$filter_select_columns
    remove <- setdiff(rv$filter_sel_col, sel_col)
    insert <- setdiff(sel_col, rv$filter_sel_col)
    if (length(remove) > 0) {
      remove_id <- gsub(" ", "_", tolower(remove))
      removeUI(paste0("#", remove_id))
    } else if (length(insert) > 0) {
      # insertUI("#select_columns + .selectize-control", "afterEnd", "filter_moduleUI(insert, dt(), insert, insert)")
      insert_id <- gsub(" ", "_", tolower(insert))
      ch_lg <- c("in", "not in")
      ch_kp <- c("Keep NAs", "Keep empty cells")
      insertUI("#filter_select_columns_div",
               "afterEnd", 
               ui = filter_moduleUI(insert_id, dt(), insert, insert, ch_lg, ch_kp))
               # ui = filter_moduleUI(insert_id, rv$dt, insert, insert, ch_lg, ch_kp))
    }
    rv$filter_sel_col <- input$filter_select_columns
  }, ignoreNULL = FALSE)
  
  
  # arrange module ui
  observeEvent(input$arrange_select_columns, {
    sel_col <- input$arrange_select_columns
    remove <- setdiff(rv$arrange_sel_col, sel_col)
    insert <- setdiff(sel_col, rv$arrange_sel_col)
    if (length(remove) > 0) {
      remove_id <- gsub(" ", "_", tolower(remove))
      removeUI(paste0("#", remove_id))
    } else if (length(insert) > 0) {
      insert_id <- gsub(" ", "_", tolower(insert))
      ch_arr <- c("descendingly", "ascendingly")
      insertUI("#arrange_select_columns_div",
               "afterEnd", 
               ui = arrange_moduleUI(insert_id, dt(), insert, ch_arr))
    }
    rv$arrange_sel_col <- input$arrange_select_columns
  }, ignoreNULL = FALSE)
  
  
  
  dt_after <- reactive({#dt()})
  # observe({
    dt <- dt()
    
    
    # filter
    map(rv$filter_sel_col, function(d) {
      d0 <- gsub(" ", "_", tolower(d))
      dt <<- filter_moduleServer(d0, dt, d)
    })
    
    
    
    # # select
    req(input$logical_columns_select, input$logical_contains_select)
    sel <- sel_n <- NULL
    if (input$logical_columns_select == "in") sel <- input$select_select_columns
    if (input$logical_columns_select == "not_in") sel_n <- input$select_select_columns
    if (input$logical_contains_select == "in") sel <- c(sel, input$contains_select_columns)
    if (input$logical_contains_select == "not_in") sel_n <- c(sel_n, input$contains_select_columns)
    sel <- sel[nzchar(sel)]
    sel_n <- sel_n[nzchar(sel_n)]
    if (sum(nchar(sel)) > 0 & sum(nchar(sel_n)) > 0) dt <- dt %>% dplyr::select(contains(sel), !contains(sel_n))
    if (sum(nchar(sel)) == 0 & sum(nchar(sel_n)) > 0) dt <- dt %>% dplyr::select(!contains(sel_n))
    if (sum(nchar(sel)) > 0 & sum(nchar(sel_n)) == 0) dt <- dt %>% dplyr::select(contains(sel))

    
    
    # arrange
    arr <- map(rv$arrange_sel_col, function(d) {
      d0 <- gsub(" ", "_", tolower(d))
      arrange_moduleServer(d0, dt, d)
    })
    if (length(arr) > 0) {
      # dt <- dt %>% arrange(eval(parse(text = paste0(arr, collapse = ", "))))
      dt <- eval(parse(text = paste0("arrange(dt, ", paste0(arr, collapse = ", "), ")")))
    }
  
    
    assign("d2", dt, envir = globalenv())
    
    # pivot longer
    # dt_ <- dt
    dt <- pivot_longer_moduleServer("pivot_longer_0", dt)
    # dt <- pivot_longer_moduleServer("pivot_longer_1", dt)
    # dt <- pivot_longer_moduleServer("pivot_longer_2", dt)
    # dt <- lt$dt
    # nm <- 0
    # while (dt_ != "NULL") {
    #   print("DDDD")
    #   print(nm)
    #   dt_ <<- pivot_longer_moduleServer(paste0("pivot_longer_", nm), dt)$dt
    #   nm <<- nm + 1
    # }
    
    # dt_ <- pivot_longer_moduleServer("pivot_longer_0", dt)
    # assign("d3", lt, envir = globalenv())
    # if (class(dt_) %in% "character") {
    #   rv$result_wrng <- infomessage(dt_)
    # } else {
    #   dt <- dt_
    #   rv$result_wrng <- ""
    # }
    
    
    
    # pivot wider
    dt <- pivot_wider_moduleServer("pivot_wider_0", dt)
    
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
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("csv", "xlsx", "json"),
                 display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("HTML" = "html", "CSV" = "csv", "JSON" = "json"))
  })
  
  # renderizando warnings resultado (si hay)
  output$result_wrng <- renderUI({
    # infomessage(rv$result_wrng, type = "warning")
    rv$result_wrng
  })
  
  # renderizando resultado
  output$result <- renderUI({
    suppressWarnings(hotr("hotr_input", data = dt_after(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
  })
  
  # url params
  par <- list(user_name = "test", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })
  
  # función con user board connect y set locale
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    } 
    dv <- fringe(x)
    dv$name <- nm
    dv$slug <- nm
    dv$description <- input$`download_data_button-modal_form-description`
    dv$license <- input$`download_data_button-modal_form-license`
    dv$tags <- input$`download_data_button-modal_form-tags`
    dv$category <- input$`download_data_button-modal_form-category`
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }  
  
  
  # descargas
  observe({
    downloadDsServer("download_data_button", element = reactive(dt_after()), formats = c("csv", "xlsx", "json"),
                     errorMessage = i_("gl_error", lang()),
                     modalFunction = pin_, x = reactive(dt_after()),
                     bkt = url_par()$inputs$user_name)
  })
  
}


shinyApp(ui, server)