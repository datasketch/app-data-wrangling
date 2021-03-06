# filter
filter_moduleUI <- function(id, data, col, text, logical_names = NULL, na_empty_names = NULL) {
  
  ns <- NS(id)
  vctr <- data[[col]]
  hd <- guess_hdType(vctr)
  if (hd %in% c("Num", "Yea", "Dat")) {
    if (hd == "Dat") {
      vctr <- as.Date(vctr, "%Y-%m-%d")
    }
    input <- sliderInput(ns("num_filter"),
                         "",
                         min = min(vctr, na.rm = TRUE),
                         max = max(vctr, na.rm = TRUE),
                         value = c(min(vctr, na.rm = TRUE), max(vctr, na.rm = TRUE)),
                         timeFormat = "%Y-%m-%d")
  } else {
    input <- selectizeInput(ns("cat_filter"), "", unique(c("", na.omit(vctr))), multiple = TRUE, options = list("plugins" = list("remove_button")))
  }
  
  choices_kp <- c()
  checkbox_na_empty <- "" 
  if (any(is.na(vctr))) choices_kp <- c("keep_na")
  if (any(!nzchar(vctr))) choices_kp <- c(choices_kp, "keep_empty")
  if (!is.null(na_empty_names) & (length(na_empty_names) >= length(choices_kp)) & (length(choices_kp) > 0)) names(choices_kp) <- na_empty_names[seq_along(choices_kp)]
  if (length(choices_kp) > 0) checkbox_na_empty <- checkboxGroupInput(ns("keep_na_empty"), "", choices = choices_kp, selected = choices_kp)
  
  choices_lg <- c("in", "not_in")
  if (!is.null(logical_names) & length(logical_names) == length(choices_lg)) names(choices_lg) <- logical_names
  div(id = id, 
      div(style = "display: flex; justify-content: space-between;",
          div(style = "font-weight: 600; padding: 0 39px 0 0;", paste0(col, ":")),
          # div(style = "font-size: 17px; font-weight: 400; padding: 0 17px 14px 0;", paste0(col, ":")),
          # div(style = "padding: 0 17px 14px 0;", class = "control-label", paste0(col, ":")),
          radioButtons(ns("logical_relation"), "", choices = choices_lg)),
      input,
      checkbox_na_empty)
  
}


filter_moduleServer <- function(id, data, col) {
  
  moduleServer(id, function(input, output, session) {
    req(input$logical_relation)
    dt <- data
    hd <- guess_hdType(data[[col]])
    
    if (hd %in% c("Num", "Yea", "Dat")) {
      fl_expr <- "input$num_filter[1] <= .data[[col]] & input$num_filter[2] >= .data[[col]]"
      if (input$logical_relation == "not_in") fl_expr <- paste0("!(", fl_expr, ")")
      # if ("keep_na" %in% input$keep_na_empty) fl_expr_nm[2] <- "is.na(.data[[col]])"
      # if ("keep_empty" %in% input$keep_na_empty) fl_expr_nm[3] <- "!nzchar(.data[[col]])"
    } else {
      if (!is.null(input$cat_filter)) {
        fl_expr <- ".data[[col]] %in% input$cat_filter"
        if (input$logical_relation == "not_in") fl_expr <- paste0("!", fl_expr)
      } else {
        fl_expr <- "TRUE"
      }
    }
    if ("keep_na" %in% input$keep_na_empty) fl_expr[2] <- "is.na(.data[[col]])"
    if ("keep_empty" %in% input$keep_na_empty) fl_expr[3] <- "!nzchar(.data[[col]])"
    dt %>%
      dplyr::filter(eval(parse(text = paste(fl_expr, collapse = " | "))))
  })
  
}


arrange_moduleUI <- function(id, data, col, arrange_names = NULL) {
  
  ns <- NS(id)
  
  choices_arr <- c("descendent", "ascendent")
  if (!is.null(arrange_names) & length(arrange_names) == length(choices_arr)) names(choices_arr) <- arrange_names
  div(id = id, 
      div(style = "display: flex; justify-content: space-between;",
          div(style = "font-weight: 600; padding: 0 29px 0 0;", paste0(col, ":")),
          radioButtons(ns("arrange_order"), "", choices = choices_arr)))
  
}


arrange_moduleServer <- function(id, data, col) {
  
  moduleServer(id, function(input, output, session) {
    req(input$arrange_order)
    dt <- data
    
    if (input$arrange_order == "descendent") {
      paste0("desc(.data[['", col, "']])") 
    } else {
      paste0(".data[['", col, "']]")
    }  
  })
  
}



# pivot_longer
pivot_longer_moduleUI <- function(id, data) {
  
  ns <- NS(id)
  vctr <- names(data)
  div(id = id,
      style = "margin-top: 37px;",
      selectizeInput(ns("columns_pv_lng"), "Columns with same measure:", c("", vctr), multiple = TRUE, options = list("plugins" = list("remove_button"))),
      textInput(ns("names_col"), "Column of names:", placeholder = "Name new column of names"),
      textInput(ns("values_col"), "Column of values:", placeholder = "Name new column of values"))
  
}


pivot_longer_moduleServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    dt <- data
    nm <- as.numeric(regmatches(id, regexec("[0-9]+", id)))
    # lt <- list(nm = nm, dt = dt)
    print(nm)
    print(input$columns_pv_lng)
    if (all(map_lgl(c(input$columns_pv_lng, input$names_col, input$values_col), ~isTruthy(.x)))) {
      dt <- tryCatch(pivot_longer(data,
                                  cols = input$columns_pv_lng, 
                                  names_to = input$names_col,
                                  values_to = input$values_col),
                     error = function(e) "The chosen columns have different type of data, thus, cannot be combined into one single column.")
      # FALTA LOGRAR ESTO (apenas se complete un módulo de pivot longer que salga otro y si se ¿descompleta el primero?
      # ¿se quita el siguiente? ¿así esté completo?)
      # insertUI(paste0("#", id),
      #          "afterEnd",
      #          ui = pivot_longer_moduleUI(paste0("pivot_longer_", nm + 1), dt),
      #          multiple = FALSE)
      # lt$dt <- dt
      # lt$nm <- nm + 1
    } else {
      # lt$dt <- "NULL"
      # lt$dt <- NULL
      # print("FFFFFF")
      # removeUI(paste0("#pivot_longer_", nm + 1))
    }
    dt
    # lt
  })
  
}





# pivot_wider
pivot_wider_moduleUI <- function(id, data) {
  
  ns <- NS(id)
  vctr <- names(data)
  div(id = id,
      style = "margin-top: 37px;",
      selectInput(ns("names_from"), "Names from column:", c("", vctr)),
      selectInput(ns("values_from"), "Values from column:", c("", vctr)))
  
}


pivot_wider_moduleServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    dt <- data
    nm <- as.numeric(regmatches(id, regexec("[0-9]+", id)))
    if (all(map_lgl(c(input$names_from, input$values_from), ~isTruthy(.x)))) {
      dt <- tryCatch(pivot_wider(dt,
                                 names_from = input$names_from,
                                 values_from = input$values_from),
                                 #values_fn = sum),
                     error = function(e) "T")
      # insertUI(paste0("#", id),
      #          "afterEnd",
      #          ui = pivot_longer_moduleUI(paste0("pivot_longer_", nm + 1), dt),
      #          multiple = FALSE)
      # lt$dt <- dt
      # lt$nm <- nm + 1
    } else {
      # lt$dt <- "NULL"
      # lt$dt <- NULL
      # removeUI(paste0("#pivot_longer_", nm + 1))
    }
    dt
    # lt
  })
  
}
