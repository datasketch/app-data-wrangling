filter_moduleUI <- function(id, data, col, text, logical_names = NULL) {
  
  ns <- NS(id)
  vctr <- data[[col]]
  hd <- guess_hdType(vctr)
  if (hd %in% c("Num", "Yea", "Dat")) {
    if (hd == "Dat") {
      col <- as.Date(col, "%Y-%m-%d")
    }
    input <- sliderInput(ns("num_filter"),
                         "",
                         min = min(vctr, na.rm = TRUE),
                         max = max(vctr, na.rm = TRUE),
                         value = c(min(vctr, na.rm = TRUE), max(vctr, na.rm = TRUE)),
                         timeFormat = "%Y-%m-%d")
  } else {
    input <- selectizeInput(ns("cat_filter"), "", c("", unique(vctr)), multiple = TRUE)
  }
  choices <- c("in", "not_in")
  if (!is.null(logical_names) & length(logical_names) == length(choices)) names(choices) <- logical_names
  div(id = id, 
      div(style = "display: flex; justify-content: space-between;",
          div(style = "font-weight: 600; padding: 0 39px 0 0;", paste0(col, ":")),
          # div(style = "font-size: 17px; font-weight: 400; padding: 0 17px 14px 0;", paste0(col, ":")),
          # div(style = "padding: 0 17px 14px 0;", class = "control-label", paste0(col, ":")),
          radioButtons(ns("logical_relation"), "", choices = choices)),
      input)
  
}


filter_moduleServer <- function(id, data, col) {
  
  moduleServer(id, function(input, output, session) {
    req(input$logical_relation)
    dt <- data
    hd <- guess_hdType(data[[col]])
    if (hd %in% c("Num", "Yea", "Dat")) {
      if (input$logical_relation == "in") {
        dt <- dt %>%
          dplyr::filter(input$num_filter[1] <= .data[[col]],
                        input$num_filter[2] >= .data[[col]])
      } else {
        dt <- dt %>%
          dplyr::filter(input$num_filter[1] > .data[[col]] | input$num_filter[2] < .data[[col]])
      }
    } else {
      if (!is.null(input$cat_filter)) {
        if (input$logical_relation == "in") {
          dt <- dt %>%
            dplyr::filter(.data[[col]] %in% input$cat_filter)
        } else {
          dt <- dt %>%
            dplyr::filter(!.data[[col]] %in% input$cat_filter)
        }
      }
    }
    dt
  })
  
}
