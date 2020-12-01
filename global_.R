select_moduleUI <- function (id, data, options_0 = NULL, label_1 = NULL, label_2 = NULL, options_2 = NULL, placeholder_3 = NULL) {
  
  ns <- NS(id)
  opt_0 <- c("sel_00", "sel_01")
  opt_2 <- c("sel_10", "sel_11")
  names(opt_0) <- options_0 %||% c("select", "unselect")
  names(opt_2) <- options_2 %||% c("that contain", "that do not contain")
  placeholder_3 <- placeholder_3 %||% "Write text"
  # tagList(radioButtons(ns("sel_un_select"), "", opt_0),
  #         selectizeInput(ns("columns_select"), label_1, names(data), multiple = TRUE, options = list("plugins" = list("remove_button"))),
  #         radioButtons(ns("con_not_select"), label_2, opt_2),
  #         textInput(ns("text_select"), "", placeholder = placeholder_3))
  tagList(radioButtons(ns("select_0"), "", opt_0),
          selectizeInput(ns("select_1"), label_1, names(data), multiple = TRUE, options = list("plugins" = list("remove_button"))),
          radioButtons(ns("select_2"), label_2, opt_2),
          textInput(ns("select_3"), "", placeholder = placeholder_3))
}

select_moduleServer <- function (id, data) {
  
  moduleServer(id, function (input, output, session) {
    d0 <- data
    
    req(input$select_0, input$select_2)
    l0 <- list(sel_00 = input$select_1, sel_10 = input$select_3)
    l1 <- list(sel_01 = input$select_1, sel_11 = input$select_3)
    l2 <- c(l0[[input$select_0]], l0[[input$select_2]]) 
    l2 <- l2[l2 %>% map_lgl(isTruthy)]
    l3 <- c(l1[[input$select_0]], l1[[input$select_2]])
    l3 <- l3[l3 %>% map_lgl(isTruthy)]
    if (isTruthy(l2) & isTruthy(l3)) {
      d0 <- select(d0, contains(l2), !contains(l3)) 
    } else if (isTruthy(l2)) {
      d0 <- select(d0, contains(l2))
    } else if (isTruthy(l3)) {
      d0 <- select(d0, !contains(l3))
    } 
    d0
  })
  
}