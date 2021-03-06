# QS---
# filter radio buttons ¿inline o block?
# filter insert ui afterEnd (para que queden primero los últimos) ¿o no?

# select

select_moduleUI <- function (id, data, options_0 = NULL, label_1 = NULL, label_2 = NULL, options_2 = NULL, placeholder_3 = NULL) {
  
  ns <- NS(id)
  opt_0 <- c("sel_00", "sel_01")
  opt_2 <- c("sel_10", "sel_11")
  names(opt_0) <- options_0 %||% c("select", "unselect")
  names(opt_2) <- options_2 %||% c("that contain", "that do not contain")
  placeholder_3 <- placeholder_3 %||% "Write text"
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


# filter

filter_moduleUI <- function (id, data) {
  
  ns <- NS(id)
  div(id = "filter_", 
      selectizeInput(ns("filter_0"), "", names(data), multiple = TRUE, options = list("plugins" = list("remove_button"))))
  
}


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
