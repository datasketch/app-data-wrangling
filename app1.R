library(shinypanels)

styles <- "
.container {
  border: 2px solid #eee;
  overflow: scroll;
  /*position: relative;*/
  height: 400px;
}

.has-tooltip {  
  /*position: relative;*/
}

.tooltip-wrapper {
  position: absolute;
  visibility: hidden;
}

.has-tooltip:hover .tooltip-wrapper {
  visibility: visible !important;
  opacity: 0.7;
  /*top: 30px;*/
  /*left: 50%;*/
  /*margin-left: -76px;*/
  /* z-index: 999; defined above with value of 5 */
}

.tooltip {
  display: block;
  position: relative;
    top: 0em;
    right: 2em;
  width: 140px;
  height: 96px;
  /*margin-left: -76px;*/
  color: #FFFFFF;
  background: #000000;
  line-height: 96px;
  text-align: center;
  border-radius: 8px;
  box-shadow: 4px 3px 10px #800000;
}
"
ui <- panelsPage(styles = styles,
                 panel(title = "options",
                       width = 250,
                       color = "chardonnay",
                       body = list(uiOutput("controls1"),
                                   br(),
                                   uiOutput("controls2"))))


server <- function(input, output, session) {
  
  
  output$controls1 <- renderUI({
    # infoTooltip("Shinypanels' tooltip", "Te jej jje fjsalsd fjjf dsjjdsalf jfk")
    div(style = "display: flex;",
        selectInput("sel0", "Filter by column", choices))
  })
  
  output$controls2 <- renderUI({
    div(class = "container",
        a(class = "has-tooltip",
          href = "#",
          "This is shows a tooltip that is within a scrollable container and overflows' it",
          span(class = "tooltip-wrapper",
               span(class = "tooltip", 
                    "R"))),
        br(),
        br(),
        div("iv>Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        Proin porttitor elit neque, in condimentum ante pulvinar et. 
        Donec et erat nulla. Vestibulum quis porta tellus. Curabitur
        non blandit metus. Vestibulum nec nisi quis urna tempor pharetra.
        Phasellus volutpat, arcu ac malesuada porttitor,
        iv>Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
        Proin porttitor elit neque, in condimentum ante pulvinar et. 
        Donec et erat nulla. Vestibulum quis porta tellus. Curabitur
        non blandit metus. Vestibulum nec nisi quis urna tempor pharetra.
        Phasellus volutpat, arcu ac malesuada porttitor
        iv>Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
        Proin porttitor elit neque, in condimentum ante pulvinar et. 
        Donec et erat nulla. Vestibulum quis porta tellus. Curabitur
        non blandit metus. Vestibulum nec nisi quis urna tempor pharetra.
        Phasellus volutpat, arcu ac malesuada porttitor
        iv>Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
        Proin porttitor elit neque, in condimentum ante pulvinar et. 
        Donec et erat nulla. Vestibulum quis porta tellus. Curabitur
        non blandit metus. Vestibulum nec nisi quis urna tempor pharetra.
        Phasellus volutpat, arcu ac malesuada porttitor
        iv>Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
        Proin porttitor elit neque, in condimentum ante pulvinar et. 
        Donec et erat nulla. Vestibulum quis porta tellus. Curabitur
        non blandit metus. Vestibulum nec nisi quis urna tempor pharetra.
        Phasellus volutpat, arcu ac malesuada porttitor"))
  })
  
}


shinyApp(ui, server)