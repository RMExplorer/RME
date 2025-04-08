tooltip_ui <- function(id, text, location = "right"){
  ns <- NS(id)
  tags$div(
    tooltip(bsicons::bs_icon("info-circle", title = " ", size= "1rem"), 
            text, 
            placement = location), 
    style="padding:0px 10px 0px 10px;")
}