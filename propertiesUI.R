#contains the properties UI
#the rendering for the text content is done in propertiesText.R
output$properties <- renderUI({
  if (shinybrowser::get_device() == "Mobile") {
    plotSize = "44vh"
  }
  else {
    plotSize = "70vh"
  }

  list(
    conditionalPanel("$('#summary').hasClass('recalculating') | $('#html').hasClass('shiny-busy')", 
                     fixedPanel(
                       tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                                style="display:flex;justify-content:center;align-content:center;flex-wrap:wrap;"),
                       top = 0,
                       left = 0,
                       right = 0,
                       bottom = 0,
                       style="display:flex;justify-content:center;align-content: center;background-color: rgba(255, 255, 255, 1);"
                     )),
    div(uiOutput("propertiesNothingSelected"), style="display:flex;justify-content:center;font-size:2rem;"),
    fluidRow(
      column(5, align = "center",
             br(),
             plotOutput('molecule', height = plotSize)
      ),
      column(7, align="left",
             h4(textOutput('currentCompoundName')),
             uiOutput("hasSpectrum"),
             uiOutput("information"),
             uiOutput("similarCompounds"),
             uiOutput("selectCRMdropdown"),
             (uiOutput('title')),
             htmlOutput('summary'), 
             uiOutput("noInfo"),
             uiOutput('doi'), 
             DTOutput('analyteTable'),
             uiOutput("analyteInfoDownload"),
             htmlOutput('date'), 
             br()
      )
    )
  )
})