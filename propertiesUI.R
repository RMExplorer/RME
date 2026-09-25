#contains the properties UI
#the rendering for the text content is done in propertiesText.R
output$properties <- renderUI({
  tagList(
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
    
    div(
      style = "padding: 0.25rem 0 1rem 0;",
      h3(textOutput('currentCompoundName'), style="font-weight:bold; margin-bottom: 0.5rem;"),
      uiOutput("hasSpectrum")
    ),
    
    layout_columns(
      col_widths = c(7, 5),
      
      card(
        style = "padding: 1.25rem;",
        card_header("Compound Information",
                    style = "font-weight:bold; background: transparent; border-bottom: 1px solid #eee; padding-left:0; padding-top:0;"),
        uiOutput("information"),
        hr(),
        uiOutput("similarCompounds")
      ),
      
      card(
        style = "padding: 1.25rem; display:flex;",
        card_header("Structure",
                    style = "font-weight:bold; background: transparent; border-bottom: 1px solid #eee; padding-left:0; padding-top:0;"),
        plotOutput('molecule', height = "55vh")
      )
    ),
    
    card(
      style = "padding: 1.25rem; margin-top: 1.5rem;",
      card_header("Certificate Information",
                  style = "font-weight:bold; background: transparent; border-bottom: 1px solid #eee; padding-left:0; padding-top:0;"),
      uiOutput("selectCRMdropdown"),
      hr(),
      uiOutput('title'),
      htmlOutput('summary'), 
      uiOutput("noInfo"),
      uiOutput('doi'), 
      DTOutput('analyteTable'),
      uiOutput("analyteInfoDownload"),
      htmlOutput('date'), 
      br()
    ),
    card(
      id = "spectralDataSection",
      style = "padding: 1.25rem; margin-top: 1.5rem;",
      card_header("Spectral Data",
                  style = "font-weight:bold; background: transparent; border-bottom: 1px solid #eee; padding-left:0; padding-top:0;"),
      output$spectrumUI <- renderUI({
        list(
          uiOutput("spectrumView")
        )
      })
    )
  )
})