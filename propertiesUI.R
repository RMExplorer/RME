## propertiesUI.R
## Renders output$properties for use in an accordion_panel(..., uiOutput("properties))

output$properties <- renderUI({
  
  # nothing selected: just the instruction line, centered, no cards
  if (is.null(viewedCompound())) {
    return(tagList(
      div(
        style = "display:flex; justify-content:center; align-items:center; 
                 min-height:70vh; text-align:center; font-size:1.5rem; color:#6c757d;",
        p(HTML("Use the <strong>CRM Search</strong> tab to search for CRMs. Add their corresponding 
                compounds to the <strong>Compound Table</strong> using the <strong>Add chosen CRM(s)
                </strong> button, or use the <strong>Compound Search</strong> tab to search for 
                compounds directly. To view a compound’s properties, select it's row in the 
                <strong>Compound Table</strong> and click <strong>View Properties</strong>."))
      )
    ))
  }
  
  tagList(
    conditionalPanel("$('#summary').hasClass('recalculating') | $('#html').hasClass('shiny-busy')", 
                     fixedPanel(
                       tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                                style="display:flex;justify-content:center;align-content:center;flex-wrap:wrap;"),
                       top = 0,
                       left = 0,
                       right = 0,
                       bottom = 0,
                       style="display: flex; justify-content: center; align-content: center; 
                       background-color: rgba(255, 255, 255, 0.6); z-index: 9999 !important;"
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
        uiOutput("similarCompounds"),
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
      card_header("CRM Information",
                  style = "font-weight:bold; background: transparent; border-bottom: 1px solid #eee; padding-left:0; padding-top:0;"),
      uiOutput("massRange"),
      hr(),
      uiOutput("selectCRMdropdown"),
      hr(),
      uiOutput('title'),
      htmlOutput('summary'), 
      uiOutput("noInfo"),
      uiOutput('doi'), 
      htmlOutput('date'),
      uiOutput("analyteInfoDownload"),
      DTOutput('analyteTable')
    ),
    card(
      id = "spectralDataSection",
      style = "padding: 1.25rem; margin-top: 1.5rem;",
      card_header("Spectral Data",
                  style = "font-weight:bold; background: transparent; border-bottom: 1px solid #eee; padding-left:0; padding-top:0;"),
      uiOutput("spectrumView")
    )
  )
})