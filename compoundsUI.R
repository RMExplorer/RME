output$RMESearch <- renderUI({
  list(
    tags$div(
      uiOutput("js_code2"),
      textOutput("urlerror"),
      p("Instructions: Add substances to the table below using the search dropdown above. 
        The table is linked to the `Properties`, `pKow-MW Plot`, and `Spectral Data` tab. 
        Select one row from the table in order to see its properties in the `Properties` tab, 
        or its spectral data in the `Spectral Data` tab. Reference Materials that appear in all rows are highlighted in red."),
      conditionalPanel(
        "$('#customTable').hasClass('recalculating') | $('#customTable').css('display') === 'none'", 
        tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                 style="display:flex;justify-content:center;")),
      DTOutput("customTable"),
    )
  )
})
  