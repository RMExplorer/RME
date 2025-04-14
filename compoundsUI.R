output$RMESearch <- renderUI({
  list(
    tags$div(
      uiOutput("js_code2"),
      div(
        uiOutput("searchAnalyte"),
        div(
          checkboxInput("additiveTable", 
                        label ="Add to the Table", TRUE, width = "fit-content"),
            tooltip_ui("checkboxTooltip", 
                       "Un-select this if you want to remove all entries in the table before adding a new substance."),
            style="display:flex;"),
        style = "display:flex;gap:10px;align-items: flex-end;"
      ),
      textOutput("urlerror"),
      div(
        actionButton("removeAnalyte", 
                     "Remove Selected Rows From Your Table", 
                     class="btn-outline-warning"),
        actionButton("removeAllAnalytes", 
                     "Remove All Rows From Your Table", 
                     class="btn-outline-danger"),
        actionButton("addallSubstances", div("Add All Substances to the table", tooltip_ui("substanceaddTooltip", 
                                                                                        "May take up to 3+ minutes. Not all substances from the NRC repository will be added due to search limitations."),
                                   style="display:flex;"), 
                     class="btn-outline-success"
        ),
        conditionalPanel('output.userLoggedIn', 
                         actionButton("saveAnalytes", "Save Table To Your Account", 
                                      class="btn-outline-success"
                                      )
                         ),
        conditionalPanel('output.userLoggedIn', 
                         actionButton("loadAnalytes", "Load Table From Your Account", 
                                      class="btn-outline-info", 
                                     )
        ),
        style = "display:flex;gap:10px;padding:0px 0px 10px 0px;"
      ),
      p("Instructions: Add substances to the table below using the search dropdown above. 
        The table is linked to the `Properties`, `pKow-MW Plot`, and `Spectral Data` tab. 
        Select one row from the table in order to see its properties in the `Properties` tab, 
        or its spectral data in the `Spectral Data` tab. Reference Materials that appear in all rows are highlighted in <em>red<em>."),
      conditionalPanel('!output.userLoggedIn', 
                       p(em("Log in if you want to save your selections or view your saved selections."))
                       ),
      conditionalPanel(
        "$('#customTable').hasClass('recalculating') | $('#customTable').css('display') === 'none'", 
        tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                 style="display:flex;justify-content:center;")),
      DTOutput("customTable"),
      style = "padding:20px 0px 20px 0px;"
    )
  )
})
  