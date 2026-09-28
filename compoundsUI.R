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
                       "Un-select this if you want to remove all entries in the table before adding a new compound."),
            style="display:flex;"),
        style = "display:flex;gap:10px;align-items: flex-end;"
      ),
      
      textOutput("urlerror"),
      
      div(
        actionButton("addallSubstances", div("Add All Compounds", tooltip_ui("substanceaddTooltip", 
                                                                                          "May take up to 3+ minutes. Not all 
                                                                                          compounds from the NRC repository will be 
                                                                                          added due to search limitations."),
                                             style="display:flex;"), 
                     class="btn-success btn-sm"
        ),
        input_task_button("saveAnalytes", "Save Table Compounds", 
                          class="btn-outline-success btn-sm"
        ),
        input_task_button("loadAnalytes", "Load Saved Compounds", 
                          class="btn-outline-info btn-sm"
        ),
        style = "display:flex;gap:10px;padding:0px 0px 10px 0px;"
      ),
      
      p("Instructions: Add compounds to the table below using the search dropdown above. 
        Select one row from the table in order to see its properties. 
        The table is linked to the `Polarity-MW Plot` tab.
        Reference Materials that appear in all rows are highlighted in red."),
      
      hr(),
      
      #table header with the actions that remove from the table
      div(
        h6("Your compound table", style = "margin:0;font-weight:bold;"),
        div(
          actionButton("unselect", "Unselect All Rows", class="btn-outline-warning btn-sm"),
          actionButton("removeAnalyte", "Remove Selected", class="btn-outline-danger btn-sm"),
          actionButton("removeAllAnalytes", "Clear All", class="btn-danger btn-sm"),
          style = "display:flex;gap:8px;"
        ),
        style = "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;padding-bottom:0.5rem;"
      ),
      
      conditionalPanel(
        "$('#customTable').hasClass('recalculating') | $('#customTable').css('display') === 'none'", 
        tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                 style="display:flex;justify-content:center;")),
      DTOutput("customTable"),
      style = "padding:20px 0px 20px 0px; max-width: 100%;"
    )
  )
})
  