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
        f7Button("removeAnalyte", "Remove Selected Rows From Your Table", color = "orange"),
        f7Button("removeAllAnalytes", "Remove All Rows From Your Table", color = "red"),

        f7Button(inputId = "addallSubstances", "Add All Substances to the table", color = "green"),
        #f7Tooltip(
          #tag = "addallSubstances",
          #text = "May take up to 3+ minutes. Not all substances from the NRC repository will be added due to search limitations."
        #),

        input_task_button("saveAnalytes", "Save Table Substances", 
                    class="btn-outline-success"
        ),
       input_task_button("loadAnalytes", "Load Saved Substances", 
                    class="btn-outline-info"
        ),
        style = "display:flex;gap:10px;padding:0px 0px 10px 0px;"
      ),
      p("Instructions: Add substances to the table below using the search dropdown above. 
        The table is linked to the `Properties`, `pKow-MW Plot`, and `Spectral Data` tab. 
        Select one row from the table in order to see its properties in the `Properties` tab, 
        or its spectral data in the `Spectral Data` tab. Reference Materials that appear in all rows are highlighted in red."),
      conditionalPanel(
        "$('#customTable').hasClass('recalculating') | $('#customTable').css('display') === 'none'", 
        tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                 style="display:flex;justify-content:center;")),
      DTOutput("customTable"),
      style = "padding:20px 0px 20px 0px;"
    )
  )
})
  