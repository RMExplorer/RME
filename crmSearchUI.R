output$crmSearch <- renderUI({
  list(
    tags$div(
      #search inputs
      fluidRow(
        column(
          width = 12,
          uiOutput("searchCRM") 
        ),
        column(
          width = 6,
          uiOutput("searchAffiliate")
        ),
        column(
          width = 6,
          uiOutput("searchMaterial")
        )
      ),
      
      #actions that add to the compound table
      div(
        actionButton("addCRM", 
                     div("Add chosen CRM(s) to the Compound table", 
                         tooltip_ui("crmaddTooltip", 
                                    "Select CRM rows, then press this button to add the corresponding compounds to the compound table. The Compound Search panel will open so you can view it."),
                         style = "display:flex;gap:4px;"), 
                     class = "btn-success btn-sm"),
        actionButton("addAllCRMs", "Add all CRMs to the table", 
                     class = "btn-outline-success btn-sm"),
        style = "display:flex;flex-wrap:wrap;gap:8px;padding:0.25rem 0 0.75rem 0;"
      ),
      
      hr(),
      
      #table header with the actions that remove from the table
      div(
        h6("Your CRM table", style = "margin:0;font-weight:bold;"),
        div(
          actionButton("unselectCRMs", "Unselect All Rows", class="btn-outline-warning btn-sm"),
          actionButton("removeCRM", "Remove selected", class = "btn-outline-danger btn-sm"),
          actionButton("removeAllCRM", "Clear all", class = "btn-danger btn-sm"),
          style = "display:flex;gap:8px;"
        ),
        style = "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;padding-bottom:0.5rem;"
      ),
      
      conditionalPanel(
        "$('#crmTable').hasClass('recalculating') | $('#crmTable').css('display') === 'none'", 
        tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                 style="display:flex;justify-content:center;")),
      DTOutput("crmTable"),
      style = "padding:10px 0px 20px 0px; max-width: 100%;"
    )
  )
})