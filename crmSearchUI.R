output$crmSearch <- renderUI({
  list(
    tags$div(
      uiOutput("js_code"),
      div(
        uiOutput("searchCRM"),
        uiOutput("searchAffiliate"),
        uiOutput("searchMaterial"),
        style = "display:flex;gap:10px;"
      ),
      div(
        actionButton("removeCRM", 
                     "Remove Selected Rows From Your Table", 
                     class="btn-outline-warning"),
        actionButton("removeAllCRM", 
                     "Remove All Rows From Your Table", 
                     class="btn-outline-danger"),
        actionButton("addAllCRMs", "Add All CRMs to the table", 
                     class="btn-outline-success"
        ),
        actionButton("addCRM", div("Add Choosen CRM(s) to the Substance table", tooltip_ui("crmaddTooltip", 
                                          "Select CRM rows, then press this button to add the corresponding substances to the substance table. Go to the General Search tab to view the substance table."),
                                   style="display:flex;"), 
                     class="btn-outline-success"
        ),
        style = "display:flex;gap:10px;padding:0px 0px 10px 0px;"
      ),
      conditionalPanel(
        "$('#crmTable').hasClass('recalculating') | $('#crmTable').css('display') === 'none'", 
        tags$div(img(src='loading.gif', style = "height: 4rem;"), 
                 style="display:flex;justify-content:center;")),
      DTOutput("crmTable"),
      style = "padding:20px 0px 20px 0px;"
    )
  )
})