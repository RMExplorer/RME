# runs a query against the DB and returns the result as a dataframe
dbQuery <- function(sql, params = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql, params = params)
}

# builds "?,?,?" for an IN (...) clause with one placeholder per value
placeholders <- function(x) paste(rep("?", length(x)), collapse = ",")

# choices for the drop downs in the crm search page
crmChoices <- dbQuery("SELECT name FROM reference_materials
                       WHERE name IS NOT NULL AND name <> ''
                       ORDER BY name COLLATE NOCASE")$name
materialChoices <- dbQuery("SELECT DISTINCT material_type FROM reference_materials
                            WHERE material_type IS NOT NULL AND material_type <> ''
                            ORDER BY material_type COLLATE NOCASE")$material_type
# affiliation is stored as one comma-separated string per crm, so split it into individual affiliates
affiliateChoices <- dbQuery("SELECT affiliation FROM reference_materials
                             WHERE affiliation IS NOT NULL AND affiliation <> ''")$affiliation
affiliateChoices <- trimws(unlist(strsplit(affiliateChoices, ", ", fixed = TRUE)))
affiliateChoices <- unique(affiliateChoices[nzchar(affiliateChoices)])
affiliateChoices <- affiliateChoices[order(tolower(affiliateChoices))]

crmList <- reactiveVal(list())

#reactive variable which stores the data frame with all the contents of the data table in the 'crm search' page
crmTableData <- reactive({
  getCRMData(crmList())
})

# function that takes the names of crms and gathers their information from the database
getCRMData <- function(crm){
  req(length(crmList()) > 0)
  shinyjs::hide("crmTable")
  
  # crmList() can be a mix of lists and vectors, so flatten it
  crm <- unique(as.character(unlist(crm)))
  
  data <- dbQuery(paste0("
    SELECT rmid, name, affiliation, material_type
    FROM reference_materials
    WHERE name IN (", placeholders(crm), ")
    ORDER BY name COLLATE NOCASE"), as.list(crm))
  
  # link that opens the crm's info modal (sprintf returns character(0) when there are no rows)
  nameHTML <- sprintf('<a href="#" class="view-info" data-name="%s">%s</a>', data$name, data$name)
  
  data <- data.frame("ID" = data$rmid, "CRM" = data$name, "Name" = nameHTML, 
                     "Affiliates" = data$affiliation, "Material Type" = data$material_type,
                     check.names = FALSE, stringsAsFactors = FALSE)
  shinyjs::show("crmTable")
  return(data)
}

output$searchCRM <- renderUI({
  selectizeInput(inputId = "selectCRM", label = div(
    style = "display: flex; align-items: center; gap: 6px;",
    span("Search for a CRM"),
    tooltip_ui(
      "crmsearchTooltip",
      "CRMs related to your search will also be added."
    )
  ),
  choices = c("", crmChoices), 
  selected = NULL, 
  width="100%")
})

#when a new crm is selected, add it to the table list
observeEvent(input$selectCRM, {
  req(input$selectCRM)
  
  link = paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=',
                gsub(' ','+', input$selectCRM), '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
  
  #overrides the ssl verifypeer so the webpage can be reached on shinyapps
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = 0)
  d = read_html(geturl(link, h))
  rm(h)
  req(d)
  
  titles <- d %>% xml_find_all(xpath="//title") %>% xml_text()
  crms <- sapply(str_split(titles,":"), function(x) x[1])
  crms <- crms[crms != "NRC Digital Repository"]
  crms <- crms[crms !=  "No results"]
  crms <- crms[!crms %in% crmList()]
  newList <- append(crms, crmList())
  crmList(newList)
  
  #refresh the select input so it doesnt show previous selection in the box
  updateSelectizeInput(session, inputId = "selectCRM", "Search for a CRM", 
                       choices = c("", crmChoices),
                       selected = NULL)
})

#the 'Select an Affiliate' drop down in the 'crm search' page
output$searchAffiliate <- renderUI({
  #affiliates is calculated in global.R
  selectizeInput(inputId = "selectedAffiliate", "Select an Affiliate", 
                 choices = append("", affiliateChoices), selected = NULL, width="100%")
})

#the 'Select material type' drop down in the 'crm search' page
output$searchMaterial <- renderUI({
  selectizeInput(inputId = "selectedMaterial", "Select a Material Type", 
                 choices = append("", materialChoices), selected = NULL, width="100%")
})

#when a new affiliate is selected, add it to the table list
observeEvent(input$selectedAffiliate, {
  req(input$selectedAffiliate)
  crms <- dbQuery("SELECT name FROM reference_materials WHERE INSTR(affiliation, ?) > 0",
                  list(input$selectedAffiliate))$name
  crms <- crms[!crms %in% crmList()]
  newList <- append(crms, crmList())
  crmList(newList)
  
  #refresh the select input so it doesnt show previous selection in the box
  updateSelectizeInput(session, inputId = "selectedAffiliate", label = "Select an Affiliate",
                       choices = append("", affiliateChoices),
                       selected = NULL)
})

# when a new material type is selected, replace the table list with the crms of that type
observeEvent(input$selectedMaterial, {
  req(input$selectedMaterial)
  crms <- dbQuery("SELECT name FROM reference_materials WHERE INSTR(material_type, ?) > 0",
                  list(input$selectedMaterial))$name
  crmList(crms)
  
  #refresh the select input so it doesnt show previous selection in the box
  updateSelectizeInput(session, inputId = "selectedMaterial", label = "Select a Material Type",
                       choices = append("", materialChoices),
                       selected = NULL)
})

#when the 'Remove Selected Rows...' button is clicked and rows are selected, remove the crm from the reactive variable crmList
observeEvent(input$removeCRM, {
  req(input$crmTable_rows_selected)
  selected <- crmTableData() %>% slice(input$crmTable_rows_selected)
  names <- selected$CRM
  
  newList <- crmList()
  newList <- newList[!newList %in% names]
  crmList(newList)
})

#when the 'Remove All Rows...' button is clicked, remove all the names from the reactive variable crmList
observeEvent(input$removeAllCRM, {
  newList <- list()
  crmList(newList)
})

output$crmTable <- renderDT({
  # when no CRMs have been added yet, show the empty table
  if (length(crmList()) == 0) {
    emptyData <- data.frame("Name" = character(0),
                            "Affiliates" = character(0),
                            "Material Type" = character(0),
                            check.names = FALSE)
    return(datatable(emptyData, 
                     options = list(scrollX = TRUE, autoWidth = TRUE, dom = 'ltip'),
                     escape = FALSE,
                     rownames = FALSE,
                     filter= list(position='top', clear = FALSE)))
  }
  req(length(crmTableData()) > 0)
  data <- crmTableData()
  datatable(data[, c("Name", "Affiliates", "Material Type")], 
            options = list(scrollX = TRUE, autoWidth = TRUE, dom = 'ltip',
                           # truncate long Affiliates values. Clicking "more"/"less" expands or collapses
                           # the cell (handled by the .cell-toggle click handler in ui.R)
                           columnDefs = list(list(
                             targets = 1,
                             render = JS(
                               "function(data, type, row, meta) {",
                               "  if (type === 'display' && data != null && data.length > 40) {",
                               "    var short = data.substr(0, 40) + '...';",
                               "    return '<span class=\"cell-expand\" data-full=\"' + encodeURIComponent(data) +",
                               "           '\" data-short=\"' + encodeURIComponent(short) +",
                               "           '\" data-expanded=\"false\">' + short + ' <a href=\"#\" class=\"cell-toggle\">more</a></span>';",
                               "  }",
                               "  return data;",
                               "}"
                             )
                           ))),
            escape = FALSE,
            rownames = FALSE,
            filter= list(position='top', clear = FALSE))
})

#button to add all crms to the table
observeEvent(input$addAllCRMs, {
  req(input$addAllCRMs)
  crms <- crmChoices
  crms <- crms[!crms %in% crmList()]
  newList <- append(crms, crmList())
  crmList(newList)
})

#fired when user trys to add the analytes from a crm to the substance table
observeEvent(input$addCRM, {
  req(input$crmTable_rows_selected)
  
  # switch the sidebar accordion over to the compound search
  accordion_panel_open("searchAccordion", "compound")
  accordion_panel_close("searchAccordion", "crm")
  
  selected <- crmTableData() %>% slice(input$crmTable_rows_selected)
  ids <- selected$ID
  
  # every analyte listed in the analyte tables of the selected crms
  namesToAdd <- dbQuery(paste0("SELECT DISTINCT name FROM analyte_tables WHERE rmid IN (",
                               placeholders(ids), ")"), as.list(ids))$name
  
  namesToAdd <- namesToAdd[!namesToAdd %in% yourTableAnalytes()]
  newList <- append(namesToAdd, yourTableAnalytes())
  yourTableAnalytes(newList)
})


#unselect all rows in the custom table when unselect button is clicked
observeEvent(input$unselectCRMs, {
  selectRows(proxy = dataTableProxy("crmTable", session = session), 
             selected = NULL)
})