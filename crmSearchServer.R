crmList <- reactiveVal(list())

#reactive variable which stores the data frame with all the contents of the data table in the 'crm search' page
crmTableData <- reactive({
  getCRMData(crmList())
})

#function that takes the name of a crn and runs it through DR to gather information and return a dataframe
getCRMData <- function(crm){
  req(length(crmList()) > 0)
  shinyjs::hide("crmTable")
  
  #recordDF is calculated in global.R
  data <- recordDF[recordDF$crm %in% crm,]
  ids <- data$id
  formats <- c()
  for (id in ids){
    link <- paste("https://nrc-digital-repository.canada.ca/eng/view/object/?id=", id, sep="")
    
    #overrides the ssl verifypeer so the webpage can be reached
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    ddf = rvest::html_table(html_nodes(read_html(geturl(link, h)),'table'))
    rm(h)
    req(ddf)
    table <- as.data.frame(ddf[[1]])
    format <- table[table$X1 == "Format", "X2"]
    formats <- append(formats, format)
  }
  
  nameHTML <- c()
  for (name in data$crm) {
    nameHTML <- append(nameHTML, HTML(paste0(
      "<a href=\"#\" class=\"view-info\" data-name=", name,">", name, "</a>",sep="")))
  }
  
  data <- data.frame("ID" = ids, "Name" = nameHTML, "Affiliates" = data$affiliation, "Format" = formats, "Material Type" = data$materialType)
  shinyjs::show("crmTable")
  return(data)
}

output$searchCRM <- renderUI({
  selectizeInput(inputId = "selectCRM", div("Search for a CRM", 
                                                tooltip_ui("crmsearchTooltip", 
                                                           "CRMs related to your search will also be added."),
                                                style="display:flex;"), 
                 choices = c("", allCrms), 
                 selected = NULL, 
                 width="350px")
})

#when a new crm is selected, add it to the table list
observeEvent(input$selectCRM, {
  req(input$selectCRM)
  #will search the repository with the name/inchikey the analyte was searched with
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
                       choices = c("", allCrms),
                       selected = NULL)
})

#the 'Select an Affiliate' drop down in the 'crm search' page
output$searchAffiliate <- renderUI({
  #affiliates is calculated in global.R
  selectizeInput(inputId = "selectedAffiliate", "Select an Affiliate", 
                 choices = append("", affiliates), selected = NULL, width="500px")
})

#the 'Select material type' drop down in the 'crm search' page
output$searchMaterial <- renderUI({
  #materials is calculated in global.R
  selectizeInput(inputId = "selectedMaterial", "Select a Material Type", 
                 choices = append("", materials), selected = NULL, width="500px")
})

#when a new affiliate is selected, add it to the table list
observeEvent(input$selectedAffiliate, {
  req(input$selectedAffiliate)
  crms <- recordDF[grepl(input$selectedAffiliate, recordDF$affiliation), "crm"]
  crms <- crms[!crms %in% crmList()]
  newList <- append(crms, crmList())
  crmList(newList)
  
  #refresh the select input so it doesnt show previous selection in the box
  updateSelectizeInput(session, inputId = "selectedAffiliate", label = "Select an Affiliate",
                       choices = append("", affiliates),
                       selected = NULL)
})

#when a new affiliate is selected, add it to the table list
observeEvent(input$selectedMaterial, {
  req(input$selectedMaterial)
  crms <- recordDF[grepl(input$selectedMaterial, recordDF$materialType), "crm"]
  crmList(crms)
  
  #refresh the select input so it doesnt show previous selection in the box
  updateSelectizeInput(session, inputId = "selectedMaterial", label = "Select a Material Type",
                       choices = append("", materials),
                       selected = NULL)
})

#when the 'Remove Selected Rows...' button is clicked and rows are selected, remove the crm from the reactive variable crmList
observeEvent(input$removeCRM, {
  req(input$crmTable_rows_selected)
  selected <- crmTableData() %>% slice(input$crmTable_rows_selected)
  names <- selected$Name
  
  newList <- yourTableAnalytes()
  newList <- newList[!newList %in% names]
  crmList(newList)
})

#when the 'Remove All Rows...' button is clicked, remove all the names from the reactive variable crmList
observeEvent(input$removeAllCRM, {
  newList <- list()
  crmList(newList)
})

output$crmTable <- renderDT({
  req(length(crmTableData()) > 0)
  data <- crmTableData()
  colnames(data) <- c("id", "Name", "Affiliates", "Format", "Material Type")
  datatable(data[, c("Name", "Affiliates", "Format", "Material Type")], escape = FALSE,
            filter= list(position='top', clear = FALSE))
})

#button to add all crms to the table
observeEvent(input$addAllCRMs, {
  req(input$addAllCRMs)
  crms <- recordDF$crm
  crms <- crms[!crms %in% crmList()]
  newList <- append(crms, crmList())
  crmList(newList)
})

#fired when user trys to add the analytes from a crm to the substance table
observeEvent(input$addCRM, {
  req(input$crmTable_rows_selected)
  selected <- crmTableData() %>% slice(input$crmTable_rows_selected)
  #"<a href=\"#\" class=\"view-info\" data-name=", name,">", name, "</a>"
  ids <- selected$ID
  namesToAdd <- c()
  
  for (id in ids){
    link <- paste("https://nrc-digital-repository.canada.ca/eng/view/object/?id=", id, sep="")
    
    #overrides the ssl verifypeer so the webpage can be reached
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    ddf = rvest::html_table(html_nodes(read_html(geturl(link, h)),'table'))
    rm(h)
    req(ddf)
    
    if(length(ddf) >= 3 & grepl('Analyte',paste(ddf[3]))){
      analyteTable <- data.frame(ddf[[3]])
      namesToAdd <- append(analyteTable$Analyte, namesToAdd)
    }
  }
  
  namesToAdd <- unique(namesToAdd)
  namesToAdd <- namesToAdd[!namesToAdd %in% yourTableAnalytes()]
  newList <- append(namesToAdd, yourTableAnalytes())
  yourTableAnalytes(newList)
})


#unselect all rows in the custom table when unselect button is clicked
observeEvent(input$unselectCRMs, {
  selectRows(proxy = dataTableProxy("crmTable", session = session), 
             selected = NULL)
})