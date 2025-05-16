#text shown when none or more than 1 analyte is selected
output$propertiesNothingSelected <- renderUI({
  req(length(input$customTable_rows_selected) != 1)
  HTML(paste(
    "<p>Make sure you have selected a <strong>single</strong> option from the table in the 'General Search' tab.</p>"
  ))
})

#dropdown with all the crm options of the given compound
output$selectCRMdropdown = renderUI({
  req(length(input$customTable_rows_selected) == 1)
  req(v$crms != "No results")
  pickerInput(inputId = "selectedCRM", strong("Select a CRM for the analyte of interest from the dropdown below"), 
              choices = v$crms)
})

#for the sketch of the molecule in the Properties tab
output$molecule <- renderPlot({
  req(length(input$customTable_rows_selected) == 1)
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  selectedAnalyte <- result %>% slice(input$customTable_rows_selected)
  smil <- selectedAnalyte$"Isomeric Smiles"
  mol <- parse.smiles(smil)[[1]]
  req(mol)
  
  depictor <- get.depictor(
    width = 800,
    height = 800,
    zoom = 1.0,
    style = "cow",
    annotate = "off",
    abbr = "on",
    suppressh = TRUE,
    showTitle = FALSE,
    smaLimit = 100,
    sma = NULL,
    fillToFit = TRUE
  )
  
  img <- view.image.2d(mol, depictor = depictor)
  
  plot(x = 1,
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       xlim = c(0, 100),
       ylim = c(0, 100),
       main = "",
       type = "n")
  
  rasterImage(img, 1,1, 100,100)
},

  height = function () {
    session$clientData$output_molecule_height
  },
  width = function () {
    session$clientData$output_molecule_height
  }
)

output$currentCompoundName <- renderText({
  req(length(input$customTable_rows_selected) == 1)
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  data <- result %>% slice(input$customTable_rows_selected)
  paste('Showing Information on:', data$Name)
})

#the title of the crm
output$title <- renderText({
  req(length(input$customTable_rows_selected) == 1)
  req(length(input$selectedCRM)>0)
  req(v$crms != "No results")
  if(input$selectedCRM=='No results') return('')
  if(v$data$title[1]=='No results') return('')
  HTML("<p><strong>",paste(v$data[v$data$name == input$selectedCRM, 'title'], "</strong></p>"))
})

#the summary of the crm
output$summary <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  req(length(input$selectedCRM)>0)
  req(v$crms != "No results")
  if(input$selectedCRM=='No results') return('')
  if(v$data$title[1]=='No results') return('')
  if(!is.null(v$abstract)) paste(v$abstract) 
  else {
    HTML(paste("<div style='padding:10px 0px 10px 0px;'><p><strong>Summary</strong></p><p>",
               v$data[v$data$name == input$selectedCRM, 'summary'], "</p></div>"))
  }
})

#text that renders when any one of the crms have spectral data, indicating it can be viewed in the Spectral Data Tab
output$hasSpectrum <- renderUI ({
  req(length(input$customTable_rows_selected) == 1)
  req(v$crms != "No results")
  if (length(v$spectralData$SpectralLink[!is.na(v$spectralData$SpectralLink)]) > 0) {
    list(
      HTML(paste("<strong>This compound has spectral data.</strong>")),
      actionButton("showSpectrum", "Go to Spectral Data Tab", class="btn-info")
    )
  } else {
    HTML(paste(""))
  }
 
})

#opens the spectral data tab
observeEvent(input$showSpectrum, {
  updateF7Tabs(session, id = "mainTabs", selected = "spectrum") 
})

#dropdown list of all the similar compound's inchikey. To be selected and added to the compounds table
output$similarCompounds <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  data <- result %>% slice(input$customTable_rows_selected)
  req(data$CID)
  
  #finds all the similar compounds cids (converted to inchikey)
  similarCIDsLink <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/fastsimilarity_2d/cid/", data$CID,"/cids/JSON", sep="")
  similarCIDs <- fromJSON(similarCIDsLink)$IdentifierList$CID[c(1:100)]
  #making sure there is a maximum of 100 CIDs in order to not get error
  similarCIDs <- paste(similarCIDs[!is.na(similarCIDs)], collapse = ",")
  inchikeys <- fromJSON(paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", 
                              similarCIDs, 
                              "/property/InChIKey/JSON", sep=""))$PropertyTable$Properties$InChIKey
  #searches the dr for the inchikeys to see if we have them in our repository
  searchIds <- gsub("/", "%2F", inchikeys)
  inchilink <- paste('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=&q=',
                searchIds, '+&q=&y1=&y2=&ps=10&s=sc&av=1', sep="")
  results <- c()
  disable <- c()
  #using the inchikey, search the repository, if it exists, add the 1st synonym (name) for the compound to the similar compound dropdown
  for (i in 1:length(inchilink)) {
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    d = read_html(geturl(inchilink[[i]], h)) %>% xml_find_all(xpath="//id") %>% xml_text()
    #if the record has an id (meaning an entry exists in the repository), then add the name (1st synonym from pubchem) to the dropdown list 
    #(+ ignore if the inchikey is the same as the current selection)
    if (any(grepl("urn:uuid:", d)) && inchikeys[[i]] != data$InchiKey){
      synonymsLink <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/", 
                            inchikeys[[i]],"/synonyms/JSON", sep="")
      synonyms <- tryCatch({
        fromJSON(synonymsLink)$InformationList$Information$Synonym[[1]][1]
      }, error = function(e) {
          return(NA)
      })
      
      if (length(synonyms) > 0 && !is.na(synonyms)){results <- c(results, synonyms)}
      
      req(length(getTableData$result()) > 0)
      tableData <- getTableData$result()
      
      if (inchikeys[[i]] %in% tableData$InchiKey){
        disable <- c(disable, 1)
      } else {
        disable <- c(disable, 0)
      }
      
    }
    rm(h)
  }
  
  #remove inchikey of the current page if its in the list
  results <- unique(results[results != data$InchiKey])
  #remove any NAs
  results <- results[!is.na(results)]

  list(
    div(pickerInput(inputId = "similarCompound", label = div(strong("Similar Compounds"), 
                                             tooltip_ui("similarcompoundtooltip", 
                                                        "Shows names of compounds that are similar in our repository. 
                                                        Pick an option to add to the compounds table. 
                                                        Options already in the compounds table will not be selectable."),
                                             style="display:flex;"), 
                      choices = results, 
                    choicesOpt = list(disabled = disable),
                      width="320px"),
    actionButton("addSimilar", "Add Compound", class="btn-info", style="height:fit-content;width:100px;margin-top:15px;"), 
    style = "display:flex;gap:10px;align-items:center;"
    )
  )
})

#when the add compound button is clicked, adds the inchikey to the compounds table
observeEvent(input$addSimilar, {
  newList <- append(yourTableAnalytes(), input$similarCompound)
  yourTableAnalytes(newList)
})

#outputs the data pulled from pubchem (molecular formula, molecular weight, smiles, inchikey, exact mass, tpsa, pkow, pubchem link)
output$information <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  data <- result %>% slice(input$customTable_rows_selected)
  req(data$InchiKey)
  synonyms <- NA
  if (!is.na(data$CID)){
    synonymsLink <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", data$CID,"/synonyms/JSON", sep="")
    synonyms <- tryCatch({
      fromJSON(synonymsLink)$InformationList$Information$Synonym[[1]]
    }, error = function(e) {
      return(NA)
    })
    synonyms <- synonyms[c(1:10)]
    synonyms <- synonyms[!is.na(synonyms)]
  }
    
  HTML(paste(
    "<div style='padding:10px 0px 10px 0px;'><p><strong>Synonyms", 
    tooltip(bsicons::bs_icon("info-circle", title = " "), "Top synonyms for this compound from PubChem"), "</strong></p><p>", 
    paste(ifelse(any(is.na(synonyms)), "No Results", paste(synonyms, collapse=", "))), "</p></div>",
    "<div style='padding:10px 0px 10px 0px;'><p><strong>Molecular Formula</strong></p><p>", 
    paste(ifelse(is.na(data$"Molecular Formula"), "No Results", data$"Molecular Formula")), "</p></div>",
    "<div style='padding:10px 0px 10px 0px;'><p><strong>Molecular Weight</strong></p><p>", 
    paste(ifelse(is.na(data$"Molecular Weight"), "No Results", data$"Molecular Weight")), "</p></div>",
    "<div style='padding:10px 0px 10px 0px;'><p><strong>Smiles</strong></p><p>", 
    paste(ifelse(is.na(data$"Isomeric Smiles"), "No Results", data$"Isomeric Smiles")), "</p></div>",
    "<div style='padding:10px 0px 10px 0px;'><p><strong>InchiKey</strong></p><p>", 
    paste(ifelse(is.na(data$InchiKey),  "No Results", data$InchiKey)), "</p></div>",
    "<div style='padding:10px 0px 10px 0px;'><p><strong>Exact Mass", 
    tooltip(bsicons::bs_icon("info-circle", title = " "), "Based on the most abundant isotope of each individual element."), "</strong></p><p>", 
    paste(ifelse(is.na(data$"Exact Mass"), "No Results", paste(round(as.numeric(data$"Exact Mass"), 5), "Da"))), "</p></div>",
    "<div style='padding:10px 0px 10px 0px;'><p><strong>Topological Polar Surface Area</strong></p><p>", 
    paste(ifelse(!is.na(data$TPSA) && data$TPSA != 0, paste(data$TPSA, "Å²"), "No Results")), "</p></div>",
    "<div style='padding:10px 0px 10px 0px;'><p><strong>pKow</strong></p><p>", 
    paste(ifelse(is.na(data$pKow), "No Results", data$pKow)), "</p></div>",
    ifelse(!is.na(data$CID), paste("<div style='padding:10px 0px 10px 0px;'><p><strong>Pubchem</strong></p><a href=\"", 
    paste("https://pubchem.ncbi.nlm.nih.gov/compound/", data$CID, sep=""),"\" target=\"new\">", 
    paste("https://pubchem.ncbi.nlm.nih.gov/compound/", data$CID, sep=""),"</a></div>"), ""),
    sep = ""))
})

#outputs the doi link
output$doi <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  req(length(input$selectedCRM)>0)
  req(v$crms != "No results")
  if(input$selectedCRM=='No results') return('')
  if(v$data$title[1]=='No results') return('')
  if(!is.null(v$doi)) {
    HTML(paste(
      "<div style='padding:10px 0px 10px 0px;'><p><strong>Link to Certificate (DOI)</strong></p><a href=\"", 
      gsub('Resolve DOI:','',v$doi),"\" target=\"new\">", 
      gsub('Resolve DOI:','',v$doi),"</a></div>"))
  }
})

#outputs the publish data for the crm
output$date <- renderUI({ 
  req(length(input$customTable_rows_selected) == 1)
  req(length(input$selectedCRM)>0)
  req(v$crms != "No results")
  if(input$selectedCRM=='No results') return('')
  if(v$data$title[1]=='No results') return('')
  if(!is.null(v$date)) {
    paste('Publication date:',v$date)
  }
})

tableExisits <- reactiveVal(FALSE)

#outputs the analyte table (if it exists)
output$analyteTable <- renderDT({
  req(length(input$customTable_rows_selected) == 1)
  req(length(input$selectedCRM)>0)
  req(v$crms != "No results")
  
  if(identical(input$selectedCRM, character(0))){
    return(data.frame())
  } 
  
  id_txt = v$data$id[v$data$name == input$selectedCRM]
  req(nchar(id_txt)>8)
  id = gsub("urn:uuid:","",v$data$id[v$data$name == input$selectedCRM])
  
  if(identical(id, character(0)) | id=='') {
    return(data.frame())
  }
  
  if(input$selectedCRM=='No results') {
    return(data.frame())
  }
  
  link = paste0('https://nrc-digital-repository.canada.ca/eng/view/object/?id=',id)
  
  #overrides the ssl verifypeer so the webpage can be reached
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = 0)
  ddf = rvest::html_table(html_nodes(read_html(geturl(link, h)),'table'))
  rm(h)

  if(length(ddf)>0){
    dd_1 = data.frame(ddf[1])
    v$doi = dd_1[dd_1[,'X1']=='DOI',2]
    v$abstract = dd_1[dd_1[,'X1']=='Abstract',2]
    v$date = dd_1[dd_1[,'X1']=='Publication date',2]
  }
  
  d = ddf[length(ddf)]
  
  if(grepl('Analyte',paste(d))){
    v$table =  ddf[length(ddf)]
    tableExisits(TRUE)
    return(datatable(data.frame(d))) 
  } else {
    tableExisits(FALSE)
    return(NULL)
  }
})

#download handler for the analyte table download button
output$downloadTable <- downloadHandler(
  filename = function() {
    paste("certificate data.csv")
  },
  content = function(file) {
    write.csv(v$table, file, row.names = FALSE)
  }
)

#ui so that download button is only shown if the analyte table is shown
output$analyteInfoDownload <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  req(length(input$selectedCRM)>0)
  req(v$crms != "No results")
  if (tableExisits()){
    downloadButton("downloadTable", "Download the Table as a .CSV")
  }
})

#displays this message if the analyte choosen does not have crm information (will still show the information pulled form PubChem)
output$noInfo <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  req(req(v$crms == "No results"))
  p("This compound does not have CRM information.")
})

