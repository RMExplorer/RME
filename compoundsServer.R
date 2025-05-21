#function to convert xml to dataframe (Juris function)
xml_to_dataframe <- function(nodeset){
  if(class(nodeset) != 'xml_nodeset'){ stop('Input should be "xml_nodeset" class') }
  lst <- lapply(nodeset, function(x){
    tmp <- xml2::xml_text(xml2::xml_children(x))
    names(tmp) <- xml2::xml_name(xml2::xml_children(x))
    return(as.list(tmp))
  })
  result <- do.call(plyr::rbind.fill, lapply(lst, function(x)
    as.data.frame(x, stringsAsFactors = F)))
  return(tibble::as_tibble(result))
}

#function that returns all the tables saved by a particular user
allTables <- function(username) {
  mongoUserCompounds <- mongo(collection="userCompounds", db="rmeDB", url= connectionLink)
  req(mongoUserCompounds)
  return(mongoUserCompounds$find(query = paste('{"username" : "', username, '"}', sep=""), fields = '{}'))
}

#function to get the specific table from mongodb, given someones username and the name of the table
specificTable <- function(username, tablename) {
  mongoUserCompounds <- mongo(collection="userCompounds", db="rmeDB", url= connectionLink)
  req(mongoUserCompounds)
  return(mongoUserCompounds$find(query = paste('{"username" : "', username, '", "tablename" : "', tablename, '"}', sep=""), fields = '{}'))
}

#inserts a new table or updates (by deleting, then inserting) the table saved by a user
insertTable <- function(username, tableValues, tablename){
  mongoUserCompounds <- mongo(collection="userCompounds", db="rmeDB", url= connectionLink)
  req(mongoUserCompounds)
  
  #check if the table already exists for the user & delete it if it does
  mongoUserCompounds$remove(query = paste('{"username" : "', username, '", "tablename" : "', tablename, '"}', sep=""))
  
  arrayString <- "["
  #convert the list of values into a quoted + comma seperated list
  for (i in 1:length(tableValues)){
    arrayString <- paste(arrayString, '"', tableValues[[i]], '",', sep="")
  }
  #remove the last comma added
  arrayString <- substr(arrayString, 1, nchar(arrayString)-1)
  #add the closing brace
  arrayString <- paste(arrayString, "]", sep="")
  
  #make the values into json string in order to insert it into the DB
  tableVals <- c(paste('{"username": "', username, '",',
                       '"table": ', arrayString, ",",
                       '"tablename": "', tablename, '"}', sep=""))
  
  #add it to mongodb
  mongoUserCompounds$insert(tableVals)
}

#overrides the ssl verifypeer so the webpage can be reached
h <- curl::new_handle()
curl::handle_setopt(h, ssl_verifypeer = 0)
tryCatch({
  d = xml_children(read_xml(geturl('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=*&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1', h)))
},
error = function(cond) {
  message(conditionMessage(cond))
  output$urlerror <- renderText({
    "We are currently unable to access the nrc digital repository"
  })
  NA
},
warning = function(cond) {
  message(conditionMessage(cond))
  output$urlerror <- renderText({
    "We are currently unable to access the nrc digital repository"
  })
  NULL
})

rm(h)

nrc_dr_all = xml_to_dataframe(d)[-1,-c(1,2)]
nrc_dr_all$name = sapply(str_split(nrc_dr_all$title,":"), function(x) x[1])
nrc_dr_all = nrc_dr_all[!is.na(nrc_dr_all$title),]

crms = sort(nrc_dr_all$name)
names(crms) = crms

#function that takes the name of a analyte and runs it through PubChem to gather information and return a dataframe
getTableData <- ExtendedTask$new(function(analytes){
  future_promise({
    data <- data.frame()
    if (length(analytes) > 0) {
      props <- c()
      data <- c()
      
      #for each analyte, get the pubchem info
      for (i in 1:length(analytes)){
        compoundName <- ""
        #if the search term is an inchikey
        if (is.inchikey(analytes[[i]])){
          props <- get_properties(
            properties = c("smiles",
                           "inchikey",
                           "MolecularFormula", 
                           "MolecularWeight", 
                           "ExactMass", 
                           "TPSA", 
                           "XLogP"),
            identifier = analytes[[i]],
            namespace = "inchikey",
            propertyMatch = list(
              .ignore.case = TRUE,
              type = "contain"
            )
          )
          
          synonymsLink <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", 
                                retrieve(object = props, .which = analytes[[i]], .to.data.frame = TRUE)$CID,
                                "/synonyms/JSON", sep="")
          
          compoundName <- tryCatch({
            fromJSON(synonymsLink)$InformationList$Information$Synonym[[1]][1]
          }, error = function(e) {
            return(analytes[[i]])
          })
          
        } else {
          #assumes that if the analyte isn't an inchikey, it is a name, and searches that in pubchem
          props <- get_properties(
            properties = c("smiles",
                           "inchikey",
                           "MolecularFormula", 
                           "MolecularWeight", 
                           "ExactMass", 
                           "TPSA", 
                           "XLogP"),
            identifier = analytes[[i]],
            namespace = "name",
            propertyMatch = list(
              .ignore.case = TRUE,
              type = "contain"
            )
          )
        }
        #contains the info from PubChem
        info <- retrieve(object = props, .which = analytes[[i]], .to.data.frame = TRUE)
        
        #will search the repository with the name/inchikey the analyte was searched with
        link = paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=',
                      gsub(' ','+', analytes[[i]]), '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
        ids <- ""
        
        #overrides the ssl verifypeer so the webpage can be reached on shinyapps
        h <- curl::new_handle()
        curl::handle_setopt(h, ssl_verifypeer = 0)
        d = xml_children(read_xml(geturl(link, h)))
        rm(h)
        req(d)
        
        df = xml_to_dataframe(d)[-1,-c(1,2)]
        #if the previous search in the repository for the inchikey stored in yourtableanalytes did not work, search with the value in the inchikey column
        if (df$title[1] == "No results" && (length(info[["InChIKey"]]) > 0)) {
          searchIds <- gsub(" ", "+", info[["InChIKey"]])
          searchIds <- gsub("/", "%2F", searchIds)
          link <- paste('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=&q=',
                        searchIds, '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1', sep="")
          
          h <- curl::new_handle()
          curl::handle_setopt(h, ssl_verifypeer = 0)
          d = xml_children(read_xml(geturl(link, h)))
          rm(h)
          req(d)
          df = xml_to_dataframe(d)[-1,-c(1,2)]
        }
        
        df$name = sapply(str_split(df$title,":"), function(x) x[1])
        df = df[!is.na(df$title),]
        crms = sort(df$name)
        titles = sort(df$title)
        names(crms) = crms
        
        #initializing the mass fraction and concentration of the compound 
        minMassFraction <- 999999
        maxMassFraction <- 0
        
        minMassConc <- 999999
        maxMassConc <- 0
        
        for (crm in crms) {
          if(crm != "No results"){
            #find the min/max mass concentration and fraction
            #search repository for id
            recordRow <- recordDF[recordDF$crm %in% crm,]
            id <- recordRow$id
            req(id)
            
            #use id to get a link to the digital repository entry
            link <- paste("https://nrc-digital-repository.canada.ca/eng/view/object/?id=", id, sep="")
            
            #use doi content to get information (title, abstract, table, doi)
            #overrides the ssl verifypeer so the webpage can be reached
            h <- curl::new_handle()
            curl::handle_setopt(h, ssl_verifypeer = 0)
            ddf = rvest::html_table(html_nodes(read_html(geturl(link, h)),'table'))
            rm(h)
            #sets analyte table data to null, unless crm contains analyte table
            analyteTable <- NULL
            if(length(ddf) >= 3 & grepl('Analyte',paste(ddf[3]))){
              analyteTable <- data.frame(ddf[[3]])
            }
            
            #read into the analyte table as long as it's not empty and the compound has a molecular weight available from Pubchem
            if (!is.null(analyteTable)) {
              massFrac <- as.numeric(analyteTable$Value[(grepl(ifelse(nchar(compoundName) > 0, compoundName, analytes[[i]]), analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass fraction", analyteTable$Quantity, ignore.case = TRUE)])
              units <- analyteTable$Unit[(grepl(ifelse(nchar(compoundName) > 0, compoundName, analytes[[i]]), analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass fraction", analyteTable$Quantity, ignore.case = TRUE)]
              #remove NAs
              units <- units[!is.na(massFrac)]
              massFrac <- massFrac[!is.na(massFrac)]
              
              #convert units to µg/g (note: mg/kg is equivalent to µg/g so it is not converted)
              if (length(units) > 0 && length(massFrac) > 0) {
                for (l in 1:length(units)){
                  if (units[[l]] == "mg/g") { massFrac[[l]] <- 1000 * massFrac[[l]]} 
                  else if (units[[l]] == "µg/kg") {massFrac[[l]] <- massFrac[[l]] / 1000} 
                  else if (units[[l]] == "g/g") {massFrac[[l]] <- 1000000 * massFrac[[l]]}
                  else if (units[[l]] == "pg/g") {massFrac[[l]] <- massFrac[[l]] / 1000000}
                  else if (units[[l]] == "ng/g") {massFrac[[l]] <- massFrac[[l]] / 1000}
                  else if (units[[l]] == "kg/kg") {massFrac[[l]] <- 1000000 * massFrac[[l]]}
                  else if (units[[l]] == "g/kg") {massFrac[[l]] <- 1000 * massFrac[[l]]}
                }
              }
              
              if (length(massFrac) > 0 && min(massFrac) < minMassFraction) {
                minMassFraction <- min(massFrac)
              }
              
              if (length(massFrac) > 0 && max(massFrac) > maxMassFraction) {
                maxMassFraction <- max(massFrac)
              }
              
              massConc <- as.numeric(analyteTable$Value[(grepl(ifelse(nchar(compoundName) > 0, compoundName, analytes[[i]]), analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass concentration", analyteTable$Quantity, ignore.case = TRUE)])
              units <- analyteTable$Unit[(grepl(ifelse(nchar(compoundName) > 0, compoundName, analytes[[i]]), analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass concentration", analyteTable$Quantity, ignore.case = TRUE)]
              #remove NAs
              units <- units[!is.na(massConc)]
              massConc <- massConc[!is.na(massConc)]
              
              #converting units to µg/mL (which is equivalent to mg/kg and mg/L )
              if (length(units) > 0 && length(massConc) > 0) {
                for (l in 1:length(units)){
                  if (units[[l]] == "µg/L") { massConc[[l]] = massConc[[l]] / 1000} 
                  else if (units[[l]] == "mg/mL") {massConc[[l]] = massConc[[l]] * 1000}
                  else if (units[[l]] == "g/mL") {massConc[[l]] = massConc[[l]] * 1000000}
                }
              }
              
              
              if (length(massConc) > 0 && min(massConc) < minMassConc) {
                minMassConc <- min(massConc)
              }
              
              if (length(massConc) > 0 && max(massConc) > maxMassConc) {
                maxMassConc <- max(massConc)
              }
            }
            
          } 
        }
        
        #if the min mass fraction/concentration were not changed
        if (minMassFraction == 999999) {
          minMassFraction <- 0
        }
        
        if (minMassConc == 999999) {
          minMassConc <- 0
        }
        
        #create the datarow with all the pubchem info
        dataRow <- c(
          ifelse(length(compoundName) > 0 && nchar(compoundName) > 0, compoundName, analytes[[i]]), 
          ifelse(length(info[["CID"]]) != 0, info[["CID"]], NA), 
          ifelse(length(info[["MolecularFormula"]]) != 0, info[["MolecularFormula"]], NA), 
          ifelse(length(info[["MolecularWeight"]]) != 0, info[["MolecularWeight"]], NA), 
          ifelse(length(info[["IsomericSMILES"]]) != 0, info[["IsomericSMILES"]], NA), 
          ifelse(length(info[["InChIKey"]]) != 0, info[["InChIKey"]], NA), 
          ifelse(length(info[["XLogP"]]) != 0, info[["XLogP"]] * -1, NA), 
          ifelse(length(info[["ExactMass"]]) != 0, info[["ExactMass"]], NA), 
          ifelse(length(info[["TPSA"]]) != 0, info[["TPSA"]], NA),
          ifelse(length(crms) != 0, paste(crms, collapse = ","), NA),
          minMassFraction, maxMassFraction, minMassConc, maxMassConc
        )
        
        #ifelse(length(crms) != 0, paste(crmHTML, collapse=", "), NA)
        #add the crm column to the table row
        data <- rbind(data, dataRow)
      }
      
      colnames(data) <- c("Name", "CID", "Molecular Formula", 
                          "Molecular Weight", "Isomeric Smiles", 
                          "InchiKey", "pKow", "Exact Mass", "TPSA", "CRMs", "Minimum Mass Fraction (µg/g)", 
                          "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
                          "Maximum Mass Concentration (µg/mL)")
      
      #find all the common crms
      allcrms <- data[, "CRMs"]
      commonCrms <- allCrms
      for (i in 1:length(data)) {
        for (crmRow in allcrms){
          crmvec <- as.vector(strsplit(crmRow, ",")[[1]])
          commonCrms <- intersect(commonCrms, crmvec)
        }
      }
      
      
      crmHTMLCol <- c()
      
      for (crmRow in allcrms){
        crmHTML <- c()
        crmvec <- as.vector(strsplit(crmRow, ",")[[1]])
        for (crm in crmvec) {
          if(crm != "No results"){
            #html for crm column (adding the link for the modal)
            if (crm %in% commonCrms){
              crmHTML <- paste(crmHTML, HTML(paste0(
                '<a href="#" class="view-info2" data-name="', crm,'" style="color:red">', crm, "</a>",sep="")))
            } else {
              crmHTML <- paste(crmHTML, HTML(paste0(
                '<a href="#" class="view-info2" data-name="', crm,'">', crm, "</a>",sep="")))
            }
          } else {
            crmHTML <- paste(crmHTML, HTML(paste0(
              "<p>", crm, "</p>",sep="")))
          }
        }
        crmHTMLCol <- c(crmHTMLCol, crmHTML)
      }
      
      data <- cbind(data, crmHTMLCol)
      
      data <- as.data.frame(data)
      
      colnames(data) <- c("Name", "CID", "Molecular Formula", 
                          "Molecular Weight", "Isomeric Smiles", 
                          "InchiKey", "pKow", "Exact Mass", "TPSA", 
                          "CRMs", "Minimum Mass Fraction (µg/g)", 
                          "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
                          "Maximum Mass Concentration (µg/mL)", "Reference Materials")
      
    }
    
    row.names(data) <- NULL
    return(data)
    
  }, seed = TRUE)
})

#the analytes shown in the select analye drop down menu
analytes <- function() {
  link <- 'https://nrc-digital-repository.canada.ca/eng/search/atom/?q=*&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1'
  
  #overrides the ssl verifypeer so the webpage can be reached
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = 0)
  
  #get all the unique category names and remove certain terms
  names <- read_html(geturl(link, h)) %>% xml_find_all(xpath="//category") %>% xml_attr("term")
  names <- unique(names)
  rm(h)
  
  return(names)
}

#observes for changes in the your table analyte list of all the substances and invokes getTableData when it changes
observeEvent(yourTableAnalytes(), {
  shinyjs::hide("customTable")
  getTableData$invoke(yourTableAnalytes())
  shinyjs::show("customTable")
})

#stores the info for the selected row
v <- reactiveValues('data' = nrc_dr_all, 
                    'crms' = crms, 'doi' = NULL, 
                    'abstract' = NULL, 
                    'date' = NULL, 
                    'table' = NULL, 
                    spectralData = NULL)

#change this with google sheets data for the logged in user
yourTableAnalytes <- reactiveVal(list("Domoic Acid", "Azaspiracid-1", "Azaspiracid-2"))

#the 'Select an Analyte' drop down in the 'Substances' page which also allows users to enter their own analyte names
output$searchAnalyte <- renderUI({
  selectizeInput(
    inputId = "selectAnalyte", 
    div("Search NRC Repository",style="display:flex;"), 
    choices = append("", analytes()), 
    selected = "", 
    options = list(create = TRUE, delimiter=';'),
    width="350px"
  )
})

# function that takes in a doi and extracts the mass spectrum file from it
getCsvFile <- function(id, type) {
  selectedlink <- paste('https://nrc-digital-repository.canada.ca/eng/view/object/?id=', gsub("urn:uuid:", "", id), sep="")
  if (!is.null(selectedlink) & length(selectedlink) != 0 & any(grepl("(https?|ftp)://[^ /$.?#].[^\\s]*" , selectedlink))) {
    #overrides the ssl verifypeer so the webpage can be reached
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    htmlOutput <- read_html(geturl(selectedlink, h))
    rm(h)
    
    #gets all the links
    linksHtml <- htmlOutput %>% xml_find_all(xpath='//a')
    
    #find the link containing the text given in the 2nd parameter
    linkHtml <- linksHtml[grepl(type, xml_text(linksHtml), ignore.case = TRUE)]
    if (length(linkHtml) > 0){
      link <- list()
      for (href in linkHtml){
        link <- append(xml_attr(href, "href"), link)
      }
      
      #gets the csv file from the url
      #data <- data.frame(read.csv(url(link), header=F))
      return(link)
    } else {
      return (NULL)
    }
  }
}

#when a row from the table is clicked, gathers all the information necessary for the 'Properties' and 'Spectral Data' page
observeEvent(input$customTable_rows_selected, {
  if (length(input$customTable_rows_selected) == 1) {
    req(length(getTableData$result()) > 0)
    result <- getTableData$result()
    row = result %>% slice(input$customTable_rows_selected)
    link = paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=',
                  gsub(' ','+', row$Name) , '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')

    #overrides the ssl verifypeer so the webpage can be reached
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    xmlDoc <- read_xml(geturl(link, h))
    rm(h)

    d = xml_children(xmlDoc)

    df = xml_to_dataframe(d)[-1,-c(1,2)]
    
    #if searching the name column in DR resulted in nothing, then search the inchikey
    if (df$title[1] == "No results" && length(row$InchiKey) > 0) {
      link <- paste('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=',
                    row$InchiKey, '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1', sep="")
      
      h <- curl::new_handle()
      curl::handle_setopt(h, ssl_verifypeer = 0)
      d = xml_children(read_xml(geturl(link, h)))
      rm(h)
      
      df = xml_to_dataframe(d)[-1,-c(1,2)]
      
    }

    #get all the mass spectrum dataset link
    entries <- xmlDoc %>% xml_ns_strip() %>% xml_find_all(xpath="//entry")
    hasData <- c()
    dataNames <- c()
    spectralData <- c()

    #for each entry/certificate, add it's id (NA if it doesn't have one) to spectralData
    for (i in 1:length(entries)){
      id <- entries[i] %>% xml_find_first(xpath="./id") %>% xml_text()
      spectralLink <- xml_attr(entries[i] %>% xml_find_all(xpath="./link[contains(@title, 'Download dataset')]"), "href")
      dataNames <- rbind(dataNames, ifelse(length(spectralLink) > 0, 
                                           str_split(entries[i] %>% xml_find_first(xpath="./title") %>% xml_text(), ":")[[1]][1], 
                                           NA))
      hasData <- rbind(hasData, ifelse(length(spectralLink) > 0, id, NA))
    }

    #finds spectral data and adds the link to the list
    for (i in 1:length(hasData)){
      if (!is.na(hasData[[i]])) {
        links <- getCsvFile(hasData[[i]], "full scan MS|fullscan|full scan")
        if (!is.null(links)) {
          for (link in links){
            #read the file from the link to get the data type and the substance name
            tmpfile <- tempfile(fileext = ".csv")
            curl::curl_download(link, tmpfile, handle = curl::new_handle(ssl_verifypeer = FALSE))
            data <- data.frame(read.csv(tmpfile))[1:20,]
            
            #check if the inchikey matches (or name), and if it does, add it to the spectral dropdown
            inchikey <- data[grep("inchikey", data[,1], ignore.case = TRUE), 2]
            substanceName <- trimws(data[grep("substance", data[,1], ignore.case = TRUE), 2])
            if (trimws(inchikey) == ifelse(is.null(trimws(row$InchiKey)), trimws(row$InchiKey), "") | grepl(substanceName, row$Name, ignore.case = TRUE)) {
              dataType <- ifelse(length(data[grep("Type of Data", data[,1], ignore.case = TRUE), 2]) > 0, 
                                 data[grep("Type of Data", data[,1], ignore.case = TRUE), 2], "mass spectrum")
              substance <- data[grep("Substance", data[,1], ignore.case = TRUE), 2]
              spectralData <- rbind(spectralData, c(paste(dataNames[[i]], ", ", dataType, ", ", substance), link))
            }
          }
        }
        links <- getCsvFile(hasData[[i]], "nmr|1H-NMR")
        if (!is.null(links)) {
          for (link in links){
            #read the file from the link to get the data type and the substance name
            tmpfile <- tempfile(fileext = ".csv")
            curl::curl_download(link, tmpfile, handle = curl::new_handle(ssl_verifypeer = FALSE))
            data <- data.frame(read.csv(tmpfile))[1:20,]
            
            #check if the inchikey matches (or name), and if it does, add it to the spectral dropdown
            inchikey <- data[grep("inchikey", data[,1], ignore.case = TRUE), 2]
            substanceName <- trimws(data[grep("substance", data[,1], ignore.case = TRUE), 2])
            if (trimws(inchikey) == ifelse(is.null(trimws(row$InchiKey)), trimws(row$InchiKey), "") | grepl(substanceName, row$Name, ignore.case = TRUE)) {
              dataType <- ifelse(length(data[grep("Type of Data", data[,1], ignore.case = TRUE), 2]) > 0, 
                                 data[grep("Type of Data", data[,1], ignore.case = TRUE), 2], "NMR")
              substance <- data[grep("Substance", data[,1], ignore.case = TRUE), 2]
              spectralData <- rbind(spectralData, c(paste(dataNames[[i]], ",", paste(dataType), ", ", substance), link))
            }
          }
        }
        
        links <- getCsvFile(hasData[[i]], "MS/MS|MSMS")
        if (!is.null(links)) {
          for (link in links){
            #read the file from the link to get the data type and the substance name
            tmpfile <- tempfile(fileext = ".csv")
            curl::curl_download(link, tmpfile, handle = curl::new_handle(ssl_verifypeer = FALSE))
            data <- data.frame(read.csv(tmpfile))[1:20,]
            
            #check if the inchikey matches (or name), and if it does, add it to the spectral dropdown
            inchikey <- data[grep("inchikey", data[,1], ignore.case = TRUE), 2]
            substanceName <- trimws(data[grep("substance", data[,1], ignore.case = TRUE), 2])
            if (trimws(inchikey) == ifelse(is.null(trimws(row$InchiKey)), trimws(row$InchiKey), "") | grepl(substanceName, row$Name, ignore.case = TRUE)) {
              dataType <- ifelse(length(data[grep("Type of Data", data[,1], ignore.case = TRUE), 2]) > 0, 
                                 data[grep("Type of Data", data[,1], ignore.case = TRUE), 2], "MS/MS")
              substance <- data[grep("Substance", data[,1], ignore.case = TRUE), 2]
              spectralData <- rbind(spectralData, c(paste(dataNames[[i]], ", ", dataType, ", ", substance), link))
            }
          }
        }
      }
    }

    if (length(spectralData) > 0){
      colnames(spectralData) <- c("Name", "SpectralLink")
    }
  
    #for the selected analyte, save its name crm information in a reactive variable
    df$name = sapply(str_split(df$title,":"), function(x) x[1])
    df = df[!is.na(df$title),]
    v$data = df
    crms = sort(df$name)
    names(crms) = crms
    v$crms = crms
    v$spectralData <- as.data.frame(spectralData)
  }
})

#when a new analyte is selected, add it to the table list
observeEvent(
  eventExpr = {
    input$selectAnalyte
  }, 
  handlerExpr = {
    if (nchar(input$selectAnalyte) > 0 && !(input$selectAnalyte %in% yourTableAnalytes())){
      if (input$additiveTable) {
        newList <- append(input$selectAnalyte, yourTableAnalytes())
      } else {
        newList <- list(input$selectAnalyte)
      }
      
      yourTableAnalytes(newList)
    }
})

#when the 'Remove Selected Rows...' button is clicked and rows are selected, remove the names from the reactive variable yourTableAnalytes
observeEvent(input$removeAnalyte, {
  req(input$customTable_rows_selected)
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  selected <- result %>% slice(input$customTable_rows_selected)
  names <- selected$Name
  
  newList <- yourTableAnalytes()
  newList <- newList[!newList %in% names]
  yourTableAnalytes(newList)
})

#when the 'Remove All Rows...' button is clicked, remove all the names from the reactive variable yourTableAnalytes
observeEvent(input$removeAllAnalytes, {
  #removes any row selection
  selectRows(proxy = dataTableProxy("customTable", session = session), 
             selected = NULL)
  
  #empties the list
  newList <- list()
  yourTableAnalytes(newList)
})

#when the 'Save All Rows...' button is clicked and user is logged in, saves all the names from the reactive variable yourTableAnalytes to mongodb
observeEvent(input$saveAnalytes, {
  #modal asking user to name/reuse a name for their saved table
  showModal(
    modalDialog(
      title = "Save Your Table",
      downloadButton("downloadSavedSubstances", "Save", color="success"),
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
})

#saves the choosen in mongodb
output$downloadSavedSubstances <- downloadHandler(
  filename = function() {
    paste("yourSubstances.csv")
  },
  content = function(file) {
    write.csv(getTableData$result()$Name, file, row.names = FALSE)
  }
)

#when the 'Load All Rows...' button is clicked and user is logged in, loads all the names to the reactive variable yourTableAnalytes from mongodb
observeEvent(input$loadAnalytes, {
  showModal(
    modalDialog(
      title = "Load Your Table",
      fileInput("uploadSubstances", NULL, buttonLabel = "Upload...", accept = ".csv"),
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
})


#loads the choosen table from mongodb when the load button is clicked
observeEvent(input$uploadSubstances, {
  if (length(input$uploadSubstances) > 0) {
    data <- read.csv(input$uploadSubstances$datapath, header = TRUE)$x
    data <- as.list(data)
    req(data)
    yourTableAnalytes(data)
    #removes the modal
    removeModal() 
  }
})

#loads the data table in the 'Compounds' page using the gettabledata result
output$customTable <- renderDT({
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  data <- result[, c("Name", 
                                "Molecular Formula", 
                                "Molecular Weight", 
                                "pKow", 
                                "Reference Materials", "Minimum Mass Fraction (µg/g)", 
                                "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
                                "Maximum Mass Concentration (µg/mL)")]
  
  data <- data.frame("Name" = data$Name, 
                     "Molecular Formula" = data$"Molecular Formula", 
                     "Molecular Weight" = as.numeric(data$"Molecular Weight"), 
                     "pKow" = as.numeric(data$"pKow"),
                     "Reference Materials" = data$"Reference Materials", 
                     "Minimum Mass Fraction (µg/g)" = as.numeric(data$"Minimum Mass Fraction (µg/g)"), 
                     "Maximum Mass Fraction (µg/g)" = as.numeric(data$"Maximum Mass Fraction (µg/g)"), 
                     "Minimum Mass Concentration (µg/mL)" = as.numeric(data$"Minimum Mass Concentration (µg/mL)"), 
                     "Maximum Mass Concentration (µg/mL)" = as.numeric(data$"Maximum Mass Concentration (µg/mL)"))
  
  colnames(data) <- c("Name", "Molecular Formula", "Molecular Weight", "pKow", "Reference Materials", "Minimum Mass Fraction (µg/g)", 
                      "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
                      "Maximum Mass Concentration (µg/mL)")
  
  datatable(data, 
            options = list(pageLength = 10, responsive = FALSE, scrollX = TRUE), 
            filter= list(position='top', clear = FALSE), escape = FALSE)
})

#unselect all rows in the custom table when unselect button is clicked
observeEvent(input$unselect, {
  selectRows(proxy = dataTableProxy("customTable", session = session), 
             selected = NULL)
})

observeEvent(input$addallSubstances, {
  #hdie the table to trigger loading image
  shinyjs::hide("customTable")
  
  #get all crm ids
  ids <- recordDF$id
  allNames <- c()
  
  #get the analyte table of all crms
  for (id in ids) {
    #use id to get a link to the digital repository entry
    link <- paste("https://nrc-digital-repository.canada.ca/eng/view/object/?id=", id, sep="")
    
    #use doi content to get information (the analyte names)
    #overrides the ssl verifypeer so the webpage can be reached
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    ddf = rvest::html_table(html_nodes(read_html(geturl(link, h)),'table'))
    rm(h)
    #sets analyte table data to null, unless crm contains analyte table
    analyteTable <- NULL
    if(length(ddf) >= 3 & grepl('Analyte',paste(ddf[3]))){
      analyteTable <- data.frame(ddf[[3]])
    }
    if (!is.null(analyteTable)){
      allNames <- append(analyteTable$Analyte, allNames)
    }
  }
  
  #list of all unique values in the analyte column of the analyte tables
  allNames <- unique(allNames)
  
  #removes any row selection
  selectRows(proxy = dataTableProxy("customTable", session = session), 
             selected = NULL)
  
  #add the unique susbtances to the yourtableanalytes list
  newList <- append(allNames, yourTableAnalytes())
  yourTableAnalytes(newList)
})