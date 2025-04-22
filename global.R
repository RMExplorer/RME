library(bslib)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(rcdk)
library(rinchi)
library(PubChemR)                   #for pubchem
library(shinyauthr)                 #for login/logout functionality
library(rvest)                      #used to pull data from DOI
library(xml2)                       #for parsing xml
library(purrr)
library(ggspectra)                  #an extension of ggplot, used to label peaks in graphs
library(shinyjs)                    #to use js code easier with shiny
library(googlesheets4)              #to access the google sheets
library(googledrive)                #to access the files stored in google drive
library(httr)                       #for doi access
library(bsicons)                    #for icons
library(parallel)                   #in order to do parallel web scraping
library(jsonlite)                   #to work with json content
library(webchem)
library(mongolite)
library(future)                     #in order to run long processes
library(promises)                   #in order to run long processes
future::plan(multisession)

#types of spectrums
types <- c("NMR", "FSMS", "MSMS")

connectionLink <- "mongodb+srv://rmexplorerdata:djGC7HZnz2et455e@usercompounds.ae2s6.mongodb.net/?retryWrites=true&w=majority&appName=UserCompounds"

#method to get url content (used to bypass ssl)
geturl <- function(url, handle) {
  curl::curl_fetch_memory(url, handle = handle)$content  
}

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


########### START OF GETTING ALL OAI AFFILIATE INFO ###########
#### Get all crms from oai ######
link <-"https://oai-pmh.nrc-cnrc.gc.ca/dr-dn?verb=ListRecords&metadataPrefix=oai_openaire&set=crm"
h <- curl::new_handle()
curl::handle_setopt(h, ssl_verifypeer = 0)
oaiHTML <- read_html(geturl(link, h))
rm(h)

#root node to store all the records
records <- xml_new_root("allRecords")
#get the first page of records from oai
pageNodes <- c(oaiHTML %>% xml_find_all(xpath="//record"))
#add each node from the first page into the created root node 'records'
for (node in pageNodes){
  xml_add_child(records, node)
}

#add the html from all pages found for the OAI query into the records root node
while (nchar(oaiHTML %>% xml_find_all(xpath="//resumptiontoken") %>% xml_text()) > 0) {
  token <- oaiHTML %>% xml_find_all(xpath="//resumptiontoken") %>% xml_text()
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = 0)
  oaiHTML <- read_html(geturl(paste("https://oai-pmh.nrc-cnrc.gc.ca/dr-dn?verb=ListRecords&resumptionToken=", token, sep=""), h))
  rm(h)
  
  #get all the records on the page
  pageNodes <- oaiHTML %>% xml_find_all(xpath="//record")
  #add the records to the root node that was created (called records)
  for (node in pageNodes){
    xml_add_child(records, node)
  }
}

#get all the record nodes from the the 'records' root node
records <- records  %>% xml_find_all(xpath="//record")
#data frame to store the id of the records + all the affiliates for that record
recordDF <- c()
#vector to store all the unique affiliates gotten from all the records
allAffiliates <- c()
allCrms <- c()
allMaterials <- c()
for (i in 1:length(records)){
  id <- gsub("oai:dr-dn.cisti-icist.nrc-cnrc.ca:", "", records[i] %>% xml_find_first(xpath=".//identifier") %>% xml_text())
  affiliation <- records[i] %>% xml_find_all(xpath=".//affiliation") %>% xml_text()
  title <- records[i] %>% xml_find_first(xpath='.//title') %>% xml_text()
  title <- ifelse(length(str_split(title,":")[[1]][1]) > 0, str_split(title,":")[[1]][1], "")
  materialType <- records[i] %>% xml_find_all(xpath='.//description') %>% xml_text()
  materialType <- materialType[grepl("Material type", materialType)]
  materialType <- ifelse(length(str_split(materialType,":")[[1]][2]) > 0, str_split(materialType,":")[[1]][2] %>% trimws, "")
  recordDF <- rbind(recordDF, c(id, paste(unique(affiliation), collapse="; "), title, materialType))
  allCrms <- append(allCrms, title)
  allAffiliates <- append(allAffiliates, affiliation)
  allMaterials <- append(allMaterials, materialType)
}
colnames(recordDF) <- c("id", "affiliation", "crm", "materialType")
recordDF <- as.data.frame(recordDF)
######### END ######### 

affiliates <- as.list(unique(allAffiliates))
materials <- as.list(unique(allMaterials))



now <- Sys.time()
#get all crm ids
ids <- recordDF$id
allNames <- c()
allSubstanceData <- data.frame()
#get the analyte table of all crms
for (id in ids) {
  #use id to get a link to the digital repository entry
  link <- paste("https://nrc-digital-repository.canada.ca/eng/view/object/?id=", id, sep="")
  
  #use doi content to get information (the analyte names)
  #overrides the ssl verifypeer so the webpage can be reached
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = 0)
  ddf = rvest::html_table(html_elements(read_html(geturl(link, h)),'table'))
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

#list of all unique values in the analyte column of the analyte tables (= ALL SUBSTANCES)
allNames <- unique(allNames)
print(Sys.time() - now)

now <- Sys.time()
for (name in allNames){
  #get info from PubChem
  props <- get_properties(
    properties = c("smiles",
                   "inchikey",
                   "MolecularFormula", 
                   "MolecularWeight", 
                   "ExactMass", 
                   "TPSA", 
                   "XLogP"),
    identifier = name,
    namespace = "name",
    propertyMatch = list(
      .ignore.case = TRUE,
      type = "contain"
    )
  )
  #contains the info from PubChem
  info <- retrieve(object = props, .which = name, .to.data.frame = TRUE)
  
  #will search the repository with the name/inchikey the analyte was searched with
  link = paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=',
                gsub(' ','+', name), '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
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
        massFrac <- as.numeric(analyteTable$Value[(grepl(name, analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass fraction", analyteTable$Quantity, ignore.case = TRUE)])
        units <- analyteTable$Unit[(grepl(name, analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass fraction", analyteTable$Quantity, ignore.case = TRUE)]
        #remove NAs
        units <- units[!is.na(massFrac)]
        massFrac <- massFrac[!is.na(massFrac)]
        
        #convert units to µg/g (note: mg/kg is equivalent to µg/g so it is not converted)
        if (length(units) == 1 && length(massFrac) > 0) {
          if (units == "mg/g") {massFrac <- 1000 * massFrac} 
          else if (units == "µg/kg") {massFrac <- 1000 * massFrac} 
          else if (units == "g/g") {massFrac <- 1000000 * massFrac}
          else if (units == "pg/g") {massFrac <- massFrac / 1000000}
          else if (units == "ng/g") {massFrac <- massFrac / 1000}
          else if (units == "kg/kg") {massFrac <- 1000000 * massFrac}
          else if (units == "g/kg") {massFrac <- 1000 * massFrac}
        } else if (length(units) > 1 && length(massFrac) > 0) {
          for (l in 1:length(units)){
            if (units[[l]] == "mg/g") { massFrac[[l]] <- 1000 * massFrac[[l]]} 
            else if (units[[l]] == "µg/kg") {massFrac[[l]] <- 1000 * massFrac[[l]]} 
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
        
        massConc <- as.numeric(analyteTable$Value[(grepl(name, analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass concentration", analyteTable$Quantity, ignore.case = TRUE)])
        units <- analyteTable$Unit[(grepl(name, analyteTable$Analyte, ignore.case = TRUE)) & grepl("mass concentration", analyteTable$Quantity, ignore.case = TRUE)]
        #remove NAs
        massConc <- massConc[!is.na(massConc)]
        
        #converting units to µg/mL (which is equivalent to mg/kg and mg/L )
        if (length(units) == 1 && length(massConc) > 0) {
          if (units == "µg/L") {massConc = massConc/1000}
          else if (units == "mg/mL") {massConc = massConc * 1000}
          else if (units == "g/mL") {massConc = massConc * 1000000}
          
        } else if (length(units) > 1 && length(massConc) > 0) {
          for (l in 1:length(units)){
            if (units[[l]] == "µg/L") { massConc[[l]] = massConc[[l]] / 1000} 
            else if (units[[l]] == "mg/mL") {massConc[[l]] = massConc[[l]] * 1000}
            else if (units[[l]] == "g/mL") {massConc[[l]] = massConc[[l]] * 1000000}
          }
        }
        
        if (suppressWarnings(min(massConc)) < minMassConc) {
          minMassConc <- suppressWarnings(min(massConc))
        }
        
        if (suppressWarnings(max(massConc)) > maxMassConc) {
          maxMassConc <- suppressWarnings(max(massConc))
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
  }
    
  #create the datarow with all the pubchem info
  dataRow <- c(
    name, 
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
  allSubstanceData <- rbind(allSubstanceData, dataRow)
  
}
  
colnames(allSubstanceData) <- c("Name", "CID", "Molecular Formula", 
                    "Molecular Weight", "Isomeric Smiles", 
                    "InchiKey", "pKow", "Exact Mass", "TPSA", "CRMs", "Minimum Mass Fraction (µg/g)", 
                    "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
                    "Maximum Mass Concentration (µg/mL)")

print(Sys.time() - now)

