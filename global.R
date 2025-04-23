library(xml2)
library(stringr)

#types of spectrums
types <- c("NMR", "FSMS", "MSMS")

#method to get url content (used to bypass ssl)
geturl <- function(url, handle) {
  curl::curl_fetch_memory(url, handle = handle)$content  
}

########### START OF GETTING ALL OAI AFFILIATE INFO ###########
#### Get all crms from oai ######
link <-"https://oai-pmh.nrc-cnrc.gc.ca/dr-dn?verb=ListRecords&metadataPrefix=oai_openaire&set=crm"
h <- curl::new_handle()
curl::handle_setopt(h, ssl_verifypeer = 0)
oaiXML <- read_xml(geturl(link, h)) %>% xml_ns_strip()
rm(h)

#root node to store all the records
records <- xml_new_root("allRecords")
#get the first page of records from oai
pageNodes <- c(oaiXML %>% xml_find_all(xpath="//record"))
#add each node from the first page into the created root node 'records'
for (node in pageNodes){
  xml_add_child(records, node)
}

#add the html from all pages found for the OAI query into the records root node
while (nchar(oaiXML %>% xml_find_all(xpath="//resumptionToken") %>% xml_text()) > 0) {
  token <- oaiXML %>% xml_find_all(xpath="//resumptionToken") %>% xml_text()
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = 0)
  oaiXML <- read_xml(geturl(paste("https://oai-pmh.nrc-cnrc.gc.ca/dr-dn?verb=ListRecords&resumptionToken=", token, sep=""), h)) %>% xml_ns_strip()
  rm(h)
  #get all the records on the page
  pageNodes <- oaiXML %>% xml_find_all(xpath="//record")
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
  affiliation <- records[i] %>% xml_find_all(xpath=".//datacite:affiliation") %>% xml_text()
  title <- records[i]  %>% xml_find_first(xpath='.//datacite:title') %>% xml_text()
  title <- str_split(title,":")[[1]][1]
  materialType <- records[i] %>% xml_find_all(xpath='.//dc:description') %>% xml_text()
  materialType <- materialType[grepl("Material type", materialType)]
  materialType <- ifelse(length(materialType) > 0, str_split(materialType,":")[[1]][2]  %>% trimws, " ")
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
