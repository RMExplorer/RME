library(xml2)
library(stringr)

#types of spectrums
types <- c("NMR", "FSMS", "MSMS")

dbPath <- "nrc_crm.sqlite"

#method to get url content (used to bypass ssl)
geturl <- function(url, handle) {
  curl::curl_fetch_memory(url, handle = handle)$content  
}

# Get PubChem synonyms for one compound
get_pubchem_synonyms <- function(cid, n = 10, sep = "; ") {
  if (length(cid) == 0 || is.na(cid[1])) return(NA_character_)
  cid <- cid[1]
  values <- tryCatch({
    syn <- get_synonyms(identifier = cid, namespace = "cid")
    synonyms(syn)$Synonyms
  }, error = function(e) {
    warning("Synonym lookup failed for CID ", cid, ": ", conditionMessage(e))
    NULL
  })
  
  if (is.null(values) || length(values) == 0) return(NA_character_)
  
  paste(head(values, n), collapse = sep)
}
