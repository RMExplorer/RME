## gobal.R
## Used to store helper functions and global variables

library(curl)
library(xml2)
library(stringr)
library(webchem)
library(PubChemR)

# conversion factors
fracFactor <- c("mg/g" = 1000, "µg/kg" = 1/1000, "g/g" = 1e6, "pg/g" = 1e-6,
                "ng/g" = 1/1000, "kg/kg" = 1e6, "g/kg" = 1000)
concFactor <- c("µg/mL" = 1, "mg/L"  = 1, "µg/L"  = 1e-3,
                "mg/mL" = 1e3, "ng/L"  = 1e-6, "g/mL"  = 1e6)

dbPath <- "nrc_crm.sqlite"

# Helper: convert xml to dataframe (Juris function)
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

# Returns crms that feature a given compound
get_compound_crms <- function(term, inchikey, max_synonyms = 100) {
  if (length(inchikey) == 1 && !is.na(inchikey) && nzchar(inchikey)) {
    link <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/",
                   inchikey, "/synonyms/JSON")
  } else if (length(term) == 1 && !is.na(term) && nzchar(term)) {
    link <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
                   URLencode(term, reserved = TRUE), "/synonyms/JSON")
  } else {
    return(NA_character_)
  }
  
  synonyms <- tryCatch(
    unlist(jsonlite::fromJSON(link)$InformationList$Information$Synonym),
    error = function(e) NULL
  )
  if (is.null(synonyms) || length(synonyms) == 0) return(NA_character_)
  synonyms <- head(unique(tolower(synonyms)), max_synonyms)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  sql <- paste0(
    "SELECT DISTINCT r.name
     FROM analyte_tables a
     JOIN reference_materials r ON a.rmid = r.rmid
     WHERE LOWER(a.name) IN (", paste(rep("?", length(synonyms)), collapse = ", "), ")"
  )
  crms <- DBI::dbGetQuery(con, sql, params = as.list(synonyms))$name
  
  if (length(crms) == 0) NA_character_ else paste(crms, collapse = ", ")
}
