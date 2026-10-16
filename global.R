library(curl)
library(xml2)
library(stringr)
library(webchem)
library(PubChemR)

#types of spectrums
types <- c("NMR", "FSMS", "MSMS")

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

h_new <- function() curl::new_handle(ssl_verifypeer = FALSE)

# Returns crms that feature a given compound
get_compound_crms <- function(term, inchikey) {
  tryCatch({
    q <- gsub(" ", "+", term)
    if (length(inchikey) == 1 && !is.na(inchikey)) {
      link <- paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=', q,
                     '+OR+', gsub("/", "%2F", gsub(" ", "+", inchikey)),
                     '&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
    } else {
      link <- paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=', q,
                     '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
    }
    d  <- xml_children(read_xml(geturl(link, h_new())))
    df <- xml_to_dataframe(d)[-1, -c(1, 2)]
    df <- df[!is.na(df$title), ]
    crms <- sort(vapply(str_split(df$title, ":"), function(x) str_trim(x[1]), character(1)))
    crms <- unique(crms[crms != "No results"])
    if (length(crms) == 0) NA_character_ else paste(crms, collapse = ", ")
  }, error = function(e) {
    message("NRC lookup failed for '", term, "': ", conditionMessage(e))
    NA_character_
  })
}
