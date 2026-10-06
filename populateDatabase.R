library(xml2)
library(dplyr)
library(httr2)
library(stringr)
library(DBI)
library(RSQLite)
library(webchem)
library(PubChemR)


OAI_BASE_URL <- "https://oai-pmh.nrc-cnrc.gc.ca/dr-dn"

OAI_NS <- c(
  oai = "http://www.openarchives.org/OAI/2.0/",
  oaire = "http://namespace.openaire.eu/schema/oaire/",
  datacite = "http://datacite.org/schema/kernel-4",
  dc = "http://purl.org/dc/elements/1.1/"
)

ANALYTE_NS <- c(
  a = "http://dr-dn.nrc-cnrc.gc.ca/analytexml"
)


# Helper: extract text from an XML node
get_text <- function(node) {
  if (length(node) == 0) {return(NA_character_)}
  
  value <- xml_text(node)
  
  if (length(value) == 0 || is.na(value)) {return(NA_character_)}
  
  value <- str_trim(value)
  
  if (value == "") {
    NA_character_
  } else {
    value
  }
}


# Parse one OAI-PMH record
parse_reference_material <- function(record) {
  identifier <- get_text(xml_find_first(record, ".//oai:identifier", OAI_NS))
  
  # skip records with no ID
  if (is.na(identifier)) return(NULL)
  
  # rmid = everything after the repository prefix
  rmid <- str_remove(identifier, "^oai:dr-dn\\.cisti-icist\\.nrc-cnrc\\.ca:")
  
  resource <- xml_find_first(record, ".//oaire:resource", OAI_NS)
  if (length(resource) == 0) return(NULL) # skip empty records
  
  # title
  title <- get_text(xml_find_first(resource, "./datacite:titles/datacite:title[@xml:lang='en']", OAI_NS))
  
  # name = part of title before first colon
  name <- if (!is.na(title)) {
    str_trim(
      str_split_fixed(title, ":", 2)[, 1]
    )
  } else {
    NA_character_
  }
  
  # affiliations
  creators <- xml_find_all(resource, "./datacite:creators/datacite:creator", OAI_NS)
  
  affiliations <- xml_find_all(creators, "./datacite:affiliation", OAI_NS)
  affiliations <- str_trim(xml_text(affiliations))
  affiliations <- unique(affiliations[affiliations != ""])
  
  affiliation <- if (length(affiliations) > 0) {
    paste(affiliations, collapse = ", ")
  } else {
    NA_character_
  }
  
  # description
  descriptions_en <- xml_find_all(resource, "./dc:description[@xml:lang='en']", OAI_NS)
  description_text <- str_trim(xml_text(descriptions_en))
  
  # material type
  material_type_description <- description_text[
    str_detect(
      description_text,
      regex("^Material type:", ignore_case = TRUE)
    )
  ]
  
  material_type <- if (length(material_type_description) > 0) {
    str_trim(
      str_remove(
        material_type_description[[1]],
        regex("^Material type:\\s*", ignore_case = TRUE)
      )
    )
  } else {
    NA_character_
  }
  
  # summary
  summary_candidates <- description_text[
    !str_detect(
      description_text,
      regex("^Material type:", ignore_case = TRUE )
    )
  ]
  
  summary <- if (length(summary_candidates) > 0) {
    summary_candidates[[1]]
  } else {
    NA_character_
  }
  
  # doi
  alternate_identifiers <- xml_find_all(resource, "./datacite:alternateIdentifiers/datacite:alternateIdentifier", OAI_NS)
  identifier_types <- xml_attr(alternate_identifiers, "alternateIdentifierType")
  doi_node <- alternate_identifiers[str_to_upper(identifier_types) == "DOI"]
  doi <- get_text(doi_node)
  
  # publication date
  dates <- xml_find_all(resource, "./datacite:dates/datacite:date", OAI_NS)
  date_types <- xml_attr(dates, "dateType")
  issued_node <- dates[str_to_lower(date_types) == "issued"]
  date <- get_text(issued_node)
  
  # analyte table xml url
  analyte_url <- get_text(xml_find_first(resource, "./oaire:file[@mimeType='application/xml']", OAI_NS))
  
  # build one-row data frame
  reference_material <- data.frame(
    rmid = rmid,
    name = name,
    title = title,
    affiliation = affiliation,
    material_type = material_type,
    summary = summary,
    doi = doi,
    date = date,
    stringsAsFactors = FALSE
  )
  
  list(
    reference_material = reference_material,
    analyte_url = analyte_url
  )
}

# Parse one NRC analyte XML file
parse_analyte_xml <- function(url, rmid) {
  
  if (is.na(url) || url == "") {
    return(
      list(
        analytes = data.frame(
          rmid = character(),
          name = character(),
          quantity = character(),
          value = character(),
          uncertainty = numeric(),
          unit = character(),
          type = character(),
          stringsAsFactors = FALSE
        ),
        
        compounds = data.frame(
          name = character(),
          inchikey = character(),
          stringsAsFactors = FALSE
        )
      )
    )
  }
  
  # download XML
  response <- request(url) |> req_perform()
  xml <- resp_body_xml(response)
  
  # find analytes
  analytes <- xml_find_all(xml, ".//a:analyte", ANALYTE_NS)
  
  if (length(analytes) == 0) {
     return(
      list(
        analytes = data.frame(
          rmid = character(),
          name = character(),
          quantity = character(),
          value = character(),
          uncertainty = numeric(),
          unit = character(),
          type = character(),
          stringsAsFactors = FALSE
        ),
        
        compounds = data.frame(
          name = character(),
          inchikey = character(),
          stringsAsFactors = FALSE
        )
      )
    )
  }
  
  analyte_rows <- list()
  compound_rows <- list()
  
  # parse each analyte
  for (analyte in analytes) {
    
    # compound name
    name <- get_text(xml_find_first(analyte, "./a:name/a:term[@xml:lang='en']", ANALYTE_NS))
    
    # inchikey
    inchikey <- get_text(xml_find_first(analyte, "./a:identifier[@type='InChIKey']", ANALYTE_NS))
    
    # quantity
    quantity <- get_text(xml_find_first(analyte, "./a:amount/a:quantity/a:term[@xml:lang='en']", ANALYTE_NS))
    
    # value
    value <- get_text(xml_find_first(analyte, "./a:amount/a:value", ANALYTE_NS))
    
    # expanded uncertainty
    uncertainty <- as.numeric(get_text(xml_find_first(analyte, "./a:amount/a:uncertainty[@type='expanded']", ANALYTE_NS)))
    
    # unit
    unit <- get_text(xml_find_first(analyte, "./a:amount/a:unit", ANALYTE_NS))
    
    # type
    type <- get_text(xml_find_first(analyte, "./a:amount/a:type/a:term[@xml:lang='en']", ANALYTE_NS))
    
    # analyte table row
    analyte_rows[[length(analyte_rows) + 1]] <- data.frame(
      rmid = rmid,
      name = name,
      quantity = quantity,
      value = value,
      uncertainty = uncertainty,
      unit = unit,
      type = type,
      stringsAsFactors = FALSE
    )
    
    # compound row
    compound_rows[[length(compound_rows) + 1]] <- data.frame(
      name = name,
      inchikey = inchikey,
      stringsAsFactors = FALSE
    )
  }
  
  # combine results
  if (length(analyte_rows) == 0) {
    analyte_table <- data.frame(
      rmid = character(),
      name = character(),
      quantity = character(),
      value = character(),
      uncertainty = numeric(),
      unit = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
  } else {
    analyte_table <- do.call(rbind, analyte_rows)
  }
  
  if (length(compound_rows) == 0) {
    compounds <- data.frame(
      inchikey = character(),
      name = character(),
      stringsAsFactors = FALSE
    )
  } else {
    compounds <- do.call(rbind, compound_rows)
    
    # one row per compound
    compounds <- compounds[!duplicated(compounds$name), , drop = FALSE]
  }
  
  list(analytes = analyte_table, compounds = compounds)
}

# Get PubChem properties for one compound
get_pubchem_properties <- function(name, inchikey) {
  identifier <- if (is.null(inchikey) || is.na(inchikey) || inchikey == "") name else inchikey
  namespace  <- if (is.null(inchikey) || is.na(inchikey) || inchikey == "") "name" else "inchikey"
  
  props <- tryCatch(
    get_properties(
      properties = c(
        "smiles",
        "InChIKey",
        "MolecularFormula",
        "MolecularWeight",
        "ExactMass",
        "TPSA",
        "XLogP"
      ),
      identifier = identifier,
      namespace = namespace,
      propertyMatch = list(
        .ignore.case = TRUE,
        type = "contain"
      )
    ),
    error = function(e) {
      warning("PubChem lookup failed for ", name, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(props)) {
    return(
      data.frame(
        name = name,
        inchikey = inchikey,
        cid = NA_integer_,
        molecular_formula = NA_character_,
        molecular_weight = NA_real_,
        smiles = NA_character_,
        pKow = NA_real_,
        exact_mass = NA_real_,
        TPSA = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }
  info <- tryCatch(
    retrieve(
      object = props,
      .which = identifier,
      .to.data.frame = TRUE
    ),
    error = function(e) {
      warning( "Could not retrieve PubChem result for ", name, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(info) || nrow(info) == 0) {
    return(
      data.frame(
        name = name,
        inchikey = inchikey,
        cid = NA_integer_,
        molecular_formula = NA_character_,
        molecular_weight = NA_real_,
        smiles = NA_character_,
        pKow = NA_real_,
        exact_mass = NA_real_,
        TPSA = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # use first matching pubchem record
  info <- info[1, , drop = FALSE]
  
  # helper for extracting a pubchem property
  get_info_value <- function(column) {
    if (
      !column %in% names(info) ||
      length(info[[column]]) == 0 ||
      is.na(info[[column]][1])
    ) {
      return(NA)
    }
    
    info[[column]][1]
  }
  
  data.frame(
    name = name,
    inchikey = if (!is.null(inchikey) || !is.na(inchikey) && inchikey != "") inchikey else as.character(get_info_value("InChIKey")),
    cid = as.integer(get_info_value("CID")),
    molecular_formula = as.character(get_info_value("MolecularFormula")),
    molecular_weight = as.numeric(get_info_value("MolecularWeight")),
    smiles = as.character(get_info_value("SMILES")),
    pKow = as.numeric(get_info_value("XLogP")) * -1,
    exact_mass = as.numeric(get_info_value("ExactMass")),
    TPSA = as.numeric(get_info_value("TPSA")),
    stringsAsFactors = FALSE
  )
}

# Get PubChem synonyms for one compound
get_pubchem_synonyms <- function(cid, n = 10, sep = "; ") {
  if (is.null(cid) || is.na(cid)) return(NA_character_)
  
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

# Harvest all NRC CRM records
# Returns: reference_materials, analyte_tables, compounds
get_oai_records <- function(base_url = OAI_BASE_URL, metadata_prefix = "oai_openaire", set = "crm") {
  reference_materials <- list()
  analyte_tables <- list()
  compounds <- list()
  
  # first request
  response <- request(base_url) |>
    req_url_query(
      verb = "ListRecords",
      metadataPrefix = metadata_prefix,
      set = set
    ) |> req_perform()
  
  xml <- resp_body_xml(response)
  
  
  # process pages
  repeat {
    
    records <- xml_find_all(
      xml,
      ".//oai:record",
      OAI_NS
    )
    
    # parse every record on this page
    for (record in records) {
      parsed <- parse_reference_material(record)
      
      if (is.null(parsed)) {next}
      
      # reference material
      reference_materials[[length(reference_materials) + 1]] <- parsed$reference_material
      
      # analyte XML
      analyte_result <- tryCatch(
        parse_analyte_xml(
          url = parsed$analyte_url,
          rmid = parsed$reference_material$rmid
        ),
        
        error = function(e) {
          warning(
            "Could not parse analyte XML for rmid ",
            parsed$reference_material$rmid,
            ": ",
            conditionMessage(e)
          )
          NULL
        }
      )
      
      if (!is.null(analyte_result)) {
        if (nrow(analyte_result$analytes) > 0) {
          analyte_tables[[length(analyte_tables) + 1]] <- analyte_result$analytes
        }
        
        if (nrow(analyte_result$compounds) > 0) {
          compounds[[length(compounds) + 1]] <- analyte_result$compounds
        }
      }
    }
    
    # resumption token
    token <- get_text(xml_find_first(xml, ".//oai:resumptionToken", OAI_NS))
    
    if (is.na(token) || token == "") {break}
    
    response <- request(base_url) |>
      req_url_query(
        verb = "ListRecords",
        resumptionToken = token
      ) |>
      req_perform()
    
    xml <- resp_body_xml(response)
  }
  
  # combine reference materials
  reference_materials <- if (length(reference_materials) > 0) {
    do.call(rbind, reference_materials)
  } else {
    data.frame(
      rmid = character(),
      name = character(),
      title = character(),
      affiliation = character(),
      material_type = character(),
      summary = character(),
      doi = character(),
      date = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # combine analytes
  analyte_tables <- if (length(analyte_tables) > 0) {
    do.call(rbind, analyte_tables)
  } else {
    data.frame(
      rmid = character(),
      name = character(),
      quantity = character(),
      value = character(),
      uncertainty = numeric(),
      unit = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # combine compounds
  compounds <- if (length(compounds) > 0) {
    compounds <- do.call(rbind, compounds)
    
    # same compound can occur in many reference materials - keep one row per name
    compounds[!duplicated(compounds$name), ,drop = FALSE]
    
  } else {
    data.frame(
      name = character(),
      inchikey = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # return the three datasets
  list(
    reference_materials = reference_materials,
    analyte_tables = analyte_tables,
    compounds = compounds
  )
}

# Add columns to compounds table with info from pubchem
enrich_compounds <- function(compounds) {
  if (nrow(compounds) == 0) {
    return(
      data.frame(
        name = character(),
        inchikey = character(),
        molecular_formula = character(),
        molecular_weight = numeric(),
        smiles = character(),
        pKow = numeric(),
        exact_mass = numeric(),
        TPSA = numeric(),
        synonyms = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  results <- lapply(seq_len(nrow(compounds)), function(i) {
    row <- get_pubchem_properties(
      name = compounds$name[i],
      inchikey = compounds$inchikey[i]
    )
    # separate call for synonyms
    row$synonyms <- get_pubchem_synonyms(row$cid)
    row
  })
  
  do.call(rbind, results)
}


harvested <- get_oai_records()

referece_materials <- harvested$reference_materials
analyte_tables <- harvested$analyte_tables
compounds<- enrich_compounds(harvested$compounds)



