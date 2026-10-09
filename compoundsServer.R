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


getTableData <- ExtendedTask$new(function(compounds, dbPath) {
  future_promise({
    con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    # overrides the ssl verifypeer so the webpage can be reached on shinyapps
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    
    # helper functions:
    fracFactor <- c("mg/g" = 1000, "µg/kg" = 1/1000, "g/g" = 1e6, "pg/g" = 1e-6,
                    "ng/g" = 1/1000, "kg/kg" = 1e6, "g/kg" = 1000)
    concFactor <- c("µg/L" = 1/1000, "mg/mL" = 1000, "g/mL" = 1e6)
    convert <- function(value, unit, factors) {
      f <- unname(factors[unit]); f[is.na(f)] <- 1
      value * f
    }
    getRange <- function(x) if (length(x) == 0) c(0, 0) else range(x)
    pick <- function(x) if (length(x) > 0 && !is.na(x[1])) x[[1]] else NA
    
    emptyAm <- data.frame(crm = character(), quantity = character(),
                          value = numeric(), unit = character(),
                          stringsAsFactors = FALSE)
    
    # get compound info from the db
    dbCompound <- function(x) {
      DBI::dbGetQuery(con, "
        SELECT * FROM compounds
        WHERE inchikey = ? COLLATE NOCASE
           OR name LIKE ?
        ORDER BY (LOWER(name) = LOWER(?)) DESC,
                 (name LIKE ?) DESC,
                 LENGTH(name)
        LIMIT 1", params = as.list(rep(x, 4)))
    }
    
    # get compound info from pubchem if db doesn't have it
    pubchemCompound <- function(term) {
      tryCatch({
        isInchikey <- is.inchikey(term)
        props <- get_properties(
          properties = c("smiles", "inchikey", "MolecularFormula", "MolecularWeight",
                         "ExactMass", "TPSA", "XLogP"),
          identifier = term,
          namespace = if (isInchikey) "inchikey" else "name",
          propertyMatch = list(.ignore.case = TRUE, type = "contain")
        )
        info <- retrieve(object = props, .which = term, .to.data.frame = TRUE)
        
        synonyms <- get_pubchem_synonyms(info$CID)
        
        compoundName <- term
        if (isInchikey) {
          compoundName <- tryCatch({
            syn <- get_synonyms(identifier = info$CID, namespace = "cid")
            synonyms(syn)$Synonyms[1]
          }, error = function(e) term)
        }
        
        data.frame(
          name = compoundName, cid = pick(info[["CID"]]), inchikey = pick(info[["InChIKey"]]),
          molecular_formula = pick(info[["MolecularFormula"]]),
          molecular_weight = pick(info[["MolecularWeight"]]),
          smiles = pick(info[["SMILES"]]), pKow = -pick(info[["XLogP"]]),
          exact_mass = pick(info[["ExactMass"]]), TPSA = pick(info[["TPSA"]]),
          synonyms = synonyms,
          stringsAsFactors = FALSE
        )
      }, error = function(e) NULL)
    }
    
    lookupCompound <- function(term) {
      comp <- dbCompound(term)
      if (nrow(comp) > 0) return(comp[1, ])
      net <- pubchemCompound(term)
      if (is.null(net)) return(NULL)
      # pubchem may resolve the term to a compound the db stores under another name
      # prefer the db row so the name matches how compounds are named
      if (!is.na(net$inchikey)) {
        comp <- dbCompound(net$inchikey)
        if (nrow(comp) > 0) return(comp[1, ])
      }
      net
    }
    
    # CRM list: from the NRC repository atom search
    searchCrms <- function(term, inchikey) {
      tryCatch({
        q <- gsub(" ", "+", term)
        if (!is.na(inchikey)) {
          link <- paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=', q,
                         '+OR+', gsub("/", "%2F", gsub(" ", "+", inchikey)),
                         '&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
        } else {
          link <- paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=', q,
                         '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
        }
        d  <- xml_children(read_xml(geturl(link, h)))
        df <- xml_to_dataframe(d)[-1, -c(1, 2)]
        df <- df[!is.na(df$title), ]
        crms <- sort(sapply(str_split(df$title, ":"), function(x) x[1]))
        crms[crms != "No results"]
      }, error = function(e) character())
    }
    
    # measurements for one compound from the database, limited to the CRMs the atom search returned
    getMeasurements <- function(compoundName, crms) {
      if (is.na(compoundName) || length(crms) == 0) return(emptyAm)
      am <- DBI::dbGetQuery(con, "
        SELECT rm.name AS crm, a.quantity, a.value, a.unit
        FROM analyte_tables a
        JOIN reference_materials rm ON rm.rmid = a.rmid
        WHERE LOWER(a.name) LIKE '%' || LOWER(?) || '%'", params = list(compoundName))
      am$value <- suppressWarnings(as.numeric(am$value))
      am[am$crm %in% crms & !is.na(am$value), ]
    }
    
    # loop through compounds
    rows <- lapply(compounds, function(term) {
      comp <- lookupCompound(term)
      val  <- function(col) if (is.null(comp)) NA else pick(comp[[col]])
      searchName <- if (is.null(comp)) term else comp$name[1]
      
      crms <- searchCrms(term, val("inchikey"))
      am   <- getMeasurements(searchName, crms)
      
      frac <- am[grepl("mass fraction", am$quantity, ignore.case = TRUE), ]
      conc <- am[grepl("mass concentration", am$quantity, ignore.case = TRUE), ]
      fracRange <- getRange(convert(frac$value, frac$unit, fracFactor))
      concRange <- getRange(convert(conc$value, conc$unit, concFactor))
      
      list(
        crms = crms,
        row = data.frame(
          "Name"                               = searchName,
          "CID"                                = val("cid"),
          "Molecular Formula"                  = val("molecular_formula"),
          "Molecular Weight"                   = val("molecular_weight"),
          "Isomeric Smiles"                    = val("smiles"),
          "InchiKey"                           = val("inchikey"),
          "pKow"                               = val("pKow"),
          "Exact Mass"                         = val("exact_mass"),
          "TPSA"                               = val("TPSA"),
          "CRMs"                               = if (length(crms)) paste(crms, collapse = ",") else NA,
          "Minimum Mass Fraction (µg/g)"       = fracRange[1],
          "Maximum Mass Fraction (µg/g)"       = fracRange[2],
          "Minimum Mass Concentration (µg/mL)" = concRange[1],
          "Maximum Mass Concentration (µg/mL)" = concRange[2],
          "Synonyms"                           = val("synonyms"),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      )
    })
    
    # CRMs shared by every searched analyte get highlighted red
    commonCrms <- Reduce(intersect, lapply(rows, `[[`, "crms"))
    makeLinks <- function(crmList) {
      if (length(crmList) == 0) return("<p>No results</p>")
      paste0('<a href="#" class="view-info2" data-name="', crmList, '"',
             ifelse(crmList %in% commonCrms, ' style="color:red"', ''), '>',
             crmList, "</a>", collapse = " ")
    }
    
    data <- do.call(rbind, lapply(rows, `[[`, "row"))
    data[["Reference Materials"]] <- vapply(lapply(rows, `[[`, "crms"), makeLinks, character(1))
    row.names(data) <- NULL
    data
  }, seed = TRUE)
})

#the analytes shown in the select analyte drop down menu
analytes <- function(dbPath) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbGetQuery(con, "SELECT name FROM compounds ORDER BY name COLLATE NOCASE")$name
}

#observes for changes in the your table analyte list of all the substances and invokes getTableData when it changes
observeEvent(yourTableAnalytes(), {
  shinyjs::hide("customTable")
  getTableData$invoke(yourTableAnalytes(), dbPath)
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
yourTableAnalytes <- reactiveVal(list())

#the 'Select an Analyte' drop down in the 'Substances' page which also allows users to enter their own analyte names
output$searchAnalyte <- renderUI({
  selectizeInput(inputId = "selectAnalyte", div("Search NRC Repository", 
                                                tooltip_ui("searchTooltip", 
                                                           "Search your Compound, Inchikey, IUPAC, or Keyword in the NRC Repository. If no results are found, will enquire the closest match from PubChem and search the repository again."),
                                                style="display:flex;"), 
                 choices = append("", analytes(dbPath)), 
                 selected = "", 
                 options = list(create = TRUE, delimiter=';'),
                 width="350px")
})

#when a row from the table is clicked, gathers all the information necessary for the 'Properties' and 'Spectral Data' page
observeEvent(input$customTable_rows_selected, {
  if (length(input$customTable_rows_selected) == 1) {
    req(length(getTableData$result()) > 0)
    result <- getTableData$result()
    row = result %>% slice(input$customTable_rows_selected)
    
    if (!is.na(row$InchiKey) && nzchar(row$InchiKey)) {
      # search by name + inchikey for max coverage
      link = paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=',
                    gsub(' ','+', row$Name), '+OR+',
                    gsub("/", "%2F", gsub(" ", "+", row$InchiKey)),
                    '&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
    } else {
      # search only by name if inchikey does not exist
      link = paste0('https://nrc-digital-repository.canada.ca/eng/search/atom/?q=',
                    gsub(' ','+', row$Name), '&q=&q=&y1=&y2=&cn=crm&ps=10&s=sc&av=1')
    }
    
    #overrides the ssl verifypeer so the webpage can be reached
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    xmlDoc <- read_xml(geturl(link, h))
    rm(h)
    
    d = xml_children(xmlDoc)
    df = xml_to_dataframe(d)[-1,-c(1,2)]
    
    # spectral data from the database
    ik <- if (!is.na(row$InchiKey) && nzchar(row$InchiKey)) trimws(row$InchiKey) else ""
    
    con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
    spectra <- tryCatch(
      DBI::dbGetQuery(con, "
        SELECT rm.name AS crm, s.name AS substance, s.datatype, s.link
        FROM spectral_data s
        JOIN reference_materials rm ON rm.rmid = s.rmid
        WHERE (? <> '' AND s.inchikey = ? COLLATE NOCASE)
           OR (LENGTH(TRIM(s.name)) > 0 AND INSTR(LOWER(?), LOWER(TRIM(s.name))) > 0)
        ORDER BY rm.name, s.datatype",
                      params = list(ik, ik, row$Name)),
      finally = DBI::dbDisconnect(con)
    )
    
    spectralData <- if (nrow(spectra) > 0) {
      data.frame(Name = paste(spectra$crm, spectra$datatype, spectra$substance, sep = ", "),
                 SpectralLink = spectra$link,
                 stringsAsFactors = FALSE)
    } else {
      data.frame()
    }
    
    #for the selected analyte, save its name crm information in a reactive variable
    df$name = sapply(str_split(df$title,":"), function(x) x[1])
    df = df[!is.na(df$title),]
    v$data = df
    crms = sort(df$name)
    names(crms) = crms
    v$crms = crms
    v$spectralData <- spectralData
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
    paste("yourCompounds.csv")
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
  result <- getTableData$result()
  cols <- c("Name", "Molecular Formula", "Molecular Weight", 
            "pKow", "Reference Materials"
            # "Minimum Mass Fraction (µg/g)", 
            # "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
            # "Maximum Mass Concentration (µg/mL)"
            )
  
  if (is.null(result) || !is.data.frame(result) || nrow(result) == 0) {
    data <- data.frame("Name" = character(0),
                       "MF" = character(0),
                       "MW" = numeric(0),
                       "pKow" = numeric(0),
                       "Reference Materials" = character(0),
                       # "Minimum Mass Fraction (µg/g)" = numeric(0),
                       # "Maximum Mass Fraction (µg/g)" = numeric(0),
                       # "Minimum Mass Concentration (µg/mL)" = numeric(0),
                       # "Maximum Mass Concentration (µg/mL)" = numeric(0),
                       check.names = FALSE)
  } else {
    data <- result[, cols]
    data <- data.frame("Name" = data$Name, 
                       "Molecular Formula" = data$"Molecular Formula", 
                       "Molecular Weight" = as.numeric(data$"Molecular Weight"), 
                       "pKow" = as.numeric(data$"pKow"),
                       "Reference Materials" = data$"Reference Materials", 
                       # "Minimum Mass Fraction (µg/g)" = as.numeric(data$"Minimum Mass Fraction (µg/g)"), 
                       # "Maximum Mass Fraction (µg/g)" = as.numeric(data$"Maximum Mass Fraction (µg/g)"), 
                       # "Minimum Mass Concentration (µg/mL)" = as.numeric(data$"Minimum Mass Concentration (µg/mL)"), 
                       # "Maximum Mass Concentration (µg/mL)" = as.numeric(data$"Maximum Mass Concentration (µg/mL)"),
                       check.names = FALSE)
    colnames(data) <- c("Name", "MF", "MW", "pKow", "Reference Materials"
                        # "Minimum Mass Fraction (µg/g)", 
                        # "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
                        # "Maximum Mass Concentration (µg/mL)"
                        )
  }
  
  datatable(data, 
            options = list(pageLength = 10, responsive = FALSE, scrollX = TRUE, autoWidth = TRUE, dom = 'ltip'), 
            filter= list(position='top', clear = FALSE), escape = FALSE, rownames = FALSE)
})

#unselect all rows in the custom table when unselect button is clicked
observeEvent(input$unselect, {
  selectRows(proxy = dataTableProxy("customTable", session = session), 
             selected = NULL)
})

observeEvent(input$addallSubstances, {
  #hide the table to trigger loading image
  shinyjs::hide("customTable")
  
  #list of all unique values in the analyte column of the analyte tables
  newList <- analytes("nrc_crm.sqlite")
  yourTableAnalytes(newList)
})