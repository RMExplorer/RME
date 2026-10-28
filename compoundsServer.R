## compoundsServer.R
## Server logic for the 'Compound Search' tab; manages the list of compounds in the compound table

getTableData <- ExtendedTask$new(function(compounds, dbPath) {
  future_promise({
    # connect to database
    con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
    on.exit(DBI::dbDisconnect(con), add = TRUE) # close connection when function returns
    
    # helper functions:
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
           OR name = ? COLLATE NOCASE
        ORDER BY (LOWER(name) = LOWER(?)) DESC,
                 (name = ? COLLATE NOCASE) DESC,
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
        
        cid          <- pick(info[["CID"]])
        synonyms     <- get_pubchem_synonyms(cid)
        compoundName <- term
        if (isInchikey && !is.na(synonyms)) compoundName <- strsplit(synonyms, "; ")[[1]][1]
        crms <- tryCatch(
          get_compound_crms(compoundName, pick(info[["InChIKey"]])),
          error = function(e) {
            message("CRM lookup failed for '", term, "': ", conditionMessage(e))
            NA_character_
          }
        )
        
        data.frame(
          name = compoundName, cid = cid, inchikey = pick(info[["InChIKey"]]),
          molecular_formula = pick(info[["MolecularFormula"]]),
          molecular_weight = pick(info[["MolecularWeight"]]),
          smiles = pick(info[["SMILES"]]), pKow = -pick(info[["XLogP"]]),
          exact_mass = pick(info[["ExactMass"]]), TPSA = pick(info[["TPSA"]]),
          crms = crms,
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
    
    # measurements for one compound from the database
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
      
      crms <- character()
      rawCrms <- val("crms")
      if (!is.na(rawCrms) && nzchar(rawCrms)) {
        crms <- unique(trimws(strsplit(rawCrms, ",")[[1]]))
      }
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
          check.names = FALSE, stringsAsFactors = FALSE
        )
      )
    })
    
    # CRMs shared by every searched analyte get highlighted red
    commonCrms <- Reduce(intersect, lapply(rows, `[[`, "crms"))
    makeLinks <- function(crmList) {
      if (length(crmList) == 0) return("<p>No results</p>")
      crmList <- c(crmList[crmList %in% commonCrms], crmList[!crmList %in% commonCrms]) # show highlighted links first
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

# the analytes shown in the select analyte drop down menu
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

# stores the info for the selected row
v <- reactiveValues(
  data = data.frame(name = character(), title = character(), summary = character(),
                    id = character(), doi = character(), date = character(),
                    stringsAsFactors = FALSE),
  crms = character(),
  spectralData = NULL
)

# the compound whose properties are currently being shown
viewedCompound <- reactiveVal(NULL)

# contains list of compounds currently in the table
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

# 'View properties' button on the home page: forwards to the main button
observeEvent(input$viewPropertiesHome, {
  if (length(input$customTable_rows_selected) == 0) {
    showNotification("Select a compound in the Compound Table first.", type = "warning")
  } else if (length(input$customTable_rows_selected) != 1) {
    showNotification("Make sure you have only selected one row.", type = "warning")
  } else {
    shinyjs::click("viewProperties")
  }
})

# when a row from the table is selected and the "View Properties" button is clicked, 
# gathers all the information necessary for the 'Properties' and 'Spectral Data' page
observeEvent(input$viewProperties, {
  if (length(input$customTable_rows_selected) == 0) {
    showNotification("Select a compound in the Compound Table first.", type = "warning")
  } else if (length(input$customTable_rows_selected) != 1) {
    showNotification("Make sure you have only selected one row.", type = "warning")
  }
  req(length(input$customTable_rows_selected) == 1)
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  row <- result %>% slice(input$customTable_rows_selected)
  viewedCompound(row)
  
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
  
  # CRM info from the database, using the CRMs already attached to this compound
  crmNames <- if (!is.na(row$CRMs) && nzchar(row$CRMs)) {
    unique(trimws(strsplit(row$CRMs, ",")[[1]]))
  } else character()
  
  if (length(crmNames) > 0) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
    df <- tryCatch(
      DBI::dbGetQuery(con, paste0(
        "SELECT rmid AS id, name, title, summary, doi, date
       FROM reference_materials
       WHERE name IN (", paste(rep("?", length(crmNames)), collapse = ","), ")
       ORDER BY name"), params = as.list(crmNames)),
      finally = DBI::dbDisconnect(con)
    )
    crms <- df$name
    names(crms) <- crms
    v$data <- df
    v$crms <- crms
  } else {
    v$data <- data.frame(name = "No results", title = "No results", summary = "",
                         id = "", doi = "", date = "", stringsAsFactors = FALSE)
    v$crms <- c("No results" = "No results")
  }
  v$spectralData <- spectralData
  session$sendCustomMessage("scrollToTop", list())
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

# when the 'Save Table' button is clicked, show modal to download compounds in the table
observeEvent(input$saveAnalytes, {
  # modal with download button
  showModal(
    modalDialog(
      title = "Save Your Table",
      downloadButton("downloadSavedSubstances", "Save", color="success"),
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
})

# saves all the names from yourTableAnalytes in a csv file
output$downloadSavedSubstances <- downloadHandler(
  filename = function() {
    paste("yourCompounds.csv")
  },
  content = function(file) {
    write.csv(getTableData$result()$Name, file, row.names = FALSE)
  }
)

# when the 'Load Table' button is clicked, show modal that asks user to upload a csv file containing compound names
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


# loads the saved compounds from the user-uploaded csv file
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
  
  datatable(data, selection = "multiple",
            options = list(pageLength = 10, responsive = FALSE, scrollX = TRUE, autoWidth = TRUE, dom = 'ltip',
                           # show only the first 4 CRM links, "more"/"less" expands or collapses the cell
                           # (handled by the .cell-toggle click handler in ui.R)
                           columnDefs = list(list(
                             targets = 4,
                             render = JS(
                               "function(data, type, row, meta) {",
                               "  if (type === 'display' && data != null) {",
                               "    var links = data.match(/<a [^>]*>.*?<\\/a>/g);",
                               "    if (links && links.length > 4) {",
                               "      var short = links.slice(0, 4).join(' ') + ' ...';",
                               "      return '<span class=\"cell-expand\" data-full=\"' + encodeURIComponent(data) +",
                               "             '\" data-short=\"' + encodeURIComponent(short) +",
                               "             '\" data-expanded=\"false\">' + short + ' <a href=\"#\" class=\"cell-toggle\">more</a></span>';",
                               "    }",
                               "  }",
                               "  return data;",
                               "}"
                             )
                           ))),
            filter= list(position='top', clear = FALSE), escape = FALSE, rownames = FALSE)
})

observeEvent(input$addallSubstances, {
  #hide the table to trigger loading image
  shinyjs::hide("customTable")
  
  #list of all unique values in the analyte column of the analyte tables
  newList <- analytes(dbPath)
  yourTableAnalytes(newList)
})