## physicoChemicalPlot.R
## Server logic for the 'Polarity-MW Plot' tab

materialTypes <- c("Matrix material", "Calibration solution", "High-purity material")

num <- function(x) suppressWarnings(as.numeric(x))

typeFilterOn <- reactive(isTruthy(input$materialType))  # "" or NULL = all types
colorOn <- reactive(typeFilterOn() && isTRUE(input$colorByConc))

analyteRanges <- reactive({
  req(typeFilterOn())
  
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  rows <- DBI::dbGetQuery(con, "
    SELECT a.name, LOWER(a.quantity) AS quantity, a.value, a.unit
    FROM analyte_tables a
    JOIN reference_materials r ON r.rmid = a.rmid
    WHERE r.material_type = ?",
                          params = list(input$materialType))
  
  # "mass concentration (20 °C)" -> "mass concentration"
  qbase <- trimws(sub("\\s*\\(.*\\)\\s*$", "", rows$quantity))
  f <- unname(concFactor[gsub("μ", "µ", trimws(rows$unit))])   # NA if unit unrecognised
  isConc <- qbase %in% c("mass concentration", "concentration") & !is.na(f)
  
  data.frame(key  = tolower(trimws(rows$name)),
             conc = ifelse(isConc, num(rows$value) * f, NA_real_)) %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(
      minConc = if (all(is.na(conc))) NA_real_ else min(conc, na.rm = TRUE),
      maxConc = if (all(is.na(conc))) NA_real_ else max(conc, na.rm = TRUE),
      .groups = "drop")
})

#allows filtering of plot data
output$plotFilters <- renderUI({
  req(nrow(getTableData$result()) > 0)
  result <- getTableData$result()
  list(
    tags$div(tags$div(selectInput("pkLevel", 
                                  "Filter polarity (pKow)", 
                                  choices = c("None", "Low", "High"), 
                                  selected = "None"), 
                      style = "padding:10px;width:200px;"),
             tags$div(selectInput("mwLevel", 
                                  "Filter Molecular Weight", 
                                  choices = c("None", "Low", "High"), 
                                  selected = "None"), 
                      style = "padding:10px;width:200px;"),
             tags$div(selectInput("showLabel", 
                                  "Show Compound Name", 
                                  choices = c("Yes", "No"), 
                                  selected = "No"), 
                      style = "padding:10px;width:200px;"),
             tags$div(selectInput("all", 
                                  "Show All Analytes in the Substances Table or Only Selected Analytes", 
                                  c("All Analytes" = TRUE, "Only Selected Analytes" = FALSE), width='100%'),
                      style = "padding:10px;width:600px;"),
             tags$div(selectInput("materialType", "Filter by Material Type",
                                  choices = c("All material types" = "", materialTypes)),
                      style = "padding:10px;width:300px;"),
             tags$div(conditionalPanel(
               condition = "input.materialType != ''",
               checkboxInput("colorByConc",
                             " Color points by minimum concentration (µg/mL)",
                             value = FALSE),
               conditionalPanel(
                 condition = "input.colorByConc",
                 helpText("Hover over a point to see the full concentration range across CRMs of this material type."))),
               style = "padding:10px;width:280px;"),
             style = "display:flex;flex-wrap: wrap;")
  )
})

#text shown when there are no analytes in the table, or "Only Selected Analytes" 
#is chosen but no rows are selected
output$plotNothingSelected <- renderUI({
  noAnalytes <- is.function(getTableData$result())
  onlySelectedEmpty <- !is.null(input$all) && input$all == "FALSE" && 
    length(input$customTable_rows_selected) == 0
  req(noAnalytes || onlySelectedEmpty)
  
  if (noAnalytes) {
    HTML(paste(
      "<p>Make sure you have added <strong>at least one</strong> analyte to the table in the 'General Search' tab.</p>"
    ))
  } else {
    HTML(paste(
      "<p>Make sure you have selected at least <strong>one</strong> option from the table in the 'General Search' tab.</p>"
    ))
  }
})

#the data after all the filtering
filteredData <- reactive({
  req(!is.function(getTableData$result()))
  result <- getTableData$result()
  req(input$all)
  s <- input$customTable_rows_selected
  
  if(input$all == "FALSE") {
    req(length(s) > 0)
    data <- result  %>% slice(s)
  } else {
    data <- result
  }
  
  #remove data points where pkow or mw is NA
  data <- data[!is.na(data$pKow),]
  data <- data[!is.na(data$"Molecular Weight"),]
  
  if (typeFilterOn()) {
    ar <- analyteRanges()
    data$key <- tolower(trimws(data$Name))
    data <- data[data$key %in% ar$key, ]
    m <- match(data$key, ar$key)
    data$minConc <- ar$minConc[m]
    data$maxConc <- ar$maxConc[m]
  }
  
  if (length(input$pkLevel) == 0 || input$pkLevel == "None"){
    data <- data
  } else if (input$pkLevel == "Low") {
    data <- data %>% filter(as.numeric(pKow) <= -2)
  } else if (input$pkLevel == "High") {
    data <- data %>% filter(as.numeric(pKow) > -2)
  }
  
  if (length(input$mwLevel) == 0 || input$mwLevel == "None"){
    data <- data
  } else if (input$mwLevel == "Low") {
    data <- data %>% dplyr::filter(as.numeric(`Molecular Weight`) <= 500)
  } else if (input$mwLevel == "High") {
    data <- data %>% dplyr::filter(as.numeric(`Molecular Weight`) > 500)
  }
  
  data$pKow<- as.numeric(data$pKow)
  data$"Molecular Weight"<- as.numeric(data$"Molecular Weight")
  
  data
})

#outputs all the names of the current compounds after filtering 
output$compoundList <- renderUI({
  #add a comma after each name
  text <- paste(filteredData()$Name, collapse = ", ")

  #download handler for the file containing all the filtered compounds
  output$filteredDataFile <- downloadHandler(
    filename = function(){
      paste("filteredData.csv")
    },
    content = function(file) {
      write.csv(apply(filteredData()[, c("Name", "CID", "Molecular Weight", "Isomeric Smiles", 
                                         "InchiKey", "pKow", "Exact Mass", "TPSA", "CRMs", "Minimum Mass Fraction (µg/g)", 
                                         "Maximum Mass Fraction (µg/g)", "Minimum Mass Concentration (µg/mL)", 
                                         "Maximum Mass Concentration (µg/mL)")],2,as.character), file, row.names = FALSE)
    }
  )
  
  #output the names and a download button if there is more than 1
  if (nchar(text) > 0) {
    list(
      tags$div(HTML(paste("<strong>Filtered Compounds: </strong>", text)),
               downloadButton('filteredDataFile', "Download the compound list", 
                              style="width:fit-content;padding:5px 20px 5px 20px;margin:10px;"),
               style="display:flex;flex-direction: column;")
      
    )
  }
})

#determine the x positions of the labels
xPos <- function(x) {
  positions <- c("left", "right", "center", "right", "center", "left", "center", "left", "right")
  locations <- c()
  for (i in 1:length(x)){
    locations <- append(locations, positions[i %% length(positions)])
  }
  return (locations)
}

#determine the x positions of the arrow tails
xTail <- function(x) {
  positions <- c(40, -40, 40, -40)
  locations <- c()
  for (i in 1:length(x)){
    locations <- append(locations, positions[i %% length(positions)])
  }
  return (locations)
}

#determine the y positions of the arrow tails
yTail <- function(x) {
  positions <- c(-40, 40, 40, -40)
  locations <- c()
  for (i in 1:length(x)){
    locations <- append(locations, positions[i %% length(positions)])
  }
  return (locations)
}

#creates the plot shown on the Physico-chemical properties tab
output$plot <- renderPlotly({
  req(length(filteredData()) > 0)
  data <- filteredData()
  
  maxMW <- max(max(data$`Molecular Weight`[!is.na(data$`Molecular Weight`)]) + 50, 2500)
  minMW <- 0
  
  maxpkow <- max(max(data$pKow[!is.na(data$pKow)]) + 2, 10)
  minpkow <- min(min(data$pKow[!is.na(data$pKow)]) - 2, -10)

  xlim <- if (length(input$pkLevel) == 0 || input$pkLevel == "None"){
    xlim(minpkow,maxpkow)
  } else if (input$pkLevel == "Low") {
    xlim(minpkow, -2)
  } else if (input$pkLevel == "High") {
    xlim(-2, maxpkow)
  }
  
  ylim <- if (length(input$mwLevel) == 0 || input$mwLevel == "None"){
    ylim(0,maxMW)
  } else if (input$mwLevel == "Low") {
    ylim(minMW, 500)
  } else if (input$mwLevel == "High") {
    ylim(500, maxMW)
  }
  
  useColor <- colorOn() && nrow(data) > 0
  
  if (useColor) {
    data$minConc[!is.na(data$minConc) & data$minConc <= 0] <- NA   # log needs positive values
    data$maxConc[!is.na(data$maxConc) & data$maxConc <= 0] <- NA
    rangeTxt <- ifelse(is.na(data$minConc), "not reported",
                       ifelse(data$minConc == data$maxConc,
                              paste(format(data$minConc, digits = 4), "µg/mL"),
                              paste0(format(data$minConc, digits = 4), " – ",
                                     format(data$maxConc, digits = 4), " µg/mL")))
  }
  data$tip <- paste('<br>Compound:', data$Name,
                    '<br>pKow:', data$pKow,
                    '<br>Molecular Weight: ', data$`Molecular Weight`,
                    if (useColor) paste0('<br>Concentration: ', rangeTxt))
  
  # colored by concentration, or plain black
  points <- if (useColor) {
    list(geom_point(aes(text = map(tip, HTML), color = minConc),
                    size = 2.5, alpha = 0.8),
         scale_color_viridis_c(transform = "log10", na.value = "grey70",
                               name = "Lowest reported\nconcentration\n(µg/mL)"))
  } else {
    geom_point(aes(text = map(tip, HTML)), size = 2, alpha = 0.7)
  }
  
  p <- data %>%
    ggplot(aes(x = pKow, y = `Molecular Weight`)) +
    theme_classic() +
    points +
    ylab("MW") +
    xlim + 
    ylim +
    geom_hline (yintercept = 500, linetype = "solid", color= "red") +
    geom_vline (xintercept = -2, linetype = "solid", color = "blue") +
    geom_text (x=-8, y=2400, label="Low Polarity / High MW", color="grey")+
    geom_text (x=6, y=2400, label="High Polarity / High MW", color="grey")+
    geom_text (x=-8, y=50, label="Low Polarity / Low MW", color="grey")+
    geom_text (x=6, y=50, label="High Polarity / Low MW", color="grey")
  
  p_plotly <- ggplotly(p, tooltip=c("text"))
  
  #show the label when "show label" is marked as "Yes"
  if (!is.null(input$showLabel) && input$showLabel == "Yes" && nrow(data) > 0) {
    p_plotly <- p_plotly %>%
      add_annotations(x = data$pKow, 
                      y = data$`Molecular Weight`, 
                      text = data$`Name`,
                      xref = "x",
                      yref = "y",
                      showarrow = T,
                      xanchor = xPos(data$pKow),
                      ax = xTail(data$pKow),
                      ay = yTail(data$`Molecular Weight`)
                      )
    
  }
  
  return(p_plotly)
})

