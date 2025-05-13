#allows filtering of plot data
output$plotFilters <- renderUI({
  req(length(getTableData$result()) > 0)
  
  div(
    class = "filter-container",
    
    div(class = "filter-item",
        selectInput("pkLevel", 
                    "Filter polarity (pKow)", 
                    choices = c("None", "Low", "High"), 
                    selected = "None")
    ),
    
    div(class = "filter-item",
        selectInput("mwLevel", 
                    "Filter Molecular Weight", 
                    choices = c("None", "Low", "High"), 
                    selected = "None")
    ),
    
    div(class = "filter-item",
        selectInput("showLabel", 
                    "Show Compound Name", 
                    choices = c("Yes", "No"), 
                    selected = "No")
    ),
    
    div(class = "filter-item full-width",
        selectInput("all", 
                    "Show All Analytes in the Substances Table or Only Selected Analytes", 
                    choices = c("All Analytes" = TRUE, "Only Selected Analytes" = FALSE),
                    selected = TRUE)
    ),
    
    div(style = "margin-bottom: 30px;")  #spacer at the bottom
  )
})


#the data after all the filtering
filteredData <- reactive({
  req(length(getTableData$result()) > 0)
  result <- getTableData$result()
  req(input$all)
  s <- input$customTable_rows_selected
  
  if(input$all == "FALSE") {
    data <- result  %>% slice(s)
  } else {
    data <- result
  }
  
  #remove data points where pkow or mw is NA
  data <- data[!is.na(data$pKow),]
  data <- data[!is.na(data$"Molecular Weight"),]

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
  
  p <- data %>%
    ggplot(aes(x = pKow, y = `Molecular Weight`)) +
    theme_classic()+
    geom_point(aes(text=map(
      paste('<br>Compound:', `Name`, 
            '<br>pKow:', `pKow`, 
            '<br>Molecular Weight: ', `Molecular Weight`), 
      HTML)),
      size = 2, alpha = 0.7) +
    ylab("MW")+
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

