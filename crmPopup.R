## crmPopup.R
## Opens a modal when a CRM link is clicked; links are handled by JavaScript in ui.R which sets input$clicked_name

# define analyteTable as reactiveVal so that it can change between null and a dataframe
analyteTable <- reactiveVal(NULL)

#waits for a click to a crm and displays the corresponding popup
observeEvent(input$clicked_name, {
  name <- input$clicked_name
  req(!is.null(name), length(name) == 1, !is.na(name), nzchar(name))
  
  con <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  data <- DBI::dbGetQuery(con, "
                                SELECT rmid, summary, doi, date
                                FROM reference_materials
                                WHERE name = ?", params = list(name))
  req(nrow(data) == 1)
  
  rmid <- data$rmid
  req(!is.na(rmid), nzchar(as.character(rmid)))
  
  tbl <- DBI::dbGetQuery(con, "
                              SELECT name, quantity, value, uncertainty, unit, type
                              FROM analyte_tables
                              WHERE rmid = ?", params = list(rmid))
  
  # set analyte table data to null, unless crm contains analyte table
  if(nrow(tbl) > 0){
    analyteTable(tbl)
  } else {
    analyteTable(NULL)
  }
  
  abstract <- data$summary
  pubDate <- data$date
  doi <- data$doi
  
  if (is.na(abstract)) abstract <- ""
  if (is.na(pubDate)) pubDate <- ""
  if (is.na(doi) || !nzchar(trimws(doi))) {
    doi <- NULL
  } else {
    doi <- paste0("https://doi.org/", doi)
  }
  
  #display the information in a popup modal
  showModal(modalDialog(
    title = paste("Information on: ", name),
    size = "l",
    p(abstract),
    a(paste("DOI:", doi), href=doi, target="_blank"),
    p(pubDate),
    uiOutput("popupanalyteTableUI"),
    uiOutput("showDownloadButton"),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
})

output$popupanalyteTableUI <- renderUI({
  if (!is.null(analyteTable())) {
    DTOutput("popupanalyteTable")
  }
})

output$popupanalyteTable <-renderDT({
  req(!is.null(analyteTable()))
  datatable(analyteTable(), options = list(pageLength = 10, responsive = FALSE, scrollX = TRUE))
})

output$downloadAnalyteTable <- downloadHandler(
  filename = function(){
    "Analyte Table.csv"
  },
  content = function(file) {
    tbl <- analyteTable()
    req(!is.null(tbl))
    write.csv(tbl, file, row.names = FALSE)
  }
)

#only render the download button if the analyte table exists
output$showDownloadButton <- renderUI({
  if (is.null(analyteTable())) {
    return(NULL)
  }
  downloadButton("downloadAnalyteTable", "Download the table")
})