
#javascript code to work with crm popup links in the crm search tab
output$js_code <- renderUI({
  tags$script(HTML("$(document).on('click', '.view-info', function(e){
                 e.preventDefault();
                 var thisClickTime = new Date().getTime();
                 if (thisClickTime - lastClickTime > 2000) {
                    var name = $(this).data('name');
                    Shiny.setInputValue('clicked_name', name, {priority: 'event'});
                    lastClickTime = thisClickTime;
                  }
                });"
  ))
})

#javascript code to work with crm popup links in the general search
output$js_code2 <- renderUI({
  tags$script(HTML("$(document).on('click', '.view-info2', function(e){
                 e.preventDefault();
                 var thisClickTime = new Date().getTime();
                 if (thisClickTime - lastClickTime > 2000) {
                    var name = $(this).data('name');
                    Shiny.setInputValue('clicked_name', name, {priority: 'event'});
                    lastClickTime = thisClickTime;
                  }
                });"
  ))
})

#waits for a click to a crm and displays the corresponding popup
observeEvent(input$clicked_name, {
  name <- input$clicked_name
  
  #search repository for id
  data <- recordDF[recordDF$crm %in% name,]
  id <- data$id
  
  req(id)
  
  #use id to get a link to the digital repository entry
  link <- paste("https://nrc-digital-repository.canada.ca/eng/view/object/?id=", id, sep="")

  #use doi content to get information (title, abstract, table, doi)
  #overrides the ssl verifypeer so the webpage can be reached
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = 0)
  ddf = rvest::html_table(html_nodes(read_html(geturl(link, h)),'table'))
  rm(h)
  #sets analyte table data to null, unless crm contains analyte table
  analyteTable <- NULL
  if(length(ddf) >= 3 & grepl('Analyte',paste(ddf[3]))){
    analyteTable <- data.frame(ddf[[3]])
  }
  
  table <- as.data.frame(ddf[[1]])
  abstract <- table[table$X1 == "Abstract", "X2"]
  pubDate <- table[table$X1 == "Publication date", "X2"]
  doi <- table[table$X1 == "DOI", "X2"]
  
  output$popupanalyteTable <- renderDT({
    datatable(analyteTable, options = list(pageLength = 10, responsive = FALSE, scrollX = TRUE))
  })
  
  output$downloadAnalyteTable <- downloadHandler(
    filename = function(){
      paste("Analyte Table.csv")
    },
    content = function(file) {
      write.csv(analyteTable, file, row.names = FALSE)
    }
  )
  
  #only render the download button if the analyte table exists
  if(is.null(analyteTable)) {
    output$showDownloadButton <- renderUI({ })
  } else {
    output$showDownloadButton <- renderUI({
      downloadButton("downloadAnalyteTable", "Download the table")
    })
  }
  
  #display the information in a popup modal
  showModal(modalDialog(
    title = paste("Information on: ", name),
    size = "l",
    p(abstract),
    a(paste("DOI:", gsub("Resolve DOI: ", "", doi)), href=gsub("Resolve DOI: ", "", doi), target="_blank"),
    p(pubDate),
    DTOutput("popupanalyteTable"),
    uiOutput("showDownloadButton"),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
})