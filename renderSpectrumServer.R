#the dropdown ui in the 'Spectral Data' page
output$spectrumDropdown <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  spectrums <- c("Select an option", v$spectralData$Name[!is.na(v$spectralData$SpectralLink)])
  
  selectizeInput(
    "spectrumSelection",
    "Select Spectrum below:",
    choices = spectrums)
})

#when none or more than 1 analyte is selected, show this information
output$spectrumNothingSelected <- renderUI({
  req(length(input$customTable_rows_selected) != 1)
  HTML(paste(
    "<p>Make sure you have selected a <strong>single</strong> option from the table in the 'General Search' tab."
  ))
})

#outputs the spectral graph and the download button
output$spectrumView <- renderUI({
  req(length(input$customTable_rows_selected) == 1)
  list(
    tags$div(
      uiOutput("spectrumDropdown"),
      tags$div(downloadButton("downloadSpectrumData", "Download the Data")),
      style="display: flex; justify-content: space-between; padding: 2rem;"
    ),
    conditionalPanel(
      "$('#spectrum').hasClass('recalculating') | $('#spectrum').css('display') === 'none'", 
      tags$div(img(src='loading.gif', style = "height: 4rem;"), 
               style="display:flex;justify-content:center;")),
    tags$div(plotlyOutput("spectrum", height = "70vh"), style="padding:15px;")
  )
})

#renders the spectral graph plot based on which option was selected from the dropdown
output$spectrum <- renderPlotly({
  shinyjs::hide("downloadSpectrumData")
  req(input$spectrumSelection)
  req(length(input$customTable_rows_selected) == 1)
  
  if (input$spectrumSelection == "Select an option") {
    output$spectrumStatus <- renderText({})
  } 
  
  #if the dropdown option has the text mass spectrum in it
  if (grepl("mass spectrum", input$spectrumSelection, ignore.case = TRUE) | 
      grepl("full scan", input$spectrumSelection, ignore.case = TRUE) |
      grepl("msms", input$spectrumSelection, ignore.case = TRUE) | 
      grepl("ms/ms", input$spectrumSelection, ignore.case = TRUE)) {
    req(v$spectralData$SpectralLink[match(input$spectrumSelection, v$spectralData$Name)])
    link <- v$spectralData$SpectralLink[match(input$spectrumSelection, v$spectralData$Name)]
    tmpfile <- tempfile(fileext = ".csv")
    curl::curl_download(link, tmpfile, handle = curl::new_handle(ssl_verifypeer = FALSE))
    data <- data.frame(read.csv(tmpfile))

    if(!is.null(data)) {
      metaData <- data[1:20,]
      spectra <- data[-c(1:(grep("Mass_to_charge", metaData[,1], ignore.case = TRUE)+1)),]
      spectra <- mutate_all(spectra, function(x) as.numeric(as.character(x)))
      colnames(spectra) <- c("Mass", "Intensity")
      
      resolution <- ifelse(any(grepl("Resolution", metaData[,1], ignore.case = TRUE)), 
                           metaData[grep("Resolution", metaData[,1], ignore.case = TRUE), 2], "no resolution")
      decimalPlaces <- ifelse(grepl(resolution, "high", ignore.case = TRUE), 4, 1)
      p <- ggplot(spectra, aes(x = `Mass`, y = `Intensity`)) +
        theme_classic()+
        stat_label_peaks(angle = 90, color ="black",
                         ignore_threshold = 0.05,
                         x.label.fmt = paste0("%.", decimalPlaces, "f", sep = "")) +
        geom_line(aes(text=map(paste('Mass to charge ratio: ', round(`Mass`, 3),
                                     '<br>Relative Intensity (%):',
                                     `Intensity`),
                               HTML))) +
        ylab("% Relative Intensity") +
        xlab("m/z") +
        theme(axis.title.x = element_text(face = 'italic')) +
        theme(legend.position="none")
  
      p_plotly <- ggplotly(p, tooltip=c("text"))

      if (!(shinybrowser::get_device() == "Mobile")) {
        p_plotly <- p_plotly %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=1,
                         text=ifelse(any(grepl("DOI", metaData[,1], ignore.case = TRUE)), 
                                     paste("DOI:", metaData[grep("DOI", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F) %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=0.96,
                         text=ifelse(any(grepl("Substance", metaData[,1], ignore.case = TRUE)), 
                                     paste("Substance:", metaData[grep("Substance", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F) %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=0.92,
                         text=ifelse(any(grepl("InChIKey", metaData[,1], ignore.case = TRUE)), 
                                     paste("InChIKey:", metaData[grep("InChIKey", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F) %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=0.88,
                         text=ifelse(any(grepl("Instrument", metaData[,1], ignore.case = TRUE)), 
                                     paste("Instrument:", metaData[grep("Instrument", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F) %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=0.84,
                         text=ifelse(any(grepl("Resolution", metaData[,1], ignore.case = TRUE)), 
                                     paste("Resolution:", metaData[grep("Resolution", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F) %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=0.80,
                         text=ifelse(any(grepl("Type of Data", metaData[,1], ignore.case = TRUE)), 
                                     paste("Type of Data:", metaData[grep("Type of Data", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F) %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=0.76,
                         text=ifelse(any(grepl("Parent Ion", metaData[,1], ignore.case = TRUE)), 
                                     paste("Parent Ion:", metaData[grep("Parent Ion", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F) %>%
        add_annotations( xref = "paper",
                         yref = "paper",
                         x=1,
                         y=0.72,
                         text=ifelse(any(grepl("Collision Energy", metaData[,1], ignore.case = TRUE)), 
                                     paste("Collision Energy:", metaData[grep("Collision Energy", metaData[,1], ignore.case = TRUE), 2]), ""),
                         showarrow = F)
      
        output$spectrumStatus <- renderText({})
      }
       
      
      #download fsms data
      output$downloadSpectrumData <- downloadHandler(
        filename = function() {
          paste("fsms.csv")
        },
        content = function(file) {
          write.csv(spectra, file, row.names = FALSE)
        }
      )
      shinyjs::show("downloadSpectrumData")
      
      return(p_plotly)
    }
  } else if (grepl("NMR", input$spectrumSelection, ignore.case = FALSE)) {
    req(v$spectralData$SpectralLink[match(input$spectrumSelection, v$spectralData$Name)])
    link <- v$spectralData$SpectralLink[match(input$spectrumSelection, v$spectralData$Name)]
    tmpfile <- tempfile(fileext = ".csv")
    curl::curl_download(link, tmpfile, handle = curl::new_handle(ssl_verifypeer = FALSE))
    data <- data.frame(read.csv(tmpfile))
    
    if(!is.null(data)) {
      metaData <- data[1:20,]
      spectra <- data[-c(1:(grep("Intensity", metaData[,1], ignore.case = TRUE)+1)),]

      spectra <- mutate_all(spectra, function(x) as.numeric(as.character(x)))
      colnames(spectra) <- c("Chemical Shift", "Intensity")
      freq <- metaData[grep("Frequency", metaData[,1], ignore.case = TRUE), 2]
      spectra$Frequency <-  spectra$"Chemical Shift" * as.numeric(freq[[1]])
      
      p <- spectra %>%
        ggplot(aes(x = `Chemical Shift`, y = `Intensity`)) +
        theme_classic() +
        stat_label_peaks( angle = 90, color ="black", 
                          ignore_threshold = 0.05, 
                          x.label.transform = abs, 
                          span = 9) +
        geom_line(aes(text=map(paste('Intensity:', 
                                     `Intensity` ,'<br>Chemical Shift (ppm): ', 
                                     round(`Chemical Shift`, 2), 
                                     '<br>Chemical Shift (Hz):', 
                                     round(`Frequency`, 1)), 
                               HTML))) +              
        ylab("Intensity") +
        xlab("Chemical Shift (ppm)") +
        scale_x_continuous(trans = "reverse", 
                           breaks=seq(14,0, by=-1), 
                           labels=seq(14,0, by=-1), 
                           limits = c(14,0)) +
        theme(legend.position="none")
      
      p_plotly <- ggplotly(p +ylim(-0.05*max(spectra$Intensity), NA), tooltip=c("text"))
      
      if (!(shinybrowser::get_device() == "Mobile")) {
          p_plotly <- p_plotly %>%
          add_annotations( xref = "paper",
                          yref = "paper",
                          x=1,
                          y=1,
                          text=ifelse(any(grepl("DOI", metaData[,1], ignore.case = TRUE)), 
                                      paste("DOI:", metaData[grep("DOI", metaData[,1], ignore.case = TRUE), 2]), ""),
                          showarrow = F) %>%
          add_annotations( xref = "paper",
                          yref = "paper",
                          x=1,
                          y=0.96,
                          text=ifelse(any(grepl("Substance", metaData[,1], ignore.case = TRUE)), 
                                      paste("Substance:", metaData[grep("Substance", metaData[,1], ignore.case = TRUE), 2]), ""),
                          showarrow = F) %>%
          add_annotations( xref = "paper",
                          yref = "paper",
                          x=1,
                          y=0.92,
                          text=ifelse(any(grepl("InChIKey", metaData[,1], ignore.case = TRUE)), 
                                      paste("InChIKey:", metaData[grep("InChIKey", metaData[,1], ignore.case = TRUE), 2]), ""),
                          showarrow = F) %>%
          add_annotations( xref = "paper",
                          yref = "paper",
                          x=1,
                          y=0.88,
                          text=ifelse(any(grepl("Instrument", metaData[,1], ignore.case = TRUE)), 
                                      paste("Instrument:", metaData[grep("Instrument", metaData[,1], ignore.case = TRUE), 2]), ""),
                          showarrow = F) %>%
          add_annotations( xref = "paper",
                          yref = "paper",
                          x=1,
                          y=0.84,
                          text=ifelse(any(grepl("Frequency", metaData[,1], ignore.case = TRUE)), 
                                      paste("Frequency:", metaData[grep("Frequency", metaData[,1], ignore.case = TRUE), 2]), ""),
                          showarrow = F) %>%
          add_annotations( xref = "paper",
                          yref = "paper",
                          x=1,
                          y=0.80,
                          text=ifelse(any(grepl("Type of Data", metaData[,1], ignore.case = TRUE)), 
                                      paste("Type of Data:", metaData[grep("Type of Data", metaData[,1], ignore.case = TRUE), 2]), ""),
                          showarrow = F) %>%
          add_annotations( xref = "paper",
                          yref = "paper",
                          x=1,
                          y=0.76,
                          text=ifelse(any(grepl("solvent", metaData[,1], ignore.case = TRUE)), 
                                      paste("Solvent:", metaData[grep("solvent", metaData[,1], ignore.case = TRUE), 2]), ""),
                          showarrow = F) 
        
        output$spectrumStatus <- renderText({})
      }

      
      
      #download nmr data
      output$downloadSpectrumData <- downloadHandler(
        filename = function() {
          paste("nmr.csv")
        },
        content = function(file) {
          write.csv(spectra, file, row.names = FALSE)
        }
      )
      shinyjs::show("downloadSpectrumData")
      
      return(p_plotly)
    }
  } else {
    return(NULL)
  }
})

