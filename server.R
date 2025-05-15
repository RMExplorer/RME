library(bslib)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(rcdk)
library(rinchi)
library(PubChemR)                   #for pubchem
library(rvest)                      #used to pull data from DOI
library(xml2)                       #for parsing xml
library(purrr)
library(ggspectra)                  #an extension of ggplot, used to label peaks in graphs
library(shinyjs)                    #to use js code easier with shiny
library(httr)                       #for doi access
library(bsicons)                    #for icons
library(parallel)                   #in order to do parallel web scraping
library(jsonlite)                   #to work with json content
library(webchem)
library(curl)
library(future)                     #in order to run long processes
library(promises)                   #in order to run long processes
plan(multisession, workers = 3)
source("tooltip_ui.R")


# server  ----
server <- function(input, output, session) {
  
  #stores all the UI for the general search page
  source("compoundsUI.R", local = TRUE)$value
  
  #stores all the code for general search page
  source("compoundsServer.R", local = TRUE)$value
  
  #stores all the UI for the crm search page
  source("crmSearchUI.R", local = TRUE)$value
  
  #stores all the code for crm search page
  source("crmSearchServer.R", local = TRUE)$value
  
  #stores the properties UI
  source("propertiesUI.R", local = TRUE)$value
  
  #stores the properties calculations
  source("propertiesServer.R", local = TRUE)$value
  
  #code that displays the plot in the Physico-chemical properties tab (stored in physicoChemicalPlot.R) 
  source("physicoChemicalPlot.R", local = TRUE)$value
  
  #stores all the code to render the spectrum user interface
  source("renderSpectrumUI.R", local = TRUE)$value
  
  #stores all the calculation for the spectrum user interface
  source("renderSpectrumServer.R", local = TRUE)$value
  
  #renders the table in the CMC Information page
  source("kcdbserver.R",  local = TRUE)$value
  
  #renders the crm popup code
  source("crmPopup.R", local = TRUE)$value
  
  #renders the text in the about page
  source("about.R",  local = TRUE)$value
  
  #renders the content in the instruction page
  source("instructions.R",  local = TRUE)$value
}
