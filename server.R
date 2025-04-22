source("tooltip_ui.R")
#useless addition
# server  ----
function(input, output, session) {
  #reactivepoll code: to pull data from google sheets, stored in reactivePolls.R
  source("reactivePolls.R", local = TRUE)$value
  
  #stores all the code for the login functionality
  source("loginCode.R", local = TRUE)$value
  
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
