library(bslib)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyauthr) #for login/logout functionality
library(shinyjs) #to use js code easier with shiny
library(bsicons)                    #for icons
library(shinyMobile)
source("tooltip_ui.R")

ui <- navbarPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  header = list(
  useShinyjs(),
  tags$style(HTML("
          .navbar-nav {
              align-items: center;
              justify-content: flex-end;
          }
          .navbar {
              padding: 0px;
          }
          body {
            padding-top: 70px;
          }
          ")),
  tags$script(HTML("var lastClickTime = 0;"))
  ),
  id="tabs",
  position = "fixed-top",
  collapsible = TRUE,
  title = tags$span(
    img(src='Icon.png', style = "height: 3rem; width:3rem;margin:10px"), 
    style="display:inline-flex;",
    p("Reference Material Explorer", 
      style="padding: 0px 0px 0px 15px; align-self: center; margin:0px")),
  #p(em("V 0.3 - NRC Biotoxin Metrology")),
  tabPanel("Home",
     fluidPage(
       theme = bs_theme(version = 5, bootswatch = "sandstone"),
       br(),
       div(
         img(src='Icon.png', style = "height: 70vh; width:70vh;"),
         h1("Reference Material Explorer", style= "font-weight: bold;"),
         style="display:flex;flex-direction:column;align-items:center;"
       ),
       br(),br(),br(),br(),br(),
       h3("The RM Explorer is an application built upon the NRC Digital Repository external Application Programming Interfaces (APIs) 
         that allows users to visualise, analyse and display useful information about the Reference Materials produced by the National 
         Research Council of Canada. This application relies upon and complies with FAIR data principles and showcases multiple uses of 
         machine-readable information in digital CRM certificates.", style="font-weight: 100;"),
     ),
     br(), br(), hr(), 
     uiOutput("about")                                                          #in about.R (loaded in server.R)
  ),
  navbarMenu("Search",
             tabPanel("General Search",
                      fluidPage(
                        tags$head(htmltools::findDependencies(selectInput("toto", "toto", choices=NULL))), #line needed or else selectizeinput will not work properly
                        theme = bs_theme(version = 5, bootswatch = "sandstone"),
                        br(),
                        h3("Substance Table"),
                        uiOutput("RMESearch"),                                  #in compoundsUI.R (loaded in server.R)
                        fixedPanel(
                          actionButton("unselect", "Unselect All Rows",         #in compoundsServer.R (loaded in server.R)
                                       class="btn-danger", 
                                       style="justify-content:center;"),
                          style="border:none;border-radius:25px;display:flex;",
                          width = "200px", 
                          right = "0px", 
                          top = "100px"
                        )
                      )
             ),
             tabPanel("CRM Search",
                      fluidPage(
                        theme = bs_theme(version = 5, bootswatch = "sandstone"),
                        uiOutput("crmSearch"),                                  #in crmSearchUI.R (loaded in server.R) (loaded in server.R)
                        fixedPanel(
                          actionButton("unselectCRMs", "Unselect All Rows",     #in crmSearchServer.R (loaded in server.R)
                                       class="btn-danger", 
                                       style="justify-content:center;"),
                          style="border:none;border-radius:25px;display:flex;",
                          width = "200px", 
                          right = "0px", 
                          top = "100px"
                        )
                      )
             )
  ),
  tabPanel("Properties",
           fluidPage(
             theme = bs_theme(version = 5, bootswatch = "sandstone"),
             uiOutput("properties"),                                            #in propertiesUI.R
           )
  ),
  tabPanel("Polarity-MW Plot",
           fluidPage(
             theme = bs_theme(version = 5, bootswatch = "sandstone"),
             div(HTML("<h3>Polarity <i>versus</i> Molecular Weight Plot </h3>"),
                 tooltip_ui("physico_chemical_instructions", 
                            "Shows all the substamces in the Substances table in the General Search tab. To view only certain substances, select them from your table in the General Search tab and filter by 'Only Selected Analytes' in the dropdown below."),
                 style="display:flex;align-items:center;justify-content:center;"),
             div(plotlyOutput("plot", height = "70vh", width="85vw"),           #in physicoChemicalPlot.R (loaded in server.R)
                 style="display:flex;justify-content:center;"),          
             div(uiOutput("plotFilters"),                                       #in physicoChemicalPlot.R (loaded in server.R)
                 style="display:flex; justify-content:center;"),                 
             uiOutput("compoundList"),                                          #in physicoChemicalPlot.R (loaded in server.R)
             br(),
             em("About the plot sectors: The quadrants represented on this plot follow the categories that were historically used by the Organic Analysis Working Group of the Consultative Committee for Amount of Substance (CCQM-OAWG). The Low/High Molecular Weight boundary is set at 500 and low/high polarity at pKow = -2. The OAWG has more recently eliminated the low/high polarity  classification for the high MW quadrant but we chose to keep it in this app as we believe it provides valuable information."),
             br(), br(), br()
           )
  ),
  tabPanel("Spectral Data",
           fluidPage(
             theme = bs_theme(version = 5, bootswatch = "sandstone"),
             tags$div(uiOutput("spectrumUI"))                                   
           )
  ),
  navbarMenu("More",
             tabPanel("CMC Information",
                      fluidPage(
                        theme = bs_theme(version = 5, bootswatch = "sandstone"),
                        DTOutput("kcdbTable")                                   #in kcdbserver.R (loaded in server.R)
                      )
             ),
             tabPanel("Instructions",
                      fluidPage(
                        theme = bs_theme(version = 5, bootswatch = "sandstone"),
                        uiOutput("instructions")                                #in instructions.R (loaded in server.R)
                      )
             ),
             align = "right"
  )
)