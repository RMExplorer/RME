library(bslib)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
#library(shinyauthr) #for login/logout functionality
library(shinyjs) #to use js code easier with shiny
library(bsicons)                    #for icons
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
          .sidebar-title {
            padding: 0.25rem 1rem !important;
            margin-bottom: 0 !important;
          }
          body {
            padding-top: 70px;
          }
          ")),
  tags$script(HTML("
    var lastClickTime = 0;

    Shiny.addCustomMessageHandler('scrollToElement', function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.scrollIntoView({
          behavior: 'smooth',
          block: 'start'
        });
      }
    });
  "))
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
       tags$head(htmltools::findDependencies(selectInput("toto", "toto", choices=NULL))), # needed for selectizeInput
       
       layout_sidebar(
         fillable = TRUE,
         sidebar = sidebar(
           width = "40%",
           open = "open",
           title = "Search",
           accordion(
             id = "searchAccordion",
             open = "crm",
             accordion_panel(
               value = "crm",
               title = "CRM Search",
               uiOutput("crmSearch"),
               actionButton("unselectCRMs", "Unselect All Rows", class="btn-danger"),
             ),
             accordion_panel(
               value = "compound",
               title = "Compound Search",
               uiOutput("RMESearch"),
               actionButton("unselect", "Unselect All Rows", class="btn-danger")
             )
           )
         ),
         
         div(
           uiOutput("properties")
         )
       )
    )
  ),
  tabPanel("Polarity-MW Plot",
           fluidPage(
             theme = bs_theme(version = 5, bootswatch = "sandstone"),
             div(HTML("<h3>Polarity <i>versus</i> Molecular Weight Plot </h3>"),
                 tooltip_ui("physico_chemical_instructions", 
                            "Shows all the substamces in the Substances table in the General Search tab. To view only certain substances, select them from your table in the General Search tab and filter by 'Only Selected Analytes' in the dropdown below."),
                 style="display:flex;align-items:center;justify-content:center;"),
             div(uiOutput("plotNothingSelected"),                               #in physicoChemicalPlot.R (loaded in server.R)
                 style="display:flex;justify-content:center;font-size:2rem;"),
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
  tabPanel("About",
           fluidPage(
             theme = bs_theme(version = 5, bootswatch = "sandstone"),
             uiOutput("about")                                                  #in about.R (loaded in server.R)
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