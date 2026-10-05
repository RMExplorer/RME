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
  theme = bs_theme(version = 5, preset = "flatly"),
  header = list(
  useShinyjs(),
  # splash screen shown immediately on load/reload
  # hidden once the initial content has rendered (see session$onFlushed in server.R)
  div(
    id = "loadingScreen",
    style = "position:fixed;top:0;left:0;right:0;bottom:0;z-index:99999;background-color:#ffffff;display:flex;flex-direction:column;align-items:center;justify-content:center;",
    img(src = "Icon.png", style = "height:18vh;"),
    h4("Loading Reference Material Explorer...", style = "margin-top:1rem;font-weight:bold;color:#5b7b7a;")
  ),
  tags$style(HTML("
          .navbar-nav {
              align-items: center;
              justify-content: flex-end;
          }
          .navbar {
              padding: 0px;
              background-color: #d5d8dc !important
          }
          .navbar-brand {
              color: #292d34 !important;
          }
          .navbar .nav-link:not(.active) {
              color: #292d34
          }
          body {
            padding-top: 70px;
          }
          .bslib-sidebar-layout .sidebar {
            background-color: #F5F6F8 !important;
          }
          .accordion-item {
            background-color: #F5F6F8 !important;
          }
          #crmTable, #customTable {
            font-size: 0.85rem;
          }
          ")),
  tags$script(HTML("
    var lastClickTime = 0;
    
    // expands/collapses a truncated table cell when its 'more'/'less' link is clicked
    $(document).on('click', '.cell-toggle', function(e){
      e.preventDefault();
      var span = $(this).closest('.cell-expand');
      var expanded = span.attr('data-expanded') === 'true';
      if (expanded) {
        span.html(decodeURIComponent(span.attr('data-short')) + ' <a href=\"#\" class=\"cell-toggle\">more</a>');
        span.attr('data-expanded', 'false');
      } else {
        span.html(decodeURIComponent(span.attr('data-full')) + ' <a href=\"#\" class=\"cell-toggle\">less</a>');
        span.attr('data-expanded', 'true');
      }
    });

    Shiny.addCustomMessageHandler('scrollToElement', function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        element.scrollIntoView({
          behavior: 'smooth',
          block: 'start'
        });
      }
    });
    
    $(document).on('click', '.view-info', function(e){
      e.preventDefault();
      var thisClickTime = new Date().getTime();
      if (thisClickTime - lastClickTime > 2000) {
        var name = $(this).data('name');
        Shiny.setInputValue('clicked_name', name, {priority: 'event'});
        lastClickTime = thisClickTime;
      }
    });
    
    // fire on press: the click's target can end up as the layout container if the table redraws mid-click
    document.addEventListener('mousedown', function(e) {
      if (e.button !== 0) return;
      var link = e.target.closest('.view-info2');
      if (!link) return;
      e.preventDefault();
      var now = new Date().getTime();
      if (now - lastClickTime > 2000) {
        Shiny.setInputValue('clicked_name', link.getAttribute('data-name'), {priority: 'event'});
        lastClickTime = now;
      }
    }, true);
    
    // swallow the follow-up click so the link doesn't also select the DT row
    document.addEventListener('click', function(e) {
      if (e.target.closest('.view-info2')) { e.preventDefault(); e.stopPropagation(); }
    }, true);
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
       theme = bs_theme(version = 5, preset = "flatly"),
       tags$head(htmltools::findDependencies(selectInput("toto", "toto", choices=NULL))), # needed for selectizeInput
       
       layout_sidebar(
         fillable = TRUE,
         sidebar = sidebar(
           width = "35%",
           open = "open",
           resizable = FALSE,
           title = "Search",
           accordion(
             id = "searchAccordion",
             open = "crm",
             accordion_panel(
               value = "crm",
               title = "CRM Search",
               uiOutput("crmSearch"),
             ),
             accordion_panel(
               value = "compound",
               title = "Compound Search",
               uiOutput("RMESearch")
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
             theme = bs_theme(version = 5, preset = "flatly"),
             div(HTML("<h3>Polarity <i>versus</i> Molecular Weight Plot </h3>"),
                 tooltip_ui("physico_chemical_instructions", 
                            "Shows all the substances in the Substances table in the General Search tab. To view only certain substances, select them from your table in the General Search tab and filter by 'Only Selected Analytes' in the dropdown below."),
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
             theme = bs_theme(version = 5, preset = "flatly"),
             uiOutput("about")                                                  #in about.R (loaded in server.R)
           )
  ),
  navbarMenu("More",
             tabPanel("CMC Information",
                      fluidPage(
                        theme = bs_theme(version = 5, preset = "flatly"),
                        DTOutput("kcdbTable")                                   #in kcdbserver.R (loaded in server.R)
                      )
             ),
             tabPanel("Instructions",
                      fluidPage(
                        theme = bs_theme(version = 5, preset = "flatly"),
                        uiOutput("instructions")                                #in instructions.R (loaded in server.R)
                      )
             ),
             align = "right"
  )
)