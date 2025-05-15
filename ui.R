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
library(shinybrowser)
source("tooltip_ui.R")

ui <- f7Page(
    shinybrowser::detect(),
    title = "Reference Material Explorer",
    options = list(
        dark = FALSE,
        theme = "ios"
    ),
    f7TabLayout(
        navbar = f7Navbar(
            title = "Reference Material Explorer",
            hairline = TRUE,
            shadow = TRUE
        ),
    
        f7Tabs(
            id = "mainTabs",
            animated = TRUE,
            f7Tab(
                tabName = "home",
                icon = f7Icon("house"),
                title = "Home",
                active = TRUE,
                
                f7Block(
                    strong = TRUE,
                    inset = TRUE,
                    style = "text-align: center;",
                    tags$img(src = 'Icon.png', style = "max-width: 90%; height: auto;"),
                    tags$h1("Reference Material Explorer", style = "font-weight: bold;")
                ),
                
                f7Block(
                    strong = TRUE,
                    inset = TRUE,
                    style = "margin-top: 2rem;",
                    tags$h3(
                    "The RM Explorer is an application built upon the NRC Digital Repository external Application Programming Interfaces (APIs) 
                    that allows users to visualise, analyse and display useful information about the Reference Materials produced by the National 
                    Research Council of Canada. This application relies upon and complies with FAIR data principles and showcases multiple uses of 
                    machine-readable information in digital CRM certificates.",
                    style = "font-weight: normal;"
                    )
                ),
                
                f7Block(
                    uiOutput("about")
                )
            ),

            f7Tab(
                tabName = "search",
                icon = f7Icon("search"),
                title = "Search",
                
                f7Tabs(
                    id = "searchTabs",
                    f7Tab(
                        tabName = "general",
                        title = "General Search",
                        active = TRUE,

                        tags$head(
                            htmltools::findDependencies(selectInput("toto", "toto", choices = NULL))  # Ensures selectizeInput loads properly
                        ),

                        f7BlockTitle("Substance Table"),
                        f7Block(
                            uiOutput("RMESearch")
                        ),

                        absolutePanel(
                            top = 100, right = 0, width = 200,
                            style = "border:none;border-radius:25px;display:flex;justify-content:center;",
                            actionButton("unselect", "Unselect All Rows", class = "btn-danger")
                        )
                    ),
                    
                    f7Tab(
                        tabName = "crm",
                        title = "CRM Search",

                        f7Block(
                            uiOutput("crmSearch")
                        ),

                        absolutePanel(
                            top = 100, right = 0, width = 200,
                            style = "border:none;border-radius:25px;display:flex;justify-content:center;",
                            actionButton("unselectCRMs", "Unselect All Rows", class = "btn-danger")
                        )
                    )
                )
            ),

            f7Tab(
                tabName = "properties",
                icon = f7Icon("list_bullet"),
                title = "Properties",
                f7Block(
                    uiOutput("properties")
                )
            ),

            f7Tab(
                tabName = "plot",
                icon = f7Icon("chart_bar"),
                title = "Polarity-MW Plot",
                
                f7BlockTitle("Polarity versus Molecular Weight Plot"),

                f7Block(
                    strong = TRUE,
                    inset = TRUE,
                    div(
                    HTML("<h3 style='text-align:center;'>Polarity <i>vs.</i> Molecular Weight</h3>"),
                    tooltip_ui("physico_chemical_instructions", 
                        "Shows all the substances in the Substances table in the General Search tab. To view only certain substances, select them from your table in the General Search tab and filter by 'Only Selected Analytes' in the dropdown below."),
                    style = "display:flex;flex-direction:column;align-items:center;"
                    )
                ),

                f7Card(
                    plotlyOutput("plot", height = "70vh"),  # width is not necessary, mobile will scale
                    class = "no-margin"
                ),

                f7Block(
                    uiOutput("plotFilters"),
                    style = "display:flex; justify-content:center;"
                ),

                f7Block(
                    uiOutput("compoundList")
                ),

                f7Block(
                    em("About the plot sectors: The quadrants represented on this plot follow the categories that were historically used by the Organic Analysis Working Group of the Consultative Committee for Amount of Substance (CCQM-OAWG). The Low/High Molecular Weight boundary is set at 500 and low/high polarity at pKow = -2. The OAWG has more recently eliminated the low/high polarity classification for the high MW quadrant but we chose to keep it in this app as we believe it provides valuable information."),
                    style = "font-size: 0.9rem;"
                )

            ),

            f7Tab(
                tabName = "spectrum",
                icon = f7Icon("waveform_path"),
                title = "Spectral Data",
                
                f7Block(
                    uiOutput("spectrumUI")
                )
            ),

            f7Tab(
                tabName = "more",
                icon = f7Icon("archivebox"),
                title = "More",
                
                f7Tabs(
                    id = "moreTabs",
                    f7Tab(
                        tabName = "info",
                        title = "CMC Information",
                        active = TRUE,

                        f7BlockTitle("CMC Information"),
                        f7Block(
                            DTOutput("kcdbTable")
                        ),
                    ),
                    
                    f7Tab(
                        tabName = "instructions",
                        title = "Instructions",

                        f7Block(
                            uiOutput("instructions")
                        )
                    )
                )
            )
        )
    )
)