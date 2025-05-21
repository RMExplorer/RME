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
            hairline = TRUE
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
                    tags$img(src = 'Icon.png', style = "max-width: 90%; max-height: 600px;"),
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

                        div(
                            style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
                            div(
                                style = "display: flex; align-items: center; gap: 8px; padding: 10px",
                                tags$div(style = "font-weight: bold; font-size: 17px;", "Substance Table"),
                                f7Tooltip(
                                    f7Icon("info_circle"),
                                    text = "Search your Compound, Inchikey, IUPAC, or Keyword in the NRC Repository. If no results are found, will enquire the closest match from PubChem and search the repository again."
                                )
                            ),
                            div(
                                style = "display: flex; align-items: center;",
                                f7Button(inputId = "unselect", label = "Unselect All Rows", color = "red")
                            )
                        ),

                        f7Block(
                            div(
                                style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
                                uiOutput("searchAnalyte"),
                                f7Checkbox("additiveTable", label ="Add to the Table", TRUE),
                                f7Tooltip(
                                    f7Icon("info_circle"),
                                    text = "Un-select this if you want to remove all entries in the table before adding a new substance."
                                )
                            ),
                            
                            f7Flex(
                                style = "display: flex; align-items: center; gap: 8px;",

                                f7Button("removeAnalyte", "Remove Selected Rows", color = "orange"),
                                f7Button("removeAllAnalytes", "Remove All Rows", color = "red"),
                                f7Tooltip(
                                    f7Button(inputId = "addallSubstances", label = "Add All Substances", color = "green"),
                                    text = "May take up to 3+ minutes. Not all substances from the NRC repository will be added due to search limitations."
                                ),
                                f7Button(inputId = "saveAnalytes", label = "Save Table", color = "green"),
                                f7Button(inputId = "loadAnalytes", label = "Load Table", color = "blue")
                            ),
                            
                            uiOutput("RMESearch")
                        )
                    ),
                    
                    f7Tab(
                        tabName = "crm",
                        title = "CRM Search",

                        div(style = "float: right; width:15%", f7Button("unselectCRMs", "Unselect All Rows", color = "red")),

                        f7Block(
                            uiOutput("crmSearch")
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
                        )
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