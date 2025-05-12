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


ui <- f7Page(
    title = "Reference Material Explorer",
    options = list(
        dark = FALSE,
        theme = "ios"
    ),
    f7TabLayout(
        panels = tagList(
            f7Panel(
                id = "left-panel",
                title = "Left Panel",
                side = "left",
                effect = "cover",
                f7PanelMenu(
                    id = "menu",
                    f7PanelItem(
                        tabName = "home",
                        icon = f7Icon("house"),
                        title = "Home"
                    ),
                    f7PanelItem(
                        tabName = "search",
                        icon = f7Icon("magnifyingglass"),
                        title = "Search"
                    ),
                    f7PanelItem(
                        tabName = "about",
                        icon = f7Icon("info_circle"),
                        title = "About"
                    )
                )
            )
        ),

        navbar = f7Navbar(
            title = "Reference Material Explorer",
            hairline = TRUE,
            shadow = TRUE
        ),
    
        f7Tabs(
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
                icon = f7Icon("magnifyingglass"),
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
                tabName = "about",
                icon = f7Icon("info_circle"),
                title = "About",
                "This app helps you explore reference materials."
            )
        )
    )
)