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
    dark = FALSE,
    init = f7Init(skin = "ios", theme = "light"),
    f7TabLayout(
        navbar = f7Navbar(
            title = "RM Explorer",
            hairline = TRUE,
            shadow = TRUE,
        )
    ),
)