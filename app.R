# SLPHYX@SHIFT-ENTER

#app.R
library(shiny)
library(dplyr)
library(tidyr)
library(gtsummary)
library(shinyjs)
library(shinycssloaders)
library(shinyBS)

source("R/global.R")

source("R/ui/app_ui.R")
source("R/server/app_server.R")

# Run the Shiny application
shinyApp(ui = app_ui, server = app_server)
