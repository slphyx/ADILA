# SLPHYX@SHIFT-ENTER

library(shiny)

source("R/global.R")


source("R/ui/app_ui.R")
source("R/server/app_server.R")

# Run the Shiny application
shinyApp(ui = app_ui, server = app_server)
