# load library
library(gtools)

source("R/functions/model.adult.shiny.v3.R")
source("R/functions/data.adult.shiny.v3.R")
source("R/functions/generate_input_dataframe.R")

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
        uiOutput(w),
        uiOutput(x),
        uiOutput(y),
        uiOutput(z)
      
    )
  )
}


finished <- 0   # for checking whether the simulation has finished ; 0 <- not finished 1 <- finished