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
