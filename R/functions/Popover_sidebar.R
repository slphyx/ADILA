Popover_sidebar <- function(id,label, content) {
  fluidRow(
    div(
      style = "display: flex; align-items: center;",
      div(id=id,class="form-group shiny-input-container poph5",tags$h5(label ))
    ),
    bsPopover(id = id, title = NULL,
              content = content,
              placement = "Right", trigger = "click"),
  )
}
