# SLPHYX@SHIFT-ENTER

# Define the server logic
app_server <- function(input, output) {
  
  # Reactive expression to create the input data based on the user input
  inputData <- reactive({
    pt.atb <- input$pt_bmen + input$pt_uuti
    
    data.frame(
      pt.admt = input$pt_admt,
      pt.atb  = pt.atb,
      p.first = input$p_first,
      p.bmen  = input$pt_bmen / pt.atb,
      p.uuti  = input$pt_uuti / pt.atb,
      p.sev.uti = input$p_sev_uti,
      p.esbl = input$p_esbl
    )
  })
  
  # Observe the submit button and update the model output
  observeEvent(input$submit, {
    output$model_output <- renderTable({
      # run the tree_adult function
      vector_to_table(tree_adult(inputData()))
      
    })
  })
}
