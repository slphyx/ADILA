# SLPHYX@SHIFT-ENTER

# R/server/app_server.R

app_server <- function(input, output, session) {
  
  # Dynamic rendering of disease tabs
  output$drug_tabs <- renderUI({
    generate_disease_tabs()  # Calling a function from utility_functions.R
  })
  
  # Simulation logic to handle inputs
  observeEvent(input$submit, {
    vectors <- list()
    
    for (i in 1:10) {
      vectors[[i]] <- handle_disease_inputs(input, i)  # Call to function from utility_functions.R
    }
    
    # Combine results from all diseases
    combined_result <- vector_to_table_multi(vectors)
    
    # Render the combined results as a table
    output$model_output <- renderTable({
      combined_result
    })
  })
  
  # Save the current input parameters to a file (JSON format)
  output$save_params <- downloadHandler(
    filename = function() {
      paste("input_parameters_", Sys.Date(), ".json", sep = "")
    },
    content = function(file) {
      params <- list()
      
      # Loop through all input parameters and save the current values
      for (i in 1:10) {
        params[[paste0("disease_", i)]] <- save_disease_inputs(input, i)  # Save inputs function
      }
      
      # Write parameters to JSON file
      jsonlite::write_json(params, file)
    }
  )
  
  # Load the input parameters from a file
  observeEvent(input$load_params, {
    req(input$load_params)
    params <- jsonlite::fromJSON(input$load_params$datapath)
    
    # Load parameters into the input fields
    for (i in 1:10) {
      load_disease_inputs(params[[paste0("disease_", i)]], i, session)  # Load inputs function
    }
  })
}

