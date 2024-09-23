# SLPHYX@SHIFT-ENTER

# Define the server logic
# app_server <- function(input, output) {
#   
#   # Reactive expression to create the input data based on the user input
#   inputData <- reactive({
#     pt.atb <- input$pt_bmen + input$pt_uuti
#     
#     data.frame(
#       pt.admt = input$pt_admt,
#       pt.atb  = pt.atb,
#       p.first = input$p_first,
#       p.bmen  = input$pt_bmen / pt.atb,
#       p.uuti  = input$pt_uuti / pt.atb,
#       p.sev.uti = input$p_sev_uti,
#       p.esbl = input$p_esbl
#     )
#   })
#   
#   # Observe the submit button and update the model output
#   observeEvent(input$submit, {
#     output$model_output <- renderTable({
#       # run the tree_adult function
#       vector_to_table(tree_adult(inputData()))
#       
#     })
#   })
# }


app_server <- function(input, output, session) {
  
  # Dynamically generate tabs for input fields based on the number of drugs
  output$drug_tabs <- renderUI({
    num_drugs <- input$num_drugs
    
    tab_list <- lapply(1:num_drugs, function(i) {
      tabPanel(
        paste("Drug", i),  # Create a tab for each drug
        numericInput(paste0("p_first_", i), "Probability of First Choice Antibiotic (p.first)", value = 0.90, min = 0, max = 1, step = 0.01),
        numericInput(paste0("p_esbl_", i), "Prevalence of ESBL Infection (p.esbl)", value = 0.30, min = 0, max = 1, step = 0.01),
        numericInput(paste0("pt_admt_", i), "Total Admitted Patients (pt.admt)", value = 150, min = 1, max = 1000, step = 1),
        numericInput(paste0("pt_bmen_", i), "Patients with Bacterial Meningitis (pt.bmen)", value = 50, min = 1, max = 1000, step = 1),
        numericInput(paste0("pt_uuti_", i), "Patients with Upper UTI (pt.uuti)", value = 50, min = 1, max = 1000, step = 1),
        numericInput(paste0("p_sev_uti_", i), "Proportion of Severe Upper UTI (p.sev.uti)", value = 0.2, min = 0, max = 1, step = 0.01)
      )
    })
    
    do.call(tabsetPanel, tab_list)  # Return a tabsetPanel containing the drug tabs
  })
  
  # When submit is clicked, calculate the model output
  observeEvent(input$submit, {
    
    num_drugs <- input$num_drugs
    vectors <- list()
    
    # Loop through each drug and calculate the result using tree_adult()
    for (i in 1:num_drugs) {
      # Gather the inputs for each drug
      pt_admt <- input[[paste0("pt_admt_", i)]]
      pt_bmen <- input[[paste0("pt_bmen_", i)]]
      pt_uuti <- input[[paste0("pt_uuti_", i)]]
      p_first <- input[[paste0("p_first_", i)]]
      p_esbl <- input[[paste0("p_esbl_", i)]]
      p_sev_uti <- input[[paste0("p_sev_uti_", i)]]
      
      pt_atb <- pt_bmen + pt_uuti  # Total patients on antibiotics
      
      # Create input data frame for tree_adult()
      input_data <- data.frame(
        pt.admt = pt_admt,
        pt.atb = pt_atb,
        p.first = p_first,
        p.bmen = pt_bmen / pt_atb,
        p.uuti = pt_uuti / pt_atb,
        p.sev.uti = p_sev_uti,
        p.esbl = p_esbl
      )
      
      # Call tree_adult() function and store the resulting named vector
      vectors[[i]] <- tree_adult(input_data)
    }
    
    # Combine the results from all drugs using vector_to_table_multi
    combined_result <- vector_to_table_multi(vectors)
    
    # Render the combined results as a table
    output$model_output <- renderTable({
      combined_result
    })
  })
}
