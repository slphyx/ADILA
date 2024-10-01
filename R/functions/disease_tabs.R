# disease input tabs

# Generate dynamic UI for disease tabs
generate_disease_tabs <- function() {
  tab_list <- lapply(1:10, function(i) {
    tabPanel(
      paste("Disease", i),
      
      # Organizing the input parameters into 2 rows and 3 columns
      
      # First row: p.first, p.esbl, pt.admt
      fluidRow(
        column(4,  # 1st column for p.first
               h4("Probability of First Choice Antibiotic (p.first)"),
               radioButtons(paste0("p_first_dist_", i), "Distribution:",
                            choices = c("Uniform", "Normal", "Gamma"), inline = TRUE),
               conditionalPanel(
                 condition = sprintf("input.p_first_dist_%d == 'Uniform'", i),
                 numericInput(paste0("p_first_min_", i), "Min:", value = 0),
                 numericInput(paste0("p_first_max_", i), "Max:", value = 1)
               ),
               conditionalPanel(
                 condition = sprintf("input.p_first_dist_%d == 'Normal'", i),
                 numericInput(paste0("p_first_mean_", i), "Mean:", value = 0.90),
                 numericInput(paste0("p_first_sd_", i), "SD:", value = 0.05)
               ),
               conditionalPanel(
                 condition = sprintf("input.p_first_dist_%d == 'Gamma'", i),
                 numericInput(paste0("p_first_shape_", i), "Shape:", value = 2),
                 numericInput(paste0("p_first_rate_", i), "Rate:", value = 2)
               )
        ),
        
        column(4,  # 2nd column for p.esbl
               h4("Prevalence of ESBL Infection (p.esbl)"),
               radioButtons(paste0("p_esbl_dist_", i), "Distribution:",
                            choices = c("Uniform", "Normal", "Gamma"), inline = TRUE),
               conditionalPanel(
                 condition = sprintf("input.p_esbl_dist_%d == 'Uniform'", i),
                 numericInput(paste0("p_esbl_min_", i), "Min:", value = 0),
                 numericInput(paste0("p_esbl_max_", i), "Max:", value = 1)
               ),
               conditionalPanel(
                 condition = sprintf("input.p_esbl_dist_%d == 'Normal'", i),
                 numericInput(paste0("p_esbl_mean_", i), "Mean:", value = 0.30),
                 numericInput(paste0("p_esbl_sd_", i), "SD:", value = 0.05)
               ),
               conditionalPanel(
                 condition = sprintf("input.p_esbl_dist_%d == 'Gamma'", i),
                 numericInput(paste0("p_esbl_shape_", i), "Shape:", value = 2),
                 numericInput(paste0("p_esbl_rate_", i), "Rate:", value = 2)
               )
        ),
        
        column(4,  # 3rd column for pt.admt
               h4("Total Admitted Patients (pt.admt)"),
               radioButtons(paste0("pt_admt_dist_", i), "Distribution:",
                            choices = c("Uniform", "Normal", "Gamma"), inline = TRUE),
               conditionalPanel(
                 condition = sprintf("input.pt_admt_dist_%d == 'Uniform'", i),
                 numericInput(paste0("pt_admt_min_", i), "Min:", value = 50),
                 numericInput(paste0("pt_admt_max_", i), "Max:", value = 1000)
               ),
               conditionalPanel(
                 condition = sprintf("input.pt_admt_dist_%d == 'Normal'", i),
                 numericInput(paste0("pt_admt_mean_", i), "Mean:", value = 150),
                 numericInput(paste0("pt_admt_sd_", i), "SD:", value = 50)
               ),
               conditionalPanel(
                 condition = sprintf("input.pt_admt_dist_%d == 'Gamma'", i),
                 numericInput(paste0("pt_admt_shape_", i), "Shape:", value = 2),
                 numericInput(paste0("pt_admt_rate_", i), "Rate:", value = 1)
               )
        )
      ),
      
      # Second row: pt.bmen, pt.uuti, p.sev.uti
      fluidRow(
        column(4,  # 1st column for pt.bmen
               h4("Patients with Bacterial Meningitis (pt.bmen)"),
               radioButtons(paste0("pt_bmen_dist_", i), "Distribution:",
                            choices = c("Uniform", "Normal", "Gamma"), inline = TRUE),
               conditionalPanel(
                 condition = sprintf("input.pt_bmen_dist_%d == 'Uniform'", i),
                 numericInput(paste0("pt_bmen_min_", i), "Min:", value = 1),
                 numericInput(paste0("pt_bmen_max_", i), "Max:", value = 1000)
               ),
               conditionalPanel(
                 condition = sprintf("input.pt_bmen_dist_%d == 'Normal'", i),
                 numericInput(paste0("pt_bmen_mean_", i), "Mean:", value = 50),
                 numericInput(paste0("pt_bmen_sd_", i), "SD:", value = 10)
               ),
               conditionalPanel(
                 condition = sprintf("input.pt_bmen_dist_%d == 'Gamma'", i),
                 numericInput(paste0("pt_bmen_shape_", i), "Shape:", value = 2),
                 numericInput(paste0("pt_bmen_rate_", i), "Rate:", value = 1)
               )
        ),
        
        column(4,  # 2nd column for pt.uuti
               h4("Patients with Upper UTI (pt.uuti)"),
               radioButtons(paste0("pt_uuti_dist_", i), "Distribution:",
                            choices = c("Uniform", "Normal", "Gamma"), inline = TRUE),
               conditionalPanel(
                 condition = sprintf("input.pt_uuti_dist_%d == 'Uniform'", i),
                 numericInput(paste0("pt_uuti_min_", i), "Min:", value = 1),
                 numericInput(paste0("pt_uuti_max_", i), "Max:", value = 1000)
               ),
               conditionalPanel(
                 condition = sprintf("input.pt_uuti_dist_%d == 'Normal'", i),
                 numericInput(paste0("pt_uuti_mean_", i), "Mean:", value = 50),
                 numericInput(paste0("pt_uuti_sd_", i), "SD:", value = 10)
               ),
               conditionalPanel(
                 condition = sprintf("input.pt_uuti_dist_%d == 'Gamma'", i),
                 numericInput(paste0("pt_uuti_shape_", i), "Shape:", value = 2),
                 numericInput(paste0("pt_uuti_rate_", i), "Rate:", value = 1)
               )
        ),
        
        column(4,  # 3rd column for p.sev.uti
               h4("Proportion of Severe Upper UTI (p.sev.uti)"),
               radioButtons(paste0("p_sev_uti_dist_", i), "Distribution:",
                            choices = c("Uniform", "Normal", "Gamma"), inline = TRUE),
               conditionalPanel(
                 condition = sprintf("input.p_sev_uti_dist_%d == 'Uniform'", i),
                 numericInput(paste0("p_sev_uti_min_", i), "Min:", value = 0),
                 numericInput(paste0("p_sev_uti_max_", i), "Max:", value = 1)
               ),
               conditionalPanel(
                 condition = sprintf("input.p_sev_uti_dist_%d == 'Normal'", i),
                 numericInput(paste0("p_sev_uti_mean_", i), "Mean:", value = 0.2),
                 numericInput(paste0("p_sev_uti_sd_", i), "SD:", value = 0.05)
               ),
               conditionalPanel(
                 condition = sprintf("input.p_sev_uti_dist_%d == 'Gamma'", i),
                 numericInput(paste0("p_sev_uti_shape_", i), "Shape:", value = 2),
                 numericInput(paste0("p_sev_uti_rate_", i), "Rate:", value = 2)
               )
        )
      )
    )
  })
  
  do.call(tabsetPanel, tab_list)
}

# Handle input processing for each disease tab
handle_disease_inputs <- function(input, i) {
  # Extract and process inputs based on selected distribution for each parameter
  p_first_dist <- input[[paste0("p_first_dist_", i)]]
  p_first_value <- if (p_first_dist == "Uniform") {
    runif(1, input[[paste0("p_first_min_", i)]], input[[paste0("p_first_max_", i)]])
  } else if (p_first_dist == "Normal") {
    rnorm(1, input[[paste0("p_first_mean_", i)]], input[[paste0("p_first_sd_", i)]])
  } else {
    rgamma(1, shape = input[[paste0("p_first_shape_", i)]], rate = input[[paste0("p_first_rate_", i)]])
  }
  
  # Similarly, process p.esbl, pt.admt, pt.bmen, pt.uuti, p.sev.uti
  # Return the vector result
}
