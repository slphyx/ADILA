library(gtsummary)
library(shiny)
library(gt)
library(gtools) # For the rdirichlet function
library(shinyjs)


app_ui <- fluidPage(
  
  # Use shinyjs to control visibility of elements
  useShinyjs(),
  
  # Navbar with three menus
  navbarPage(
    title = "Antibiotic Usage Model",
    
    # Menu 1: About App (Empty for now)
    tabPanel("About App", 
             h3("About this App"),
             p("This section will describe the app in detail.")),
    
    # Menu 2: Run Simulation
    tabPanel("Run Simulation",
             
             # Layout with two sections (above: inputs, below: output)
             fluidRow(
               
               # Top section: input parameters with two tabs
               column(12,
                      actionButton("toggle_input", "Show/Hide Input Parameters"),
                      hidden(
                        div(id = "input_parameters",
                            
                            # Tabset for Input Parameters and Summary
                            tabsetPanel(
                              
                              # Tab 1: Inputs for std_err and adult_cases$cases
                              tabPanel("Inputs for std_err and adult_cases$cases",
                                       
                                       fluidRow(
                                         column(4,
                                                sliderInput("std_err", 
                                                            "Standard Error (as a percentage)", 
                                                            min = 0, max = 100, value = 20, step = 0.1)
                                         )
                                       ),
                                       
                                       fluidRow(
                                         column(4, sliderInput("cap_cases", "CAP Cases", min = 0, max = 100, value = 50)),
                                         column(4, sliderInput("hap_cases", "HAP Cases", min = 0, max = 100, value = 10)),
                                         column(4, sliderInput("cns_cases", "CNS Cases", min = 0, max = 100, value = 15))
                                       ),
                                       
                                       fluidRow(
                                         column(4, sliderInput("ia_cases", "Intra-abdominal Infection Cases", min = 0, max = 100, value = 35)),
                                         column(4, sliderInput("pye_cases", "Pyelonephritis Cases", min = 0, max = 100, value = 35)),
                                         column(4, sliderInput("sst_cases", "Skin and Soft Tissue Infection Cases", min = 0, max = 100, value = 30))
                                       ),
                                       
                                       fluidRow(
                                         column(4, sliderInput("bj_cases", "Bone and Joint Infection Cases", min = 0, max = 100, value = 30)),
                                         column(4, sliderInput("cdif_cases", "C. difficile Infection Cases", min = 0, max = 100, value = 30)),
                                         column(4, sliderInput("fn_cases", "Febrile Neutropenia Cases", min = 0, max = 100, value = 10))
                                       ),
                                       
                                       fluidRow(
                                         column(4, sliderInput("sepsis_cases", "Sepsis Cases", min = 0, max = 100, value = 30)),
                                         column(4, sliderInput("sp_cases", "Surgical Prophylaxis Cases", min = 0, max = 100, value = 60))
                                       )
                              ),
                              
                              # Tab 2: Inputs for para.data$value
                              tabPanel("Inputs for para.data$value",
                                       
                                       fluidRow(
                                         column(4, sliderInput("p_first", "Probability of first-choice antibiotics", min = 0, max = 1, value = 0.8, step = 0.01)),
                                         column(4, sliderInput("cap_severe", "Proportion of severe CAP cases", min = 0, max = 1, value = 0.4, step = 0.01)),
                                         column(4, sliderInput("hap_mdr", "Risk of multi-drug resistant HAP infection", min = 0, max = 1, value = 0.5, step = 0.01))
                                       ),
                                       
                                       fluidRow(
                                         column(4, sliderInput("abd_severe", "Proportion of severe intra-abdominal infection cases", min = 0, max = 1, value = 0.45, step = 0.01)),
                                         column(4, sliderInput("uti_severe", "Proportion of severe upper UTI cases", min = 0, max = 1, value = 0.35, step = 0.01)),
                                         column(4, sliderInput("cdf_severe", "Proportion of severe C. difficile infection cases", min = 0, max = 1, value = 0.25, step = 0.01))
                                       ),
                                       
                                       fluidRow(
                                         column(4, sliderInput("sst_nf", "Proportion of necrotizing fasciitis cases in SST patients", min = 0, max = 1, value = 0.04, step = 0.01)),
                                         column(4, sliderInput("esbl_prevalence", "Prevalence of ESBL", min = 0, max = 1, value = 0.4, step = 0.01)),
                                         column(4, sliderInput("mrsa_prevalence", "Prevalence of MRSA", min = 0, max = 1, value = 0.3, step = 0.01))
                                       ),
                                       
                                       fluidRow(
                                         column(4, sliderInput("strep_pyogenes", "Prevalence of Strep pyogenes infection in necrotizing fasciitis", min = 0, max = 1, value = 0.1, step = 0.01)),
                                         column(4, sliderInput("admitted_patients", "Total admitted patients", min = 0, max = 1000, value = 600))
                                       )
                              ),
                              
                              # Tab 3: Summary of inputs
                              tabPanel("Summary Inputs",
                                       h4("Summary of Inputs"),
                                       tableOutput("summary_inputs")
                              )
                            )
                        )
                      ),
                      actionButton("run_model", "Run Model")
               ),
               
               # Bottom section: Simulation output
               column(12,
                      h3("Summary Table of Expected Antibiotic Usage"),
                      gt_output(outputId = "summary_table")%>%withSpinner()
               )
             )
    ),
    
    # Menu 3: Model Details (Empty for now)
    tabPanel("Model Details", 
             h3("Model Details"),
             p("This section will explain the details of the model."))
  )
)
