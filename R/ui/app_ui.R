library(gtsummary)
library(shiny)
library(gt)
library(gtools) # For the rdirichlet function
library(shinyjs)
library(shinydashboard)
library(rintrojs)

app_ui <- dashboardPage(

  
  # HEADER ------------------------------------------------------------------
  dashboardHeader(
    title = span(img(src = "img/ADILA_logo-removebg-preview.svg", height = 35), "Antibiotic Data to Inform Local Action (ADILA)"),
    titleWidth = 300
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  dashboardSidebar(
    width = 350,
    useShinyjs(),
    sidebarMenu(
      introBox(data.step = 1, data.intro = "test", # intro tour
               div(id = "sidebar_button",
                   bsButton(inputId = "run_model", 
                            label = "Run Model", 
                            icon = icon("play-circle"), 
                            style = "danger")
               )
      ),
      # Sidebar item for "Type of Patients"
      menuItem("Type of patients", tabName = "type_patients", icon = icon("user"),
               div(id = "type_patients_inputs",
                   fluidRow(
                  column(6,
                   numericInput("cap_cases", tags$h6("Patients with community acquired pneumonia (CAP)"), min = 0, max = 10000, value = 50,width ="100%"),
                   numericInput("cns_cases", tags$h6("Patients with bacterial meningitis"), min = 0, max = 10000, value = 15,width ="100%"),
                   numericInput("sst_cases", tags$h6("Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)"), min = 0, max = 10000, value = 30,width ="100%"),
                   numericInput("pye_cases",tags$h6("Patients with upper urinary tract infection"), min = 0, max = 10000, value = 35,width ="100%"),
                   numericInput("fn_cases", tags$h6("Patients with sepsis & septic shock"), min = 0, max = 10000, value = 10,width ="100%"),
                   numericInput("sepsis_cases", tags$h6("Patients with Clostridioides difficile infection"), min = 0, max = 10000, value = 30,width ="100%"),
                   ),
                   column(6,
                   numericInput("hap_cases", tags$h6("Patients with hospital acquired pneumonia (HAP) non-VAP"), min = 0, max = 10000, value = 10,width ="100%"),
                   numericInput("ia_cases", tags$h6("Patients with intra-abdominal infections"), min = 0, max = 10000, value = 35,width ="100%"),
                   numericInput("bj_cases", tags$h6("Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)"), min = 0, max = 10000, value = 30,width ="100%"),
                   numericInput("cdif_cases", tags$h6("Patients with febrile neutropenic"), min = 0, max = 10000, value = 30,width ="100%"),
                   numericInput("sp_cases", tags$h6("Patients on surgical prophylaxis"), min = 0, max = 10000, value = 60,width ="100%")
                   ),
               )
               )
      ),
      
      # Sidebar item for "Severity of Cases"
      menuItem("Severity of cases", tabName = "severity_cases", icon = icon("exclamation-triangle"),
               div(id = "severity_cases_inputs",
                   sliderInput("cap_severe", "Proportion of severe CAP cases", min = 0, max = 1, value = 0.4, step = 0.01),
                   sliderInput("abd_severe", "Proportion of severe intra-abdominal infection cases", min = 0, max = 1, value = 0.45, step = 0.01),
                   sliderInput("uti_severe", "Proportion of severe upper UTI cases", min = 0, max = 1, value = 0.35, step = 0.01),
                   sliderInput("cdf_severe", "Proportion of severe C. difficile infection cases", min = 0, max = 1, value = 0.25, step = 0.01),
                   sliderInput("sst_nf", "Proportion of necrotizing fasciitis cases in SST patients", min = 0, max = 1, value = 0.04, step = 0.01)
               )
               
      ),
      
      # Sidebar item for "Availability of First Choice Antibiotics"
      menuItem("Availability of first choice antibiotics", tabName = "availability_antibiotics", icon = icon("capsules"),
               div(id = "availability_antibiotics_inputs",
                   sliderInput("p_first", "Probability of first-choice antibiotics", min = 0, max = 1, value = 0.8, step = 0.01)
               )           
      ),
      
      # Sidebar item for "Prevalence of AMR"
      menuItem("Prevalence of AMR", tabName = "prevalence_amr", icon = icon("chart-line"),
               div(id = "prevalence_amr_inputs",
                   sliderInput("esbl_prevalence", "Prevalence of ESBL", min = 0, max = 1, value = 0.4, step = 0.01),
                   sliderInput("mrsa_prevalence", "Prevalence of MRSA", min = 0, max = 1, value = 0.3, step = 0.01),
                   sliderInput("hap_mdr", "Prevalence of multi-drug resistance gram-negative infection in patients with hospital acquired pneumonia", min = 0, max = 1, value = 0.1, step = 0.01),
                   sliderInput("strep_pyogenes", "Prevalence of Strep pyogenes infection in necrotizing fasciitis", min = 0, max = 1, value = 0.1, step = 0.01)
               )         
      ),
      
      # Sidebar item for "Total Admitted Patients"
      menuItem("Total admitted patients", tabName = "total_patients", icon = icon("hospital"),
      div(id = "total_patients_inputs",
          numericInput("admitted_patients", "Total admitted patients", min = 0, max = 10000, value = 600,width ="30%"),
          numericInput("std_err", "std_err", min = 0, max = 1, value = 0.2,width ="30%")
      )
    )
    )
  ),
  
  # BODY --------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "style.css"),
      tags$script(src = "scripts.js")
    ),
    # Preconnect to Google Fonts and font assets
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    
    # Link to the Google Font Montserrat with various weights and styles
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Rubik:ital,wght@0,300..900;1,300..900&display=swap"),
    
    bsButton("Summary_input", 
             label = "Summary of input data", 
             icon = icon("table"), 
             style = "success"),
    bsButton("Summary_model", 
             label = "Summary of expected antibiotic usage (model’s output)", 
             icon = icon("spinner", class = "spinner-box"), 
             style = "success"),
    bsButton("Visualization", 
             label = "Visualization", 
             icon = icon("flask", class = "flask-box"), 
             style = "success"),
    
    fluid_design("Summary_model_table", "summary_output", NULL, NULL, NULL),
    fluid_design("Visualization_plot", "Visualization_output1", "Visualization_output2", "Visualization_output3",NULL),
    
    fluidRow(
      div(
        id = "Summary_input_table",
        column(width = 12,
               downloadButton(
                 "downloadInput",
                 label = "Download",
                 icon = shiny::icon("download")
               )
        ),
          tabBox(
            title = "",
            tabPanel(title = "Patients with different infection syndromes",
              tableOutput("summary_inputs")
            )
          ),
          tabBox(
            title = "",
            tabPanel(title = "Proportion/ prevalence parameters",
              tableOutput("summary_inputs2")
            )
          )
      )
    )
    
  ),
  skin = "black",
  title = "Antibiotic Usage Model"
)

