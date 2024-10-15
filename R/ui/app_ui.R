library(gtsummary)
library(shiny)
library(gt)
library(gtools) # For the rdirichlet function
library(shinyjs)
library(shinydashboard)

app_ui <- dashboardPage(
  skin = "black",
  title = "Antibiotic Usage Model",
  
  # HEADER ------------------------------------------------------------------
  dashboardHeader(
    title = span(img(src = "logo.svg", height = 35), "Antibiotic Usage Model"),
    titleWidth = 300
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  dashboardSidebar(
    width = 350,
    useShinyjs(),
    sidebarMenu(
      
      # Sidebar item for "Type of Patients"
      menuItem("Type of patients", tabName = "type_patients", icon = icon("user"),
               div(id = "type_patients_inputs",
                   numericInput("cap_cases", "CAP Cases", min = 0, max = NA, value = 50),
                   numericInput("hap_cases", "HAP Cases", min = 0, max = NA, value = 10),
                   numericInput("cns_cases", "CNS Cases", min = 0, max = NA, value = 15),
                   numericInput("ia_cases", "Intra-abdominal Infection Cases", min = 0, max = NA, value = 35),
                   numericInput("pye_cases", "Pyelonephritis Cases", min = 0, max = NA, value = 35),
                   numericInput("sst_cases", "Skin and Soft Tissue Infection Cases", min = 0, max = NA, value = 30),
                   numericInput("bj_cases", "Bone and Joint Infection Cases", min = 0, max = NA, value = 30),
                   numericInput("cdif_cases", "C. difficile Infection Cases", min = 0, max = NA, value = 30),
                   numericInput("fn_cases", "Febrile Neutropenia Cases", min = 0, max = NA, value = 10),
                   numericInput("sepsis_cases", "Sepsis Cases", min = 0, max = NA, value = 30),
                   numericInput("sp_cases", "Surgical Prophylaxis Cases", min = 0, max = NA, value = 60)
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
                   sliderInput("multidrug_resistance", "Prevalence of multi-drug resistance gram-negative infection in patients with hospital acquired pneumonia", min = 0, max = 1, value = 0.1, step = 0.01),
                   sliderInput("strep_pyogenes", "Prevalence of Strep pyogenes infection in necrotizing fasciitis", min = 0, max = 1, value = 0.1, step = 0.01)
               )         
      ),
      
      # Sidebar item for "Total Admitted Patients"
      menuItem("Total admitted patients", tabName = "total_patients", icon = icon("hospital"),
      div(id = "total_patients_inputs",
          numericInput("admitted_patients", "Total admitted patients", min = 0, max = NA, value = 600)
      )
    )
    )
  ),
  
  # BODY --------------------------------------------------------------------
  dashboardBody(
    fluidRow(
      column(12, h3("Select inputs from the sidebar to configure the model"))
    )
  )
)

