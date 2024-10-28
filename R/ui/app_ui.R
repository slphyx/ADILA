#SLPHYX@SHIFT-ENTER

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
    titleWidth = 400,
    # Header with actionLink buttons
    tags$li(actionLink("goto_intro", "Introduction", class = "btn btn-default"), class = "dropdown"),
    tags$li(actionLink("goto_howto","How to use the dashboard", class = "btn btn-default"), class = "dropdown"),
    tags$li(actionLink("goto_simulation", "Simulation", class = "btn btn-default"), class = "dropdown"),
    tags$li(actionLink("goto_about", "About us", class = "btn btn-default"), class = "dropdown")
    
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  dashboardSidebar(
    width = 400,
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
                   numericInput("cap_cases", tags$h5("Patients with community acquired pneumonia (CAP)"), min = 0, max = 10000, value = 50,width ="100%"),
                   numericInput("bm_cases", tags$h5("Patients with bacterial meningitis"), min = 0, max = 10000, value = 15,width ="100%"),
                   numericInput("bji_cases", tags$h5("Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)"), min = 0, max = 10000, value = 30,width ="100%"),
                   numericInput("uut_cases",tags$h5("Patients with upper urinary tract infection"), min = 0, max = 10000, value = 35,width ="100%"),
                   numericInput("sepsis_cases", tags$h5("Patients with sepsis & septic shock"), min = 0, max = 10000, value = 10,width ="100%"),
                   numericInput("cdif_cases", tags$h5("Patients with Clostridioides difficile infection"), min = 0, max = 10000, value = 30,width ="100%"),
                   ),
                   column(6,
                   numericInput("hap_cases", tags$h5("Patients with hospital acquired pneumonia (HAP) non-VAP"), min = 0, max = 10000, value = 10,width ="100%"),
                   numericInput("ia_cases", tags$h5("Patients with intra-abdominal infections"), min = 0, max = 10000, value = 35,width ="100%"),
                   numericInput("sst_cases", tags$h5("Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)"), min = 0, max = 10000, value = 30,width ="100%"),
                   numericInput("fn_cases", tags$h5("Patients with febrile neutropenic"), min = 0, max = 10000, value = 30,width ="100%"),
                   numericInput("sp_cases", tags$h5("Patients on surgical prophylaxis"), min = 0, max = 10000, value = 60,width ="100%")
                   ),
               )
               )
      ),
      
      # Sidebar item for "Severity of Cases"
      menuItem("Severity of cases", tabName = "severity_cases", icon = icon("exclamation-triangle"),
               div(id = "severity_cases_inputs",
                   sliderInput("cap_severe", tags$h5("Proportion of severe CAP cases"), min = 0, max = 1, value = 0.4, step = 0.01),
                   sliderInput("abd_severe", tags$h5("Proportion of severe intra-abdominal infection cases"), min = 0, max = 1, value = 0.45, step = 0.01),
                   sliderInput("uti_severe", tags$h5("Proportion of severe upper UTI cases"), min = 0, max = 1, value = 0.35, step = 0.01),
                   sliderInput("cdf_severe", tags$h5("Proportion of severe C. difficile infection cases"), min = 0, max = 1, value = 0.25, step = 0.01),
                   sliderInput("sst_nf", tags$h5("Proportion of necrotizing fasciitis cases in SST patients"), min = 0, max = 1, value = 0.04, step = 0.01)
               )
               
      ),
      
      # Sidebar item for "Availability of First Choice Antibiotics"
      menuItem("Availability of first choice antibiotics", tabName = "availability_antibiotics", icon = icon("capsules"),
               div(id = "availability_antibiotics_inputs",
                   tableOutput("first_choice_table"),
                   sliderInput("p_first", tags$h5("Probability of first-choice antibiotics"), min = 0, max = 1, value = 0.8, step = 0.01)
               )           
      ),
      
      # Sidebar item for "Prevalence of AMR"
      menuItem("Prevalence of AMR", tabName = "prevalence_amr", icon = icon("chart-line"),
               div(id = "prevalence_amr_inputs",
                   sliderInput("esbl_prevalence", tags$h5("Prevalence of ESBL"), min = 0, max = 1, value = 0.4, step = 0.01),
                   sliderInput("mrsa_prevalence", tags$h5("Prevalence of MRSA"), min = 0, max = 1, value = 0.3, step = 0.01),
                   sliderInput("hap_mdr", tags$h5("Prevalence of multi-drug resistance gram-negative infection in patients with hospital acquired pneumonia"), min = 0, max = 1, value = 0.1, step = 0.01),
                   sliderInput("strep_pyogenes", tags$h5("Prevalence of Strep pyogenes infection in necrotizing fasciitis"), min = 0, max = 1, value = 0.1, step = 0.01)
               )         
      ),
      
      # Sidebar item for "Total Admitted Patients"
      menuItem("Total admitted patients", tabName = tags$h5("total_patients"), icon = icon("hospital"),
      div(id = "total_patients_inputs",
          column(width=6,
          numericInput("admitted_patients", tags$h5("Total admitted patients"), min = 0, max = 10000, value = 600,width ="100%"),
          )
          # ,column(width=6,
          # numericInput("std_err", tags$h5("Standard error of the parameters"), min = 0, max = 100, value = 20,width ="100%")
          # ),
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
        href = "css/style.css"),
      tags$script(src = "js/toTheTop.js")
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
    
    fluidRow(id = "intro_text",
      column(8,
             # Introduction Section
             h1("Introduction"),
             h4(
               tags$ul(
                 tags$li("This is an interactive open software dashboard to estimate expected usage of empirical antibiotic prescription in hospital settings"),
                 tags$li("It is a rapid, reproducible and easy-to-use platform that enables users without prior software experience to get expected pattern of empirical antibiotic usage in hospitals"),
                 tags$li(HTML("The dashboard is part of the Antibiotic Data to Inform Local Action (ADILA) project")),
                 tags$li("The estimation of expected empirical antibiotic use is based on treatment guidelines for common infections, as specified in the WHO AWaRe antibiotic book"),
                 tags$li("Expected use refer to the anticipated empirical prescription per day, considering patients with varying infections, case severity, access to recommended first-choice antibiotics, and the local prevalence of antimicrobial resistance (AMR)")
               )
             ),
             
             # What Users Can Do Section
             h1("What you (users) can do:"),
             h4(
               tags$ul(
                 tags$li("You can get generated tables on expected levels and patterns of empirical antibiotic usage upon entering information on numbers of patients with different infection syndromes, severity of cases, local prevalence of AMR (ESBL and MRSA etc.) and numbers of admitted patients in the hospital"),
                 tags$li(HTML("The outputs are available for overall usage as well as usage disaggregated by antibiotic classes and 
                              <span style='color:#4DAE42;'>A</span><span style='color:#dfb31d;'>Wa</span><span style='color:#DC202A;'>Re</span>
                              (<span style='color:#4DAE42;'>Access</span>, 
                              <span style='color:#dfb31d;'>Watch</span>, 
                              <span style='color:#DC202A;'>Reserve</span>) 
                              antibiotic classification.")),
                 tags$li("You can visualize the expected estimates"),
                 tags$li("You can download output tables and figures"),
               )
             )
      ),
      
      # Image on the right
      column(4,
             tags$img(src = "img/antibiotic_book.png", width = "70%")
      )
    ),
    
    fluidRow(id="howto_text",
      column(12,
             # Introduction Section
             h1("How to use the dashboard"),
             h4(
               tags$ul(
                 tags$li("Step 1: Users should put the required data on left side of the panel."),
                 tags$li("Step 2: Once the data is entered, review the “SUMMARY OF INPUT DATA”. If any changes are needed, return to Step 1 and update the data."),
                 tags$li("Step 3: Click “Run Model” and wait a few seconds for the model to process the data to generate the output."),
                 tags$li("Step 4: Review the expected empirical antibiotic use across four different tables in “SUMMARY OF EXPECTED ANTIBIOTIC USAGE (MODEL’S OUTPUT)”"),
                 tags$li("Step 5: You can also visualise the expected usage by AWaRe category and antibiotic class under “VISUALIZATION”.")
               )
             ))
    ),
    
    
    fluidRow(id = "about_text",
      column(12,
             # Title
             h1("Contact us"),
             h4(
             # Contact Information
               tags$p(
                 strong("Drug-Resistant Infections and Disease Dynamics group"),
                 br(),
                 "Centre for Tropical Medicine and Global Health",
                 br(),
                 "Bid Data Institute, Old Road Campus, OX3 7LF",
                 br(),
                 "University of Oxford",
                 br(),
                 br(),
                 "For any inquiry about the dashboard, please contact",
                 br(),
                 "Email: ", tags$a(href="mailto:myo.swe@ndm.ox.ac.uk", "myo.swe@ndm.ox.ac.uk")
               )
             )
      )
    ),
    
    fluid_design("Summary_model_table", "summary_output", NULL, NULL, NULL),
    fluid_design("Visualization_plot", "Visualization_output1", "Visualization_output2", "Visualization_output3",NULL),
    
    fluidRow(id = "summary_inputs_ui",
      div(
        id = "Summary_input_table",
        column(width = 12,
               column(width = 6,
                   downloadButton(
                     "downloadInput",
                     label = "Download Inputs",
                     icon = shiny::icon("download")
                   ),
               ),
               column(width = 6,
                  fileInput("load_params", "",
                            buttonLabel="Upload Input File" ,
                            accept = c(".csv"))
               ),
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
    ),
    tags$div(id="partnersImage",
             style = "text-align: center;", 
      tags$img(
        src = "img/partners_image.png", width="70%" ,height="70%"
      )
    ),
    tags$div(id = "goTopButton", "Go to Top"),
  ),
  skin = "black",
  title = "Antibiotic Usage Model"
)

