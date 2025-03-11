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
    title = span(img(src = "img/ADILA_logo-removebg-preview.svg", height = 24), "Antibiotic Data to Inform Local Action (ADILA)"),
    titleWidth = 600,
    # Header with actionLink buttons
    tags$li(actionLink("goto_intro", HTML("<b>Introduction</b>"), class = "btn btn-default"), class = "dropdown"),
    tags$li(actionLink("goto_howto",HTML("<b>How to use the dashboard</b>"), class = "btn btn-default"), class = "dropdown"),
    tags$li(actionLink("goto_simulation", HTML("<b>Simulation</b>"), class = "btn btn-default"), class = "dropdown"),
    tags$li(actionLink("goto_about", HTML("<b>About us</b>"), class = "btn btn-default"), class = "dropdown")
    
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  dashboardSidebar(
    width = 600,
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
      br(),
      radioButtons("choices_ac", "Type of patients :",
                                  c("Adolescents and adults (> 12 years)" = "adult",
                                    "Child (1 month – 12 years)" = "child",
                                    "Both" = "both"),
                   inline = T
                     ),

      menuItem("Type of clinical infections", tabName = "type_patients", icon = icon("user"),

               div(id = "type_patients_inputs_single",
                   #1
                   fluidRow(
                     column(6,
                            numericInput("cap_cases", tags$h5("Patients with community acquired pneumonia (CAP)"), min = 0, max = 10000, value = 50,width ="100%"),
                     ),
                     column(6,
                            numericInput("hap_cases", tags$h5("Patients with hospital acquired pneumonia (HAP) non-VAP"), min = 0, max = 10000, value = 10,width ="100%"),
                     ),
                   ),
                   #2
                   fluidRow(
                     column(6,
                            numericInput("bm_cases", tags$h5("Patients with bacterial meningitis"), min = 0, max = 10000, value = 15,width ="100%"),
                     ),
                     column(6,
                            numericInput("ia_cases", tags$h5("Patients with intra-abdominal infections"), min = 0, max = 10000, value = 35,width ="100%"),
                     ),
                   ),
                   #3
                   fluidRow(
                     column(6,
                            numericInput("uut_cases",tags$h5("Patients with Community acquired pyelonephritis "), min = 0, max = 10000, value = 35,width ="100%"),
                     ),
                     column(6,
                            numericInput("cdif_cases", tags$h5("Patients with", tags$em("Clostridioides difficile"), "infection"), min = 0, max = 10000, value = 30,width ="100%"),
                     ),
                   ),
                   #4
                   fluidRow(
                     column(6,
                            
                            numericInput("bji_cases", tags$h5("Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)"), min = 0, max = 10000, value = 30,width ="100%"),
                     ),
                     column(6,
                            numericInput("sst_cases", tags$h5("Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)"), min = 0, max = 10000, value = 10,width ="100%"),
                     ),
                   ),
                   #5
                   fluidRow(
                     column(6,
                            numericInput("fn_cases", tags$h5("Patients with febrile neutropenic"), min = 0, max = 10000, value = 30,width ="100%"),
                     ),
                     column(6,
                            
                            numericInput("sepsis_cases", tags$h5("Patients with sepsis & septic shock"), min = 0, max = 10000, value = 30,width ="100%"),
                     ),
                   ),
                   #6
                   fluidRow(
                     column(6,
                            numericInput("sp_cases", tags$h5("Patients on surgical prophylaxis"), min = 0, max = 10000, value = 60,width ="100%")
                     ),
                     column(6,
                     ),
                   ),
               
               ),
               div(id = "type_patients_inputs_both",
                   fluidRow(
                       tabsetPanel(
                         tabPanel("Adult",
                                  #1
                                  fluidRow(
                                    column(6,
                                           numericInput("cap_cases_adult", tags$h5("Patients with community acquired pneumonia (CAP)"), min = 0, max = 10000, value = 50,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("hap_cases_adult", tags$h5("Patients with hospital acquired pneumonia (HAP) non-VAP"), min = 0, max = 10000, value = 10,width ="100%"),
                                    ),
                                  ),
                                  #2
                                  fluidRow(
                                    column(6,
                                           numericInput("bm_cases_adult", tags$h5("Patients with bacterial meningitis"), min = 0, max = 10000, value = 15,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("ia_cases_adult", tags$h5("Patients with intra-abdominal infections"), min = 0, max = 10000, value = 35,width ="100%"),
                                    ),
                                  ),
                                  #3
                                  fluidRow(
                                    column(6,
                                           numericInput("uut_cases_adult",tags$h5("Patients with Community acquired pyelonephritis "), min = 0, max = 10000, value = 35,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("cdif_cases_adult", tags$h5("Patients with", tags$em("Clostridioides difficile"), "infection"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                  ),
                                  #4
                                  fluidRow(
                                    column(6,
                                           
                                           numericInput("bji_cases_adult", tags$h5("Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("sst_cases_adult", tags$h5("Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)"), min = 0, max = 10000, value = 10,width ="100%"),
                                    ),
                                  ),
                                  #5
                                  fluidRow(
                                    column(6,
                                           numericInput("fn_cases_adult", tags$h5("Patients with febrile neutropenic"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                    column(6,
                                           
                                           numericInput("sepsis_cases_adult", tags$h5("Patients with sepsis & septic shock"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                  ),
                                  #6
                                  fluidRow(
                                    column(6,
                                           numericInput("sp_cases_adult", tags$h5("Patients on surgical prophylaxis"), min = 0, max = 10000, value = 60,width ="100%")
                                    ),
                                    column(6,
                                    ),
                                  ),
                         ),
                         tabPanel("Child",
                                  #1
                                  fluidRow(
                                    column(6,
                                           numericInput("cap_cases_child", tags$h5("Patients with community acquired pneumonia (CAP)"), min = 0, max = 10000, value = 50,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("hap_cases_child", tags$h5("Patients with hospital acquired pneumonia (HAP) non-VAP"), min = 0, max = 10000, value = 10,width ="100%"),
                                    ),
                                  ),
                                  #2
                                  fluidRow(
                                    column(6,
                                           numericInput("bm_cases_child", tags$h5("Patients with bacterial meningitis"), min = 0, max = 10000, value = 15,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("ia_cases_child", tags$h5("Patients with intra-abdominal infections"), min = 0, max = 10000, value = 35,width ="100%"),
                                    ),
                                  ),
                                  #3
                                  fluidRow(
                                    column(6,
                                           numericInput("uut_cases_child",tags$h5("Patients with Community acquired pyelonephritis "), min = 0, max = 10000, value = 35,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("cdif_cases_child", tags$h5("Patients with", tags$em("Clostridioides difficile"), "infection"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                  ),
                                  #4
                                  fluidRow(
                                    column(6,
                                           
                                           numericInput("bji_cases_child", tags$h5("Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                    column(6,
                                           numericInput("sst_cases_child", tags$h5("Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)"), min = 0, max = 10000, value = 10,width ="100%"),
                                    ),
                                  ),
                                  #5
                                  fluidRow(
                                    column(6,
                                           numericInput("fn_cases_child", tags$h5("Patients with febrile neutropenic"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                    column(6,
                                           
                                           numericInput("sepsis_cases_child", tags$h5("Patients with sepsis & septic shock"), min = 0, max = 10000, value = 30,width ="100%"),
                                    ),
                                  ),
                                  #6
                                  fluidRow(
                                    column(6,
                                           numericInput("sp_cases_child", tags$h5("Patients on surgical prophylaxis"), min = 0, max = 10000, value = 60,width ="100%")
                                    ),
                                    column(6,
                                    ),
                                  ),
                         ),
                       )
                   )
               )
      ),
      
      # Sidebar item for "Severity of Cases"
      menuItem("Severity of cases", tabName = "severity_cases", 
               icon = icon("exclamation-triangle"),
               div(id = "severity_cases_inputs_single",
                   tags$h6(class="poph5","*Click on the slider thumb to view the parameter information."),
               
                   sliderInput("cap_severe_single_adult", tags$h5("Proportion of CAP cases which are severe"), min = 0, max = 1, value = 0.26, step = 0.01),
                   sliderInput("cap_severe_single_child", tags$h5("Proportion of CAP cases which are severe"), min = 0, max = 1, value = 0.64, step = 0.01),
                   div(id="severity_cases_inputs_child",
                       sliderInput("cap_no_resp", tags$h5("proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr"), min = 0, max = 1, value = 0.5, step = 0.01),
                       sliderInput("hiv_severe", tags$h5("proportion of severe CAP patients with HIV infection"), min = 0, max = 1, value = 0.01, step = 0.01)
                   ),
                   sliderInput("abd_severe", tags$h5("Proportion of intra-abdominal infection cases which are severe"), min = 0, max = 1, value = 0.45, step = 0.01),
                   sliderInput("uti_severe", tags$h5("Proportion of upper UTI cases which are severe"), min = 0, max = 1, value = 0.16, step = 0.01),
                   sliderInput("cdf_severe", tags$h5("Proportion of", tags$em("Clostridioides difficile"), "infection (CDI) cases which are severe"), min = 0, max = 1, value = 0.36, step = 0.01),
                   sliderInput("sst_nf", tags$h5("Proportion of necrotizing fasciitis (NF) among patients with skin and soft-tissue (SST) infections"), min = 0, max = 0.2, value = 0.05, step = 0.01)
               ),
               div(id = "severity_cases_inputs_both",
                   fluidRow(
                     tabsetPanel(
                       tabPanel("Adult",
                                tags$h6(class="poph5","*Click on the slider thumb to view the parameter information."),
                                sliderInput("cap_severe_adult", tags$h5("Proportion of CAP cases which are severe"), min = 0, max = 1, value = 0.26, step = 0.01),
                                sliderInput("abd_severe_adult", tags$h5("Proportion of intra-abdominal infection cases which are severe"), min = 0, max = 1, value = 0.45, step = 0.01),
                                sliderInput("uti_severe_adult", tags$h5("Proportion of upper UTI cases which are severe"), min = 0, max = 1, value = 0.16, step = 0.01),
                                sliderInput("cdf_severe_adult", tags$h5("Proportion of", tags$em("Clostridioides difficile"), "infection (CDI) cases which are severe"), min = 0, max = 1, value = 0.36, step = 0.01),
                                sliderInput("sst_nf_adult", tags$h5("Proportion of necrotizing fasciitis (NF) among patients with skin and soft-tissue (SST) infections"), min = 0, max = 0.2, value = 0.05, step = 0.01)
                       ),
                       tabPanel("Child",
                                tags$h6(class="poph5","*Click on the slider thumb to view the parameter information."),
                                sliderInput("cap_severe_child", tags$h5("Proportion of CAP cases which are severe"), min = 0, max = 1, value = 0.64, step = 0.01),
                                sliderInput("cap_no_resp_child", tags$h5("proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr"), min = 0, max = 1, value = 0.5, step = 0.01),
                                sliderInput("hiv_severe_child", tags$h5("proportion of severe CAP patients with HIV infection"), min = 0, max = 1, value = 0.01, step = 0.01),
                                sliderInput("abd_severe_child", tags$h5("Proportion of intra-abdominal infection cases which are severe"), min = 0, max = 1, value = 0.45, step = 0.01),
                                sliderInput("uti_severe_child", tags$h5("Proportion of upper UTI cases which are severe"), min = 0, max = 1, value = 0.16, step = 0.01),
                                sliderInput("cdf_severe_child", tags$h5("Proportion of", tags$em("Clostridioides difficile"), "infection (CDI) cases which are severe"), min = 0, max = 1, value = 0.36, step = 0.01),
                                sliderInput("sst_nf_child", tags$h5("Proportion of necrotizing fasciitis (NF) among patients with skin and soft-tissue (SST) infections"), min = 0, max = 0.2, value = 0.05, step = 0.01)
                       ),
                     )
                   )
               )
               
      ),
      
      # Sidebar item for "Availability of First Choice Antibiotics"
      menuItem("Availability of first choice antibiotics", tabName = "availability_antibiotics", icon = icon("capsules"),
               div(id = "availability_antibiotics_inputs_single",
                   checkboxGroupInput(
                     inputId = "selected_antibiotics",
                     label = list(tags$h5("Proportion of recommended first-choice antibiotics which are available in the hospital :"),
                                  tags$h6("*Please de-select the box next to the antibiotic if it is not available in your hospital")
                                  ),
                     choices = NULL,  # Initially empty; populated dynamically
                     selected = NULL  # Initially empty; populated dynamically
                   ),
                   verbatimTextOutput("prob_1stchoice")
               ),
               div(id = "availability_antibiotics_inputs_both",
                     tabsetPanel(
                       tabPanel("Adult",
                                checkboxGroupInput(
                                  inputId = "selected_antibiotics_adult",
                                  label = list(tags$h5("Proportion of recommended first-choice antibiotics which are available in the hospital :"),
                                               tags$h6("*Please de-select the box next to the antibiotic if it is not available in your hospital")
                                  ),
                                  choices = NULL,  # Initially empty; populated dynamically
                                  selected = NULL  # Initially empty; populated dynamically
                                ),
                                verbatimTextOutput("prob_1stchoice_adult")
                       ),
                       tabPanel("Child",
                                checkboxGroupInput(
                                  inputId = "selected_antibiotics_child",
                                  label = list(tags$h5("Proportion of recommended first-choice antibiotics which are available in the hospital :"),
                                               tags$h6("*Please de-select the box next to the antibiotic if it is not available in your hospital")
                                  ),
                                  choices = NULL,  # Initially empty; populated dynamically
                                  selected = NULL  # Initially empty; populated dynamically
                                ),
                                verbatimTextOutput("prob_1stchoice_child")
                       ),
                     )
                   
               )   
      ),
      
      # Sidebar item for "Proportion of local AMR"
      menuItem("Proportion of local AMR", tabName = "prevalence_amr", icon = icon("chart-line"),
               tags$h6(class="poph5","*Click on the slider thumb to view the parameter information."),
               div(id = "prevalence_amr_inputs_single",
                   sliderInput("esbl_prevalence", tags$h5("Proportion of ESBL producing ", tags$em("Escherichia coli") ," among bloodstream infections with ",tags$em("Escherichia coli")), min = 0, max = 1, value = 0.4, step = 0.01),
                   sliderInput("mrsa_prevalence", tags$h5("Proportion of MRSA among bloodstream infections with", tags$em("Staphylococcus aureus")),  min = 0, max = 1, value = 0.3, step = 0.01),
                   sliderInput("hap_mdr", tags$h5("Proportion of multi-drug resistance (MDR) infection among patients with hospital acquired pneumonia (non-ventilator associated)"), min = 0, max = 1, value = 0.37, step = 0.01),
                   sliderInput("strep_pyogenes", tags$h5("Proportion of", tags$em("Streptococcus pyogenes"), "(Group A Streptococcus) infection among patients with necrotizing fasciitis"), min = 0, max = 1, value = 0.6, step = 0.01)
               ),
               div(id = "prevalence_amr_inputs_both",
                   tabsetPanel(
                     tabPanel("Adult",
                              sliderInput("esbl_prevalence_adult", tags$h5("Proportion of ESBL producing ", tags$em("Escherichia coli") ," among bloodstream infections with ",tags$em("Escherichia coli")), min = 0, max = 1, value = 0.4, step = 0.01),
                              sliderInput("mrsa_prevalence_adult", tags$h5("Proportion of MRSA among bloodstream infections with", tags$em("Staphylococcus aureus")), min = 0, max = 1, value = 0.3, step = 0.01),
                              sliderInput("hap_mdr_adult", tags$h5("Proportion of multi-drug resistance (MDR) infection among patients with hospital acquired pneumonia (non-ventilator associated)"), min = 0, max = 1, value = 0.37, step = 0.01),
                              sliderInput("strep_pyogenes_adult", tags$h5("Proportion of", tags$em("Streptococcus pyogenes"), "(Group A Streptococcus) infection among patients with necrotizing fasciitis"), min = 0, max = 1, value = 0.6, step = 0.01)
                     ),
                     tabPanel("Child",
                              sliderInput("esbl_prevalence_child", tags$h5("Proportion of ESBL producing ", tags$em("Escherichia coli") ," among bloodstream infections with ",tags$em("Escherichia coli")), min = 0, max = 1, value = 0.4, step = 0.01),
                              sliderInput("mrsa_prevalence_child", tags$h5("Proportion of MRSA among bloodstream infections with", tags$em("Staphylococcus aureus")),  min = 0, max = 1, value = 0.3, step = 0.01),
                              sliderInput("hap_mdr_child", tags$h5("Proportion of multi-drug resistance (MDR) infection among patients with hospital acquired pneumonia (non-ventilator associated)"), min = 0, max = 1, value = 0.37, step = 0.01),
                              sliderInput("strep_pyogenes_child", tags$h5("Proportion of", tags$em("Streptococcus pyogenes"), "(Group A Streptococcus) infection among patients with necrotizing fasciitis"), min = 0, max = 1, value = 0.6, step = 0.01)
                     ),
                   )
               )   
      ),
      
      # Sidebar item for "Total inpatients in a given day"
      menuItem("Total inpatients in a given day", tabName = tags$h5("total_patients"), icon = icon("hospital"),
               div(id = "total_patients_inputs_single",
                   column(width=6,
                          numericInput("admitted_patients", tags$h5("Total inpatients in a given day"), min = 0, max = 10000, value = 600,width ="100%"),
                   )
               ),
               div(id = "total_patients_inputs_both",
                   
                   tabsetPanel(
                     tabPanel("Adult",
                              column(width=12,
                                     numericInput("admitted_patients_adult", tags$h5("Total inpatients in a given day"), min = 0, max = 10000, value = 600,width ="100%"),
                              )
                     ),
                     tabPanel("Child",
                              column(width=12,
                                     numericInput("admitted_patients_child", tags$h5("Total inpatients in a given day"), min = 0, max = 10000, value = 600,width ="100%"),
                              )
                     ),
                   )
               )
      )
      
      # Sidebar item for "Type of Patients"
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
    tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css2?family=Noto+Sans:ital,wght@0,100..900;1,100..900&display=swap"),
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
               tags$ol(
                 tags$li("This interactive online tool estimates expected empirical antibiotic use in hospitals on a given day assuming prescribing follows the WHO AWaRe guidelines (",tags$a(href="https://www.who.int/publications/i/item/9789240062382",target='_blank', "https://www.who.int/publications/i/item/9789240062382"),  ")."),
                 tags$li("Expected use refers to the anticipated empirical antibiotic use based on local data or assumptions about numbers of patients with different types of infections, their severity, availability of recommended first-choice antibiotics, and local Proportion of antimicrobial resistance in different pathogens."),
                 tags$li("A 2-minute video explaining how the estimation is done is available here (video link)."),
                 br(),
                 # tags$img(src = "img/intro_model.png", width = "85%")
               )
             ),
             
             # # What Users Can Do Section
             # h1("What you (users) can do:"),
             # h4(
             #   tags$ul(
             #     tags$li("You can get generated tables on expected levels and patterns of empirical antibiotic usage upon entering information on numbers of patients with different infection syndromes, severity of cases, local Proportion of AMR (ESBL and MRSA etc.) and numbers of admitted patients in the hospital"),
             #     tags$li(HTML("The outputs are available for overall usage as well as usage disaggregated by antibiotic classes and 
             #                  <span style='color:#4DAE42;'>A</span><span style='color:#dfb31d;'>Wa</span><span style='color:#DC202A;'>Re</span>
             #                  (<span style='color:#4DAE42;'>Access</span>, 
             #                  <span style='color:#dfb31d;'>Watch</span>, 
             #                  <span style='color:#DC202A;'>Reserve</span>) 
             #                  antibiotic classification.")),
             #     tags$li("You can visualize the expected estimates"),
             #     tags$li("You can download output tables and figures"),
             #   )
             # )
      ),
      
      # Image on the right
      column(4,
             tags$img(src = "img/antibiotic_book.png", width = "50%")
      )
    ),
    
    fluidRow(id="howto_text",
      column(12,
             # Introduction Section
             h1("How to use the dashboard"),
             tabsetPanel(
               tabPanel("Input Parameter",
             h4(
               tags$ul(
                 tags$li(tags$b("Step 1:")," Users need to select type of patients (adult or children).",
                         tags$br(),tags$br(),
                         tags$img(src = "img/howto1.png", 
                                  # height = "50%",
                                  # width="50%",
                                  style="max-width: 40%;"
                                  )
                 ), 
                 tags$br(),tags$br(),
                 tags$li(tags$b("Step 2:")," Users need to enter required data on the left side of the panel.",
                         tags$br(),tags$br(),
                         tags$img(src = "img/howto2.png", 
                                  # height = "50%",
                                  width="40%"),
                 ),
                 tags$br(),tags$br(),
                 tags$li(tags$b("Step 3:")," Once all the required data is entered, users can review the “SUMMARY OF INPUT DATA” . If any changes are needed, return to Step 2 to update the data.",
                         tags$br(),tags$br(),
                         tags$img(src = "img/howto3.png", 
                                  # height = "50%",
                                  width="40%"),
                 ),
                 tags$br(),tags$br(),
                 tags$li(tags$b("Step 4:")," Click “Run Model” and wait a few seconds for the model to process the data to generate the output.",
                         tags$br(),tags$br(),
                         tags$img(src = "img/howto4.png", 
                                  # height = "50%",
                                  # width="50%"
                                  style="max-width: 100%;"
                                  ),
                 ),
                 tags$br(),tags$br(),
                 tags$li(tags$b("Step 5:")," Review the expected empirical antibiotic use across four different tables under “SUMMARY OF EXPECTED ANTIBIOTIC USAGE (MODEL’S OUTPUT)”.",
                         tags$br(),tags$br(),
                         tags$img(src = "img/howto5.png", 
                                  # height = "50%",
                                  width="40%"),
                         tags$br(),tags$br(),
                 ),
                 tags$li(tags$b("Step 6:")," You can also visualise the expected use by AWaRe category and by antibiotic classes under “VISUALIZATION”.",
                         tags$br(),tags$br(),
                         tags$img(src = "img/howto6.png", 
                                  # height = "50%",
                                  width="40%")
                 )
               ))
               ),
             tabPanel("Input File",
                          h4(
                            tags$ul(
                              tags$li(tags$b("Step 1:")," Download the inputs.",
                                      tags$br(),tags$br(),
                                      tags$img(src = "img/csvfile1.png",width="20%")
                              ), 
                              tags$br(),tags$br(),
                              tags$li(tags$b("Step 2:")," Changes the values of parameters as necessary in the downloaded csv.file.",
                                      tags$br(),tags$br(),
                                      tags$img(src = "img/csvfile2.png",width="40%"),
                              ),
                              tags$br(),tags$br(),
                              tags$li(tags$b("Step 3:")," Save the updated csv.file.",
                                      tags$br(),tags$br(),
                                      tags$img(src = "img/csvfile3.png", width="20%"),
                              ),
                              tags$br(),tags$br(),
                              tags$li(tags$b("Step 4:")," Use “UPLOAD INPUT FILE” button to upload the updated csv.file.",
                                      tags$br(),tags$br(),
                                      tags$img(src = "img/csvfile4.png",width="30%"),
                              ),
                            ))
             )
             )
      )
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
                        br(),
                        tags$b("Model development team:"),
                        br(),
                        "Dr. Myo Maung Maung Swe (email: ", tags$a(href="mailto:myo.swe@ndm.ox.ac.uk", "myo.swe@ndm.ox.ac.uk"),")",
                        br(),
                        "Dr. Cherry Lim (email: ", tags$a(href="mailto:cherry.lim@ndm.ox.ac.uk", "cherry.lim@ndm.ox.ac.uk"),")",
                        br(),
                        "Prof. Ben Cooper (email: ", tags$a(href="mailto:ben.cooper@ndm.ox.ac.uk", "ben.cooper@ndm.ox.ac.uk"),")",
                        br(),
                        br(),
                        tags$b("ADILA project email: "),tags$a(href="mailto:adila@sgul.ac.uk", "adila@sgul.ac.uk"),
                      )
                    ),
                    br(),br(),br(),
                    h4(
                      tags$p(strong("ADILA is funded by the Wellcome Trust, 222051/Z/20/Z"))
                    )
             )
    ),
    
    fluid_design("Summary_model_table", "summary_output", NULL, NULL, NULL),
    fluid_design("Visualization_plot", "Visualization_output1", "Visualization_output2","Visualization_output3" ,NULL),
    
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
            tabPanel(title = HTML("<b>Patients with different infection syndromes</b>"),
              uiOutput("summary_inputs_ui")
            )
          ),
          tabBox(
            title = "",
            tabPanel(title = HTML("<b>Proportion/ Proportion parameters</b>"),
                     uiOutput("summary_inputs_ui2")
            )
          )
      )
    ),
    tags$div(id="partnersImage",
             style = "text-align: center;", 
      tags$img(
        src = "img/partners_image.png", width="75%" ,height="75%"
      )
    ),
    tags$div(id = "goTopButton", "Go to Top"),
  ),
  skin = "black",
  title = "Antibiotic Usage Model"
)

