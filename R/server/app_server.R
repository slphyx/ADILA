#SLPHYX@SHIFT-ENTER

library(shiny)
library(dplyr)
library(gtsummary)
library(gtools)
library(plotly)
library(shinyalert)
library(formattable)

# Define server logic
app_server <- function(session,input, output) {
  # collect value for summary
  value_summary <- reactiveValues()
  value_summary_both <- reactiveValues()
  # Load shinyjs to enable showing/hiding
  useShinyjs()
  value_fin$run <-0
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  observeEvent("", {
    show("Summary_input_table")
    hide("Summary_model_table")
    hide("Visualization_plot")
    disable("Summary_input")
    disable("Summary_model")
    disable("Visualization")
    hide("Summary_bsButton")
    hide("summary_inputs_ui")
    hide("summary_inputs_ui2")
    hide("about_text")
    hide("howto_text")
  }, once = TRUE)

  # goto_intro
  observeEvent(input$goto_intro,{
    value_fin$run <-0
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    disable("Summary_input")
    hide("Summary_input_table")
    disable("Summary_model")
    disable("Visualization")
    hide("Summary_bsButton")
    show("intro_text")
    hide("howto_text")
    hide("about_text")
    hide("Summary_model_table")
    hide("Visualization_plot")
    show("partnersImage")

  })
  
  # goto_howto
  observeEvent(input$goto_howto,{
    value_fin$run <-0
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    disable("Summary_input")
    hide("Summary_input_table")
    disable("Summary_model")
    disable("Visualization")
    hide("Summary_bsButton")
    hide("intro_text")
    hide("about_text")
    hide("Summary_model_table")
    hide("Visualization_plot")
    show("howto_text")
    hide("partnersImage")
  })
  
  
  # goto_simulation
  observeEvent(input$goto_simulation,{
    value_fin$run <-1
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    enable("Summary_input")
    show("Summary_bsButton")

    if(value_fin$finished){
      enable("Summary_model")
      enable("Visualization")
      
    }
    show("Summary_input_table")
    show("summary_inputs_ui")
    show("summary_inputs_ui2")
    hide("intro_text")
    hide("howto_text")
    hide("about_text")
    hide("partnersImage")
    # show only when the simulation has run
    # enable("Summary_model")
    # enable("Visualization")
    
  })
  
  
  # goto_details
  observeEvent(input$goto_about,{
    value_fin$run <-0
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    disable("sidebar_button")
    disable("Summary_input")
    hide("Summary_input_table")
    disable("Summary_model")
    disable("Visualization")
    hide("Summary_bsButton")
    hide("intro_text")
    hide("howto_text")
    show("about_text")
    show("partnersImage")
    hide("Summary_model_table")
    hide("Visualization_plot")
    
  })
  
  # Toggle input parameters panel visibility when the button is clicked
  observeEvent(input$toggle_input, {
    shinyjs::toggle(id = "input_parameters")
  })
  
  
  output$downloadInput <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      if(input$choices_ac == "adult"){
      paste0("input_data_adult.csv")
      }else if(input$choices_ac == "child"){
        paste0("input_data_child.csv")
      }else if(input$choices_ac == "both"){
        paste0("input_data_both.csv")
      }
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      if(input$choices_ac == "adult"){
        # input.adult
      write.csv(data.frame(name_parameter = c(input_big()$adult_cases[,1],input_big()$adult_para_data[,1],rep("first-choice antibiotics",length(input$selected_antibiotics))),
                           value = c(input_big()$adult_cases[,2],input_big()$adult_para_data[,2],input$selected_antibiotics)
                           ) , file,
                row.names=F)
      }else if(input$choices_ac == "child"){
        write.csv(data.frame(name_parameter = c(input_big()$child_cases[,1],input_big()$child_para_data[,1],rep("first-choice antibiotics",length(input$selected_antibiotics))),
                             value = c(input_big()$child_cases[,2],input_big()$child_para_data[,2],input$selected_antibiotics)) , file,
                  row.names=F)
      }else if(input$choices_ac == "both"){
        write.csv(data.frame(name_parameter = c(input_big()$adult_cases[,1],input_big()$adult_para_data[,1],rep("first-choice antibiotics",length(input$selected_antibiotics_adult)),
                                                "END_ADULT",
                                                input_big()$child_cases[,1],input_big()$child_para_data[,1],rep("first-choice antibiotics",length(input$selected_antibiotics_child)),
                                                "END_CHILD"
                                                ),
                             value = c(input_big()$adult_cases[,2],input_big()$adult_para_data[,2],input$selected_antibiotics_adult,
                             "END_ADULT",
                             input_big()$child_cases[,2],input_big()$child_para_data[,2],input$selected_antibiotics_child,
                             "END_CHILD"
                             )) , file,
                  row.names=F)
      }
    }
  )


  # Reactive block to compute input.adult
  input_big <- reactive({
    req(!is.null(input$cap_severe_single_adult) & !is.null(input$cap_severe_single_child) & !is.null(input$cap_severe_adult)& !is.null(input$cap_severe_child))
    # write.csv
    outlist <- NULL
    
    if(input$choices_ac == "adult" || input$choices_ac == "both"){
    # Inputs
    # std_err <- input$std_err / 100 # Convert percentage to decimal
    std_err <- 0.2   # fixed at 20%
    if(input$choices_ac == "adult"){
    adult_cases <- data.frame(
      syndrome = c("Patients with community acquired pneumonia (CAP)", 
                   "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                   "Patients with bacterial meningitis", 
                   "Patients with intra-abdominal infection (IA)",
                   "Patients with upper UTI",
                   "Patients with skin and soft-tissue infection (SST)", 
                   "Patients with bone and joint infections (BJ)", 
                   "Patients with Clostridioides difficile infection (CDIF)", 
                   "Patients with febrile neutropenia (FN)", 
                   "Patients with sepsis/ septic shock (SEPSIS)", 
                   "Patients for surgical prophylaxis (SP)"),
      syndrome2 = c("Patients with community acquired pneumonia (CAP)", 
                    "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                    "Patients with bacterial meningitis", 
                    "Patients with intra-abdominal infection",
                    "Patients with Community acquired pyelonephritis",
                    "Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)", 
                    "Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)", 
                    "Patients with Clostridioides difficile infection (CDIF)", 
                    "Patients with febrile neutropenic", 
                    "Patients with sepsis & septic shock", 
                    "Patients on surgical prophylaxis"),
      cases = c(input$cap_cases,    input$hap_cases, input$bm_cases , input$ia_cases,
                input$uut_cases,    input$sst_cases,    input$bji_cases,   input$cdif_cases, 
                input$fn_cases,     input$sepsis_cases, input$sp_cases)
    )
    ######
    p_first <- length(input$selected_antibiotics)/length(first_choice_list_adult$Antibiotic)
    if(p_first > 1){
      p_first <- 1
    }
    adult_para.data <- data.frame(
      parameter = c("probability of first-choice antibiotics", 
                    "proportion of severe cases in CAP patients", 
                    "probability(risk) of multi-drug resistant infection in HAP patients", 
                    "proportion of severe cases in patients with intra-abdominal infection",
                    "proportion of severe cases in patients with acute pyelonephritis (upper UTI)", 
                    "proportion of severe cases in patients with C. difficile infection", 
                    "proportion of necrotizing fasciitis cases in patient with SST",
                    "prevalence of ESBL", 
                    "prevalence of MRSA", 
                    "prevalence of Strep pyogenes infection in necrotizing fasciitis",
                    "total admitted patients"),
      parameter2 = c("Proportion of recommended first-choice antibiotics which are available in the hospital", 
                    "Proportion of CAP cases which are severe", 
                    "Proportion of multi-drug resistance (MDR) infection among patients with hospital acquired pneumonia (non-ventilator associated)", 
                    "Proportion of intra-abdominal infection cases which are severe",
                    "Proportion of upper UTI cases which are severe", 
                    "Proportion of C. difficile infection (CDI) cases which are severe", 
                    "Proportion of necrotizing fasciitis (NF) among patients with skin and soft-tissue (SST) infection",
                    "Proportion of ESBL producing Escherichia coli among bloodstream infections with Escherichia coli", 
                    "Proportion of MRSA among bloodstream infections with Staphylococcus aureus", 
                    "Proportion of Streptococcus pyogenes infection in patients with necrotizing fasciitis",
                    "Total inpatients in a given day"),
      value = c(p_first, 
                input$cap_severe_single_adult, 
                input$hap_mdr, 
                input$abd_severe,
                input$uti_severe, 
                input$cdf_severe, 
                input$sst_nf, 
                input$esbl_prevalence,
                input$mrsa_prevalence, 
                input$strep_pyogenes, 
                input$admitted_patients)
    )
    }else{
      adult_cases <- data.frame(
        syndrome = c("Patients with community acquired pneumonia (CAP)", 
                     "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                     "Patients with bacterial meningitis", 
                     "Patients with intra-abdominal infection (IA)",
                     "Patients with upper UTI",
                     "Patients with skin and soft-tissue infection (SST)", 
                     "Patients with bone and joint infections (BJ)", 
                     "Patients with Clostridioides difficile infection (CDIF)", 
                     "Patients with febrile neutropenia (FN)", 
                     "Patients with sepsis/ septic shock (SEPSIS)", 
                     "Patients for surgical prophylaxis (SP)"),
        syndrome2 = c("Patients with community acquired pneumonia (CAP)", 
                      "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                      "Patients with bacterial meningitis", 
                      "Patients with intra-abdominal infection",
                      "Patients with Community acquired pyelonephritis",
                      "Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)", 
                      "Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)", 
                      "Patients with Clostridioides difficile infection (CDIF)", 
                      "Patients with febrile neutropenic", 
                      "Patients with sepsis & septic shock", 
                      "Patients on surgical prophylaxis"),
        cases = c(input$cap_cases_adult,    input$hap_cases_adult, input$bm_cases_adult , input$ia_cases_adult,
                  input$uut_cases_adult,    input$sst_cases_adult,    input$bji_cases_adult,   input$cdif_cases_adult, 
                  input$fn_cases_adult,     input$sepsis_cases_adult, input$sp_cases_adult)
      )
      p_first <- length(input$selected_antibiotics_adult)/length(first_choice_list_adult$Antibiotic)
      
      adult_para.data <- data.frame(
        parameter = c("probability of first-choice antibiotics", 
                      "proportion of severe cases in CAP patients", 
                      "probability(risk) of multi-drug resistant infection in HAP patients", 
                      "proportion of severe cases in patients with intra-abdominal infection",
                      "proportion of severe cases in patients with acute pyelonephritis (upper UTI)", 
                      "proportion of severe cases in patients with C. difficile infection", 
                      "proportion of necrotizing fasciitis cases in patient with SST",
                      "prevalence of ESBL", 
                      "prevalence of MRSA", 
                      "prevalence of Strep pyogenes infection in necrotizing fasciitis",
                      "total admitted patients"),
        parameter2 = c("Proportion of recommended first-choice antibiotics which are available in the hospital", 
                       "Proportion of CAP cases which are severe", 
                       "Proportion of multi-drug resistance (MDR) infection among patients with hospital acquired pneumonia (non-ventilator associated)", 
                       "Proportion of intra-abdominal infection cases which are severe",
                       "Proportion of upper UTI cases which are severe", 
                       "Proportion of C. difficile infection (CDI) cases which are severe", 
                       "Proportion of necrotizing fasciitis (NF) among patients with skin and soft-tissue (SST) infection",
                       "Proportion of ESBL producing Escherichia coli among bloodstream infections with Escherichia coli", 
                       "Proportion of MRSA among bloodstream infections with Staphylococcus aureus", 
                       "Proportion of Streptococcus pyogenes infection in patients with necrotizing fasciitis",
                       "Total inpatients in a given day"),
        value = c(p_first, 
                  input$cap_severe_adult, 
                  input$hap_mdr_adult, 
                  input$abd_severe_adult,
                  input$uti_severe_adult, 
                  input$cdf_severe_adult, 
                  input$sst_nf_adult, 
                  input$esbl_prevalence_adult,
                  input$mrsa_prevalence_adult, 
                  input$strep_pyogenes_adult, 
                  input$admitted_patients_adult)
      )
    }

    # Probability of each infection syndromes from the "adult_cases" data 
    syd.adult <- rdirichlet(1, adult_cases$cases)
    
    # Total patients on antimicrobial
    pt.atb   <- sum(adult_cases$cases)
    # Total admitted patients
    pt.admt <- adult_para.data$value[adult_para.data$parameter=="total admitted patients"]
    # Probability of first-choice antibiotics
    p.first.est <- adult_para.data$value[adult_para.data$parameter=="probability of first-choice antibiotics"]

    p.first.para.alpha <- (p.first.est * 100) + 1
    p.first.para.beta  <- ((1-p.first.est) * 100) + 1

    p.first <- rbeta(1, p.first.para.alpha, p.first.para.beta)
    
    # Availability of first-choice antibiotic for specific infections
    p.first.cap <- p.first.bm <- p.first.abd <- p.first.bj <- p.first
    # Availability of first-choice antibiotic for different type of surgical prophylaxis
    p.first.bow.surg <- p.first.clean.cont.surg <- p.first.uro.surg <- p.first.cont.surg <- p.first
    
    
    # Prevalence of ESBL
    p.esbl.est  <- adult_para.data$value[adult_para.data$parameter=="prevalence of ESBL"]
    p.esbl.para.alpha <- (p.esbl.est * 100) + 1
    p.esbl.para.beta  <- ((1-p.esbl.est) * 100) + 1
    p.esbl <- rbeta(1, p.esbl.para.alpha, p.esbl.para.beta)
    
    # Assume the same prevalence/ risk of ESBL for different infection syndromes
    p.esbl.abd <- p.esbl.uti <- p.esbl.fn <- p.g.neg.fn <- p.esbl
    
    # Prevalence of MRSA
    p.mrsa.est  <- adult_para.data$value[adult_para.data$parameter=="prevalence of MRSA"]
    p.mrsa.para.alpha <- (p.mrsa.est * 100) + 1
    p.mrsa.para.beta  <- ((1-p.mrsa.est) * 100) + 1
    p.mrsa <- rbeta(1, p.mrsa.para.alpha, p.mrsa.para.beta)
    
    # Assume the same prevalence/ risk of ESBL for different infection syndromes
    p.mrsa.sst <- p.mrsa.bj <- p.mrsa.fn <- p.mrsa
    
    # Prevalence of Strep pyogenes in patients with necrotizing fasciitis
    p.Strep.pyogene.est  <- adult_para.data$value[adult_para.data$parameter=="prevalence of Strep pyogenes infection in necrotizing fasciitis"]
    p.Strep.pyogene.para.alpha <- (p.Strep.pyogene.est * 100) + 1
    p.Strep.pyogene.para.beta  <- ((1-p.Strep.pyogene.est) * 100) + 1
    p.Strep.pyogene      <- rbeta(1, p.Strep.pyogene.para.alpha, p.Strep.pyogene.para.beta)
    
    # Severity of cases
    # Probability of severe CAP cases
    p.sev.cap.est <- adult_para.data$value[adult_para.data$parameter=="proportion of severe cases in CAP patients"]
    alpha.cap <- p.sev.cap.est * adult_cases$cases[adult_cases$syndrome=="Patients with community acquired pneumonia (CAP)"]
    beta.cap  <- (1- p.sev.cap.est) * adult_cases$cases[adult_cases$syndrome=="Patients with community acquired pneumonia (CAP)"]
    p.sev.cap <- rbeta(1, 1+alpha.cap, 1+beta.cap)
    
    p.curb.high <- p.sev.cap # Assume CURB > 2 = proportion of severe CAP
    
    # Probability of high risk of multi-drug resistant infeciton in HAP
    p.highrisk.hap.est <- adult_para.data$value[adult_para.data$parameter=="probability(risk) of multi-drug resistant infection in HAP patients"]
    alpha.hap <- p.highrisk.hap.est * adult_cases$cases[adult_cases$syndrome=="Patients with hospital acquired pneumonia (HAP) Non-VAP"]
    beta.hap  <-  (1 - p.highrisk.hap.est) * adult_cases$cases[adult_cases$syndrome=="Patients with hospital acquired pneumonia (HAP) Non-VAP"]
    p.highrisk.hap <- rbeta(1, 1+alpha.hap, 1+beta.hap)
    
    
    # Probability of cases with severe intra-abdominal infection
    p.sev.abd.est <- adult_para.data$value[adult_para.data$parameter=="proportion of severe cases in patients with intra-abdominal infection"]
    alpha.abd <- p.sev.abd.est * adult_cases$cases[adult_cases$syndrome=="Patients with intra-abdominal infection (IA)"]
    beta.abd  <- (1 - p.sev.abd.est) * adult_cases$cases[adult_cases$syndrome=="Patients with intra-abdominal infection (IA)"]
    p.sev.abd <- rbeta(1, 1+alpha.abd, 1+beta.abd)

    # Probability of upper UTI (Pyelonephritis) with severe cases
    p.sev.uti.est <- adult_para.data$value[adult_para.data$parameter=="proportion of severe cases in patients with acute pyelonephritis (upper UTI)"]
    alpha.uti <- p.sev.uti.est * adult_cases$cases[adult_cases$syndrome=="Patients with upper UTI"]
    beta.uti  <- (1 - p.sev.uti.est) * adult_cases$cases[adult_cases$syndrome=="Patients with upper UTI"]
    p.sev.uti <- rbeta(1, 1+alpha.uti, 1+beta.uti)
    
    # Probability of C. difficile with severe cases
    p.sev.cdf.est <- adult_para.data$value[adult_para.data$parameter=="proportion of severe cases in patients with C. difficile infection"]
    alpha.cdf <- p.sev.cdf.est * adult_cases$cases[adult_cases$syndrome=="Patients with Clostridioides difficile infection (CDIF)"]
    beta.cdf  <- (1 - p.sev.cdf.est) * adult_cases$cases[adult_cases$syndrome=="Patients with Clostridioides difficile infection (CDIF)"]
    p.sev.cdf <- rbeta(1, 1+alpha.cdf, 1+beta.cdf)
    
    # Probability of "Necrotizing fasciitis" among patients with skin and soft-tissue infection
    p.nf.est <- adult_para.data$value[adult_para.data$parameter=="proportion of necrotizing fasciitis cases in patient with SST"]
    alpha.nf <- p.nf.est * adult_cases$cases[adult_cases$syndrome=="Patients with skin and soft-tissue infection (SST)"]
    beta.nf  <- (1- p.nf.est) * adult_cases$cases[adult_cases$syndrome=="Patients with skin and soft-tissue infection (SST)"]
    p.nf <- rbeta(1, 1+alpha.nf, 1+beta.nf)
    
    
    # Underlying causes of sepsis
    und.sepsis <- rdirichlet(1, c(10,10,10,10,10,10,10))
    
    # Type of surgical prophylaxis
    type.surg <- rdirichlet(1, c(10,10,10,10))
    
    
    
    # Create a dataframe using the above model parameters
    input.adult <- data.frame(
      
      # Patients on Antimicrobial
      pt.atb   = pt.atb,
      # Total admitted patients
      pt.admt  = pt.admt,
      
      # Probability of first choice antibiotic
      p.first.cap = p.first.cap,
      p.first.bm  = p.first.bm,
      p.first.abd = p.first.abd,
      p.first.bj  = p.first.bj,
      
      # Probability of each syndrome
      p.cap     = syd.adult[1],
      p.hap     = syd.adult[2],
      p.bmen    = syd.adult[3],
      p.abd     = syd.adult[4],
      p.uuti    = syd.adult[5],
      p.sst     = syd.adult[6],
      p.bj      = syd.adult[7],
      p.clodiff = syd.adult[8],
      p.fneut   = syd.adult[9],
      p.sep     = syd.adult[10],
      p.sug     = syd.adult[11],
      
      # Severity for each syndrome
      
      p.sev.cap      = p.sev.cap,
      p.curb.high    = p.curb.high,
      p.highrisk.hap = p.highrisk.hap,
      p.sev.abd      = p.sev.abd,
      p.sev.uti      = p.sev.uti,
      p.sev.cdf      = p.sev.cdf,
      
      # Probability of necrotizing fasciitis and pyomyositis among pt with SST
      p.nf    = p.nf,
      p.pyomy = (1- p.nf),
      
      # Prevalence of AMR
      p.esbl.abd = p.esbl.abd,
      p.esbl.uti = p.esbl.uti,
      p.esbl.fn  = p.esbl.fn,
      p.g.neg.fn = p.g.neg.fn,
      
      p.mrsa.sst = p.mrsa.sst,
      p.mrsa.bj  = p.mrsa.bj,
      p.mrsa.fn  = p.mrsa.fn,
      
      p.Strep.pyogene = p.Strep.pyogene, # Probability of Strep pyogenes infection in necrotizing fasciitis
      p.snf           = p.nf,           # Probability of suspected necrotizing fasciitis in sepsis patients with SST
      
      # Underlying causes of sepsis
      p.unk    = und.sepsis[1],
      p.men    = und.sepsis[2],
      p.lrti   = und.sepsis[3],
      p.ent    = und.sepsis[4],
      p.abd.s  = und.sepsis[5],
      p.sst.s  = und.sepsis[6],
      p.uti.s  = und.sepsis[7],
      
      # Type of surgical prophylaxis
      p.bowsg  = type.surg[1],
      p.clean  = type.surg[2],
      p.urosg  = type.surg[3],
      p.contsg = type.surg[4]
    )
    

     # Store the inputs in a list
    outlist <-list(
        std_err = std_err,   # fixed at 20 %
        adult_cases = adult_cases,
        adult_para_data = adult_para.data,
        input.adult = input.adult
      )
    }
    if(input$choices_ac == "child" || input$choices_ac == "both"){
      std_err     <- 0.2     # Standard error (assumed 20%)
      req(!is.null(input$hiv_severe))
      
      if(input$choices_ac == "child"){
      # A dataframe with number of cases with 11 type of clinical infection syndromes 
      
      child_cases <- data.frame(
        syndrome = c("Patients with community acquired pneumonia (CAP)", 
                     "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                     "Patients with bacterial meningitis", 
                     "Patients with intra-abdominal infection (IA)",
                     "Patients with upper UTI",
                     "Patients with skin and soft-tissue infection (SST)", 
                     "Patients with bone and joint infections (BJ)", 
                     "Patients with Clostridioides difficile infection (CDIF)", 
                     "Patients with febrile neutropenia (FN)", 
                     "Patients with sepsis/ septic shock (SEPSIS)", 
                     "Patients for surgical prophylaxis (SP)"),
        syndrome2 = c("Patients with community acquired pneumonia (CAP)", 
                      "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                      "Patients with bacterial meningitis", 
                      "Patients with intra-abdominal infection",
                      "Patients with Community acquired pyelonephritis",
                      "Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)", 
                      "Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)", 
                      "Patients with Clostridioides difficile infection (CDIF)", 
                      "Patients with febrile neutropenic", 
                      "Patients with sepsis & septic shock", 
                      "Patients on surgical prophylaxis"),
        cases = c(input$cap_cases,    input$hap_cases, input$bm_cases , input$ia_cases,
                  input$uut_cases,    input$sst_cases,    input$bji_cases,   input$cdif_cases,
                  input$fn_cases,     input$sepsis_cases, input$sp_cases)
      )
      p_first <- length(input$selected_antibiotics)/length(first_choice_list_child$Antibiotic)
      if(p_first > 1){
        p_first <- 1
      }
      # Parameter value 
      child.para.data <- data.frame(
        parameter = c("probability of first-choice antibiotics", 
                      "proportion of severe cases in CAP patients", 
                      "proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr", 
                      "proportion(risk) of severe CAP patients with HIV infection",
                      "probability(risk) of multi-drug resistant infection in HAP patients", 
                      "proportion of severe cases in patients with intra-abdominal infection",
                      "proportion of severe cases in patients with acute pyelonephritis (upper UTI)", 
                      "proportion of severe cases in patients with C. difficile infection", 
                      "proportion of necrotizing fasciitis cases in patient with SST",
                      "prevalence of ESBL", "prevalence of MRSA", 
                      "prevalence of Strep pyogenes infection in necrotizing fasciitis",
                      "total admitted patients"),
        parameter2 = c("Proportion of recommended first-choice antibiotics which are available in the hospital", 
                       "Proportion of CAP cases which are severe",
                       "proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr", 
                       "proportion of severe CAP patients with HIV infection",
                       "Proportion of multi-drug resistance (MDR) infection among patients with hospital acquired pneumonia (non-ventilator associated)", 
                       "Proportion of intra-abdominal infection cases which are severe",
                       "Proportion of upper UTI cases which are severe", 
                       "Proportion of C. difficile infection (CDI) cases which are severe", 
                       "Proportion of necrotizing fasciitis (NF) among patients with skin and soft-tissue (SST) infection",
                       "Proportion of ESBL producing Escherichia coli among bloodstream infections with Escherichia coli", 
                       "Proportion of MRSA among bloodstream infections with Staphylococcus aureus", 
                       "Proportion of Streptococcus pyogenes infection in patients with necrotizing fasciitis",
                       "Total inpatients in a given day"),
        value = c(p_first,
                  #input$p_first, 
                  input$cap_severe_single_child, 
                  input$cap_no_resp,
                  input$hiv_severe,
                  input$hap_mdr, 
                  input$abd_severe,
                  input$uti_severe, 
                  input$cdf_severe, 
                  input$sst_nf, 
                  input$esbl_prevalence,
                  input$mrsa_prevalence, 
                  input$strep_pyogenes, 
                  input$admitted_patients)
      )
      
      }else{
        child_cases <- data.frame(
          syndrome = c("Patients with community acquired pneumonia (CAP)", 
                       "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                       "Patients with bacterial meningitis", 
                       "Patients with intra-abdominal infection (IA)",
                       "Patients with upper UTI",
                       "Patients with skin and soft-tissue infection (SST)", 
                       "Patients with bone and joint infections (BJ)", 
                       "Patients with Clostridioides difficile infection (CDIF)", 
                       "Patients with febrile neutropenia (FN)", 
                       "Patients with sepsis/ septic shock (SEPSIS)", 
                       "Patients for surgical prophylaxis (SP)"),
          syndrome2 = c("Patients with community acquired pneumonia (CAP)", 
                        "Patients with hospital acquired pneumonia (HAP) Non-VAP", 
                        "Patients with bacterial meningitis", 
                        "Patients with intra-abdominal infection",
                        "Patients with Community acquired pyelonephritis",
                        "Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)", 
                        "Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)", 
                        "Patients with Clostridioides difficile infection (CDIF)", 
                        "Patients with febrile neutropenic", 
                        "Patients with sepsis & septic shock", 
                        "Patients on surgical prophylaxis"),
          cases = c(input$cap_cases_child,    input$hap_cases_child, input$bm_cases_child , input$ia_cases_child,
                    input$uut_cases_child,    input$sst_cases_child,    input$bji_cases_child,   input$cdif_cases_child,
                    input$fn_cases_child,     input$sepsis_cases_child, input$sp_cases_child)
        )
        # Parameter value 
        p_first <- length(input$selected_antibiotics_child)/length(first_choice_list_child$Antibiotic)
        child.para.data <- data.frame(
          parameter = c("probability of first-choice antibiotics", 
                        "proportion of severe cases in CAP patients", 
                        "proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr", 
                        "proportion(risk) of severe CAP patients with HIV infection",
                        "probability(risk) of multi-drug resistant infection in HAP patients", 
                        "proportion of severe cases in patients with intra-abdominal infection",
                        "proportion of severe cases in patients with acute pyelonephritis (upper UTI)", 
                        "proportion of severe cases in patients with C. difficile infection", 
                        "proportion of necrotizing fasciitis cases in patient with SST",
                        "prevalence of ESBL", "prevalence of MRSA", 
                        "prevalence of Strep pyogenes infection in necrotizing fasciitis",
                        "total admitted patients"),
          parameter2 = c("Proportion of recommended first-choice antibiotics which are available in the hospital", 
                         "Proportion of CAP cases which are severe",
                         "proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr", 
                         "proportion of severe CAP patients with HIV infection",
                         "Proportion of multi-drug resistance (MDR) infection among patients with hospital acquired pneumonia (non-ventilator associated)", 
                         "Proportion of intra-abdominal infection cases which are severe",
                         "Proportion of upper UTI cases which are severe", 
                         "Proportion of C. difficile infection (CDI) cases which are severe", 
                         "Proportion of necrotizing fasciitis (NF) among patients with skin and soft-tissue (SST) infection",
                         "Proportion of ESBL producing Escherichia coli among bloodstream infections with Escherichia coli", 
                         "Proportion of MRSA among bloodstream infections with Staphylococcus aureus", 
                         "Proportion of Streptococcus pyogenes infection in patients with necrotizing fasciitis",
                         "Total inpatients in a given day"),
          value = c(p_first, 
                    input$cap_severe_child, 
                    input$cap_no_resp_child,
                    input$hiv_severe_child,
                    input$hap_mdr_child, 
                    input$abd_severe_child,
                    input$uti_severe_child, 
                    input$cdf_severe_child, 
                    input$sst_nf_child, 
                    input$esbl_prevalence_child,
                    input$mrsa_prevalence_child, 
                    input$strep_pyogenes_child, 
                    input$admitted_patients_child)
        )
      }
      

      
      
      # Probability of each infection syndromes from the "child_cases" data 
      syd.child <- rdirichlet(1, child_cases$cases)
      
      # Total patients on antimicrobial 
      pt.atb   <- sum(child_cases$cases)
      # Total admitted patients
      pt.admt <- child.para.data$value[child.para.data$parameter=="total admitted patients"]
      # Probability of first-choice antibiotics
      p.first.est <- child.para.data$value[child.para.data$parameter=="probability of first-choice antibiotics"]
      p.first.para.alpha <- (p.first.est * 100) + 1
      p.first.para.beta  <- ((1-p.first.est) * 100) + 1
      p.first <- rbeta(1, p.first.para.alpha, p.first.para.beta)
      
      # Availability of first-choice antibiotic for specific infections 
      p.first.cap <- p.first.bm <- p.first.abd <- p.first.bj <- p.first
      # Availability of first-choice antibiotic for different type of surgical prophylaxis
      p.first.bow.surg <- p.first.clean.cont.surg <- p.first.uro.surg <- p.first.cont.surg <- p.first
      
      
      # Prevalence of ESBL
      p.esbl.est  <- child.para.data$value[child.para.data$parameter=="prevalence of ESBL"]
      p.esbl.para.alpha <- (p.esbl.est * 100) + 1
      p.esbl.para.beta  <- ((1-p.esbl.est) * 100) + 1
      p.esbl <- rbeta(1, p.esbl.para.alpha, p.esbl.para.beta)
      
      # Assume the same prevalence/ risk of ESBL for different infection syndromes 
      p.esbl.abd <- p.esbl.uti <- p.esbl.fn <- p.g.neg.fn <- p.esbl
      
      # Prevalence of MRSA
      p.mrsa.est  <- child.para.data$value[child.para.data$parameter=="prevalence of MRSA"]
      p.mrsa.para.alpha <- (p.mrsa.est * 100) + 1
      p.mrsa.para.beta  <- ((1-p.mrsa.est) * 100) + 1
      p.mrsa <- rbeta(1, p.mrsa.para.alpha, p.mrsa.para.beta)
      
      # Assume the same prevalence/ risk of ESBL for different infection syndromes 
      p.mrsa.sst <- p.mrsa.bj <- p.mrsa.fn <- p.mrsa
      
      # Prevalence of Strep pyogenes in patients with necrotizing fasciitis
      p.Strep.pyogene.est  <- child.para.data$value[child.para.data$parameter=="prevalence of Strep pyogenes infection in necrotizing fasciitis"]
      p.Strep.pyogene.para.alpha <- (p.Strep.pyogene.est * 100) + 1
      p.Strep.pyogene.para.beta  <- ((1-p.Strep.pyogene.est) * 100) + 1
      p.Strep.pyogene      <- rbeta(1, p.Strep.pyogene.para.alpha, p.Strep.pyogene.para.beta)
      
      # Severity of cases
      # Probability of severe CAP cases
      p.sev.cap.est <- child.para.data$value[child.para.data$parameter=="proportion of severe cases in CAP patients"]  
      alpha.cap <- p.sev.cap.est * child_cases$cases[child_cases$syndrome=="Patients with community acquired pneumonia (CAP)"]
      beta.cap  <- (1- p.sev.cap.est) * child_cases$cases[child_cases$syndrome=="Patients with community acquired pneumonia (CAP)"]
      p.sev.cap <- rbeta(1, 1+alpha.cap, 1+beta.cap)
      
      
      # Probability of treatment failure to first line antibiotics after 48 hr in patients with CAP
      p.no.resp.cap.est  <- child.para.data$value[child.para.data$parameter=="proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr"] 
      p.no.resp.cap.para.alpha <- (p.no.resp.cap.est * 100) + 1
      p.no.resp.cap.para.beta  <- ((1-p.no.resp.cap.est) * 100) + 1
      p.no.resp.cap <- rbeta(1, p.no.resp.cap.para.alpha, p.no.resp.cap.para.beta)
      
      # Probability of HIV in patients with severe CAP cases
      p.hiv.est  <- child.para.data$value[child.para.data$parameter=="proportion(risk) of severe CAP patients with HIV infection"]  
      p.hiv.para.alpha <- (p.hiv.est * 100) + 1
      p.hiv.para.beta  <- ((1-p.hiv.est) * 100) + 1
      p.hiv <- rbeta(1, p.hiv.para.alpha, p.hiv.para.beta)
      
      
      # Probability of high risk of multi-drug resistant infection in HAP
      p.highrisk.hap.est <- child.para.data$value[child.para.data$parameter=="probability(risk) of multi-drug resistant infection in HAP patients"]  
      alpha.hap <- p.highrisk.hap.est * child_cases$cases[child_cases$syndrome=="Patients with hospital acquired pneumonia (HAP) Non-VAP"]
      beta.hap  <-  (1 - p.highrisk.hap.est) * child_cases$cases[child_cases$syndrome=="Patients with hospital acquired pneumonia (HAP) Non-VAP"]
      p.highrisk.hap <- rbeta(1, 1+alpha.hap, 1+beta.hap)
      
      
      # Probability of cases with severe intra-abdominal infection
      p.sev.abd.est <- child.para.data$value[child.para.data$parameter=="proportion of severe cases in patients with intra-abdominal infection"] 
      alpha.abd <- p.sev.abd.est * child_cases$cases[child_cases$syndrome=="Patients with intra-abdominal infection (IA)"]
      beta.abd  <- (1 - p.sev.abd.est) * child_cases$cases[child_cases$syndrome=="Patients with intra-abdominal infection (IA)"]
      p.sev.abd <- rbeta(1, 1+alpha.abd, 1+beta.abd)

      # Probability of upper UTI (Pyelonephritis) with severe cases
      p.sev.uti.est <- child.para.data$value[child.para.data$parameter=="proportion of severe cases in patients with acute pyelonephritis (upper UTI)"]
      alpha.uti <- p.sev.uti.est * child_cases$cases[child_cases$syndrome=="Patients with upper UTI"]
      beta.uti  <- (1 - p.sev.uti.est) * child_cases$cases[child_cases$syndrome=="Patients with upper UTI"]
      p.sev.uti <- rbeta(1, 1+alpha.uti, 1+beta.uti)
      
      # Probability of C. difficile with severe cases
      p.sev.cdf.est <- child.para.data$value[child.para.data$parameter=="proportion of severe cases in patients with C. difficile infection"]
      alpha.cdf <- p.sev.cdf.est * child_cases$cases[child_cases$syndrome=="Patients with Clostridioides difficile infection (CDIF)"]
      beta.cdf  <- (1 - p.sev.cdf.est) * child_cases$cases[child_cases$syndrome=="Patients with Clostridioides difficile infection (CDIF)"]
      p.sev.cdf <- rbeta(1, 1+alpha.cdf, 1+beta.cdf)
      
      # Probability of "Necrotizing fasciitis" among patients with skin and soft-tissue infection
      p.nf.est <- child.para.data$value[child.para.data$parameter=="proportion of necrotizing fasciitis cases in patient with SST"]
      alpha.nf <- p.nf.est * child_cases$cases[child_cases$syndrome=="Patients with skin and soft-tissue infection (SST)"]
      beta.nf  <- (1- p.nf.est) * child_cases$cases[child_cases$syndrome=="Patients with skin and soft-tissue infection (SST)"]
      p.nf <- rbeta(1, 1+alpha.nf, 1+beta.nf)
      
      
      # Type of surgical prophylaxis
      type.surg <- rdirichlet(1, c(10,10,10,10))
      
      
      
      # Create a dataframe using the above model parameters
      input.child <- data.frame(
        
        # Patients on Antimicrobial 
        pt.atb   <- pt.atb,
        # Total admitted patients
        pt.admt  <- pt.admt,
        
        # Probability of first choice antibiotic 
        p.first.cap <- p.first.cap, 
        p.first.bm  <- p.first.bm,
        p.first.abd <- p.first.abd,
        p.first.bj  <- p.first.bj, 
        
        # Probability of each syndrome
        p.cap     <- syd.child[1],
        p.hap     <- syd.child[2],
        p.bmen    <- syd.child[3],
        p.abd     <- syd.child[4],
        p.uuti    <- syd.child[5],
        p.sst     <- syd.child[6],
        p.bj      <- syd.child[7],
        p.clodiff <- syd.child[8],
        p.fneut   <- syd.child[9],
        p.sep     <- syd.child[10],
        p.sug     <- syd.child[11],
        
        # Severity for each syndrome 
        
        p.sev.cap      <- p.sev.cap, 
        p.no.resp.cap  <- p.no.resp.cap,
        p.hiv          <- p.hiv,
        p.highrisk.hap <- p.highrisk.hap, 
        p.sev.abd      <- p.sev.abd, 
        p.sev.uti      <- p.sev.uti, 
        p.sev.cdf      <- p.sev.cdf, 
        
        # Probability of necrotizing fasciitis and pyomyositis among pt with SST
        p.nf    <- p.nf,
        p.pyomy <- (1- p.nf), 
        
        # Prevalence of AMR
        p.esbl.abd <- p.esbl.abd,
        p.esbl.uti <- p.esbl.uti,
        p.esbl.fn  <- p.esbl.fn,
        p.g.neg.fn <- p.g.neg.fn,
        
        p.mrsa.sst <- p.mrsa.sst,
        p.mrsa.bj  <- p.mrsa.bj,
        p.mrsa.fn  <- p.mrsa.fn, 
        
        p.Strep.pyogene <- p.Strep.pyogene, # Probability of Strep pyogenes infection in necrotizing fasciitis 
        
        # Type of surgical prophylaxis 
        p.bowsg  <- type.surg[1],
        p.clean  <- type.surg[2],
        p.urosg  <- type.surg[3],
        p.contsg <- type.surg[4]
      )
      if(input$choices_ac == "both"){
        outlist$child_cases <- child_cases
        outlist$child_para_data <- child.para.data
        outlist$input.child <- input.child
      }else{
      # Store the inputs in a list
      outlist <-list(
        std_err = std_err,   # fixed at 20 %
        child_cases = child_cases,
        child_para_data = child.para.data,
        input.child = input.child
      )
      }
    }
    return(outlist)
  })
  
  
  observeEvent(c(input$admitted_patients,input$cap_cases , input$hap_cases,input$bm_cases,
                 input$ia_cases, input$uut_cases,    input$sst_cases,    input$bji_cases, 
                 input$cdif_cases,input$fn_cases,     input$sepsis_cases, input$sp_cases,
                 input$admitted_patients_adult,input$cap_cases_adult , input$hap_cases_adult,input$bm_cases_adult,
                 input$ia_cases_adult, input$uut_cases_adult,    input$sst_cases_adult,    input$bji_cases_adult, 
                 input$cdif_cases_adult,input$fn_cases_adult,     input$sepsis_cases_adult, input$sp_cases_adult,
                 input$admitted_patients_child,input$cap_cases_child , input$hap_cases_child,input$bm_cases_child,
                 input$ia_cases_child, input$uut_cases_child,    input$sst_cases_child,    input$bji_cases_child, 
                 input$cdif_cases_child,input$fn_cases_child,     input$sepsis_cases_child, input$sp_cases_child,
                 input$goto_intro,input$goto_howto,input$goto_simulation,input$goto_about
                 ), {

    if(input$choices_ac != "both"){
      if(input$choices_ac == "child"){
        value_fin$TotalAdmittedPatient <- sum(input_big()$child_cases[["cases"]])
      }else{
        value_fin$TotalAdmittedPatient <- sum(input_big()$adult_cases[["cases"]])
      }
      if (is.na(value_fin$TotalAdmittedPatient) | is.na(input$admitted_patients)) {
          disable("run_model")
      }else if(input$admitted_patients < value_fin$TotalAdmittedPatient){
        shinyalert("Warning!", paste0("The Total inpatients in a given day must be equal to (or) greater than the total number of infection syndrome cases. Please enter a value not less than ",
                                      "\"",value_fin$TotalAdmittedPatient , "\""), type = "error")
        disable("run_model")
      }else if(!value_fin$run){
        disable("run_model")
      }
      else{
        enable("run_model")
      }
    } else if(input$choices_ac == "both"){
      value_fin$TotalAdmittedPatient_adult <- sum(input_big()$adult_cases[["cases"]])
      value_fin$TotalAdmittedPatient_child <- sum(input_big()$child_cases[["cases"]])
      if (is.na(value_fin$TotalAdmittedPatient_adult) | is.na(input$admitted_patients_adult)|
          is.na(value_fin$TotalAdmittedPatient_child) | is.na(input$admitted_patients_child)) {
        disable("run_model")
      }else if(input$admitted_patients_adult < value_fin$TotalAdmittedPatient_adult ){
        shinyalert("Warning!", paste0("The Total inpatients in a given day (Adult) must be equal to (or) greater than the total number of infection syndrome cases. Please enter a value not less than ",
                                      "\"",value_fin$TotalAdmittedPatient_adult , "\""), type = "error")
        disable("run_model")
      }else if(input$admitted_patients_child < value_fin$TotalAdmittedPatient_child){
        shinyalert("Warning!", paste0("The Total inpatients in a given day (Child) must be equal to (or) greater than the total number of infection syndrome cases. Please enter a value not less than ",
                                      "\"",value_fin$TotalAdmittedPatient_child , "\""), type = "error")
        disable("run_model")
      }else if(!value_fin$run){
        disable("run_model")
      }
      else{
        enable("run_model")
      }
    }
  })
  


  # Run the model when the button is clicked
  observeEvent(input$run_model, {
    withProgress(message = 'Simulation in progress…', value = 0, {
    updateButton(session, "Summary_model",
                  icon = icon("spinner", class = "fa-spin"))
    shinyjs::disable("run_model")
    shinyjs::disable("Summary_model")
    # Adult #####
    if(input$choices_ac == "adult"){
    # Create an empty dataframe to store model's output
    result_adult <- data.frame()

    # Get the input_adult_df values (reactive values)
    # Run the model 1000 times
    for (i in 1:1000) {
      set.seed(Sys.time() + i)
      incProgress(1/1000)
      # set.seed(Sys.time())
      # SS
      model_input <- generate_input_dataframe(input_big()$adult_cases,
                                              input_big()$adult_para_data, input_big()$std_err)

      temp <- as.data.frame(t(model_A(model_input)))
      result_adult <- rbind(result_adult, temp)
    }

    # Convert all columns to numeric
    df_numeric <- result_adult %>%
      mutate_all(~ as.numeric(as.character(.)))
    
    # Summary tables----
    ## Table 1 ----
    summary_table_overall <- df_numeric[, 1:8] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median}",  # Display median
        missing = "no",
        digits = all_continuous() ~ 1  # Ensure 1 decimal place for median
      ) %>%
      modify_header(label="Description", stat_0 = "Expected_usage") %>%
      modify_footnote(all_stat_cols() ~ "Median (95% credible intervals), DDD = defined daily dose, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
      modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
      ) %>%
      modify_table_body(
        ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
      ) %>%
      as_tibble()
    
    # Calculate quantiles and add them to the summary table
    quantiles_overall <- df_numeric[, 1:8] %>%
      summarise(across(everything(), list(
        `0.025 Quantile` = ~ quantile(., probs = 0.025),
        `0.975 Quantile` = ~ quantile(., probs = 0.975)
      ))) %>%
      pivot_longer(cols = everything(), names_to = c("Description", ".value"), names_sep = "_") %>%
      mutate(across(where(is.numeric), ~ sprintf("%.1f", .)))
    
    # Combine the median and quantiles into a single column
    summary_table_overall <- summary_table_overall %>%
      left_join(quantiles_overall, by = "Description") %>%
      mutate(`Expected_usage (95% CI)` = paste0(Expected_usage, " (", `0.025 Quantile`, ", ", `0.975 Quantile`, ")")) %>%
      select(Description, `Expected_usage (95% CI)`)
    
    summary_table_overall_percent_access_watch <- df_numeric[, 7:8] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median}% ({p25}%, {p75}%)",
        missing = "no",
        digits = all_continuous() ~ 1
      )%>%
      as_tibble()
    
    value_summary$antibiotic_access <- summary_table_overall_percent_access_watch[1,2]
    value_summary$antibiotic_watch <- summary_table_overall_percent_access_watch[2,2]

    ## Table 2 ------
    summary_table_syndrome_long <- as.data.frame(df_numeric[,9:(9+21)]) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "Description",         # create a column for the original column names
        values_to = "expected_use"     # create a column for the values
      ) 
    
    # Compute summary statistics by syndrome 
    
    summary_table_syndrome_stat <- summary_table_syndrome_long %>%
      group_by(Description) %>%
      mutate(
        median_value = round(median(expected_use, na.rm = TRUE), 1),
        lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
        upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
      ) %>%
      ungroup() %>%
      select(Description, median_value, lower, upper) %>%
      distinct() %>%
      mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
      select(Description, expected_use)
    
    percent_table_syndrome_long <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "Description",         # create a column for the original column names
        values_to = "Expected use (% of total use)"     # create a column for the values
      ) 
    
    # expected use in percentage of total 
    percent_table_syndrome <- percent_table_syndrome_long %>%
      group_by(Description) %>%
      mutate(
        median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
        lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
        upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
      ) %>%
      ungroup() %>%
      select(Description, median_value, lower, upper) %>%
      distinct() %>%
      mutate(
        median_value = paste0(median_value, "%"),
        lower = paste0(lower, "%"),
        upper = paste0(upper, "%"),
        expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
      ) %>%
      select(Description, expected_use)
    
    
    # # Combine the median and quantiles into a single column
    # summary_table_combined <- percent_table_syndrome %>%
    #   left_join(quantiles_overall, by = "Description") %>%
    #   mutate(`Expected_usage (95% CI)` = paste0(Expected_usage, " (", `0.025 Quantile`, ", ", `0.975 Quantile`, ")")) %>%
    #   select(Description, `Expected_usage (95% CI)`)
    
    
    # Store as dataframe
    percent_table_syndrome_df <- percent_table_syndrome %>%
      as_tibble()
    
    # Combine DDD and percent use 
    summary_table_syndrome_ddd_percent <- left_join(summary_table_syndrome_stat, percent_table_syndrome_df, by = "Description")
    
    
    summary_table_syndrome_ddd_percent <- summary_table_syndrome_ddd_percent %>%
      mutate(
        AWaRe_category = str_extract(Description, "Access|Watch"), # Extract "Access" or "Watch"
        Infection_syndromes = str_remove(Description, ".*: ") # Remove everything before ": "
      ) %>%
      select(AWaRe_category, Infection_syndromes, expected_use.x, expected_use.y) %>% # Reorder columns
      mutate(Infection_syndromes = case_when(
        Infection_syndromes == "CAP" ~ "Community acquired pneumonia",
        Infection_syndromes == "HAP(non-VAP)" ~ "Hospital acquired pneumonia (non-VAP)",
        Infection_syndromes == "Upper UTI" ~ "Community acquired pyelonephritis",
        Infection_syndromes == "SST" ~ "Skin and soft-tissue infection",
        Infection_syndromes == "Sepsis" ~ "Sepsis and septic shock",
        TRUE ~ Infection_syndromes # Keep other values unchanged
      )) %>%
      rename(`AWaRe category`      = AWaRe_category,
             `Infection syndromes` = Infection_syndromes,
             `Expected use (DDD)` = expected_use.x,
             `Expected use (% of total use)` = expected_use.y)
    
    ## Table 3 -------
    summary_table_class_access_long <- as.data.frame(df_numeric[, 31:38]) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "Description",         # create a column for the original column names
        values_to = "expected_use"     # create a column for the values
      ) 
    
    # Compute summary statistics by syndrome 
    
    summary_table_class_access <- summary_table_class_access_long %>%
      group_by(Description) %>%
      mutate(
        median_value = round(median(expected_use, na.rm = TRUE), 1),
        lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
        upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
      ) %>%
      ungroup() %>%
      select(Description, median_value, lower, upper) %>%
      distinct() %>%
      mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
      select(Description, expected_use)
    
    # summary_table_class_access <- df_numeric[, 31:38] %>%
    #   tbl_summary(
    #     by = NULL,
    #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    #     missing = "no",
    #     digits = all_continuous() ~ 0
    #   ) %>%
    #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (DOT)") %>%
    #   modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
    #   modify_caption("**Table 3: Expected empirical Access antibiotic usage by antibiotic class**")
    # 
    
    # Store as a dataframe
    summary_table_class_access <- summary_table_class_access %>%
      as_tibble()
    
    summary_table_class_access <- summary_table_class_access %>%
      mutate(`AWaRe category` = "Access")
    
    
    # Percentage "Access" out of total use 
    # expected use in percentage of total 
    access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "Description",         # create a column for the original column names
        values_to = "Expected use (% of total use)"     # create a column for the values
      ) 
    
    # expected use in percentage of total 
    percent_access_df <- access_df %>%
      group_by(Description) %>%
      mutate(
        median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
        lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
        upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
      ) %>%
      ungroup() %>%
      select(Description, median_value, lower, upper) %>%
      distinct() %>%
      mutate(
        median_value = paste0(median_value, "%"),
        lower = paste0(lower, "%"),
        upper = paste0(upper, "%"),
        expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
      ) %>%
      select(Description, expected_use)
    # access_df <- (df_numeric[, 31:38]/ df_numeric[, 1])*100
    # 
    # percent_access_df <- access_df %>%
    #   tbl_summary(
    #     by = NULL,
    #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    #     missing = "no",
    #     digits = all_continuous() ~ 1 
    #   ) %>%
    #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (% of total use)") %>%
    #   modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = Days of Therapy") %>%
    #   modify_caption("**Table 3: Expected (Access) antibiotic usage by antibiotic class**") 
    
    
    
    # Store as a dataframe
    percent_access_df <- percent_access_df %>%
      as_tibble()
    
    
    summary_access_ddd_percent <- left_join(summary_table_class_access,percent_access_df, by = "Description" ) %>%
      select(`AWaRe category`, everything())
    
    
    # DOT "Watch" out of total use 
    summary_table_class_Watch_long <- as.data.frame(df_numeric[, 39:ncol(df_numeric)]) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "Description",         # create a column for the original column names
        values_to = "expected_use"     # create a column for the values
      ) 
    
    # Compute summary statistics by syndrome 
    
    summary_table_class_Watch <- summary_table_class_Watch_long %>%
      group_by(Description) %>%
      mutate(
        median_value = round(median(expected_use, na.rm = TRUE), 1),
        lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
        upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
      ) %>%
      ungroup() %>%
      select(Description, median_value, lower, upper) %>%
      distinct() %>%
      mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
      select(Description, expected_use)
    
    # summary_table_class_watch <- df_numeric[, 39:ncol(df_numeric)] %>%
    #   tbl_summary(
    #     by = NULL,
    #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    #     missing = "no",
    #     digits = all_continuous() ~ 0
    #   ) %>%
    #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (DOT)") %>%
    #   modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
    #   modify_caption("**Table 4: Expected empirical Watch antibiotic usage by antibiotic class**") 
    # 
    
    
    # Store as a dataframe
    summary_table_class_watch <- summary_table_class_Watch %>%
      as_tibble()
    
    
    summary_table_class_watch <- summary_table_class_watch %>%
      mutate(`AWaRe category` = "Watch")
    
    
    # Percentage "Watch" out of total use 
    # expected use in percentage of total 
    watch_df <- ((df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "Description",         # create a column for the original column names
        values_to = "Expected use (% of total use)"     # create a column for the values
      ) 
    
    # expected use in percentage of total 
    percent_watch_df <- watch_df %>%
      group_by(Description) %>%
      mutate(
        median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
        lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
        upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
      ) %>%
      ungroup() %>%
      select(Description, median_value, lower, upper) %>%
      distinct() %>%
      mutate(
        median_value = paste0(median_value, "%"),
        lower = paste0(lower, "%"),
        upper = paste0(upper, "%"),
        expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
      ) %>%
      select(Description, expected_use)
    
    # watch_df <- (df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100
    # 
    # 
    # percent_watch_df <- watch_df %>%
    #   tbl_summary(
    #     by = NULL,
    #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    #     missing = "no",
    #     digits = all_continuous() ~ 1 
    #   ) %>%
    #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (% of total use)") %>%
    #   modify_footnote(all_stat_cols() ~ "Median (IQR), DTT = Days of Therapy") %>%
    #   modify_caption("**Table 3: Expected (Watch) antibiotic usage by antibiotic class**") 
    
    
    
    
    # Store as a dataframe
    percent_watch_df <- percent_watch_df %>%
      as_tibble()
    
    # Merge  Watch DDD and percent 
    summary_watch_ddd_percent <- left_join(summary_table_class_watch,percent_watch_df, by = "Description" ) %>%
      select(`AWaRe category`, everything())
    
    
    # Append Access and Watch 
    atb_class_summary_tbl <- rbind(summary_access_ddd_percent, summary_watch_ddd_percent)%>%
      rename(`AWaRe category`      = `AWaRe category`,
             `Antibiotic class` = Description,
             `Expected use (DDD)` = expected_use.x,
             `Expected use (% of total use)` = expected_use.y)
    
    # collect Antibiotic class and Expected use %
    value_summary$antibiotic_class_access <- atb_class_summary_tbl[atb_class_summary_tbl$`AWaRe category`=="Access",][1,c(2,4)]
    value_summary$antibiotic_class_watch <- atb_class_summary_tbl[atb_class_summary_tbl$`AWaRe category`=="Watch",][1,c(2,4)]
    
    ##Plotting----
    # Change the dataformat for the plot
    df_plot <- df_numeric %>%
      gather(key = "text", value= "value") %>%
      mutate(value = round(as.numeric(value),1))
    
    
    # Distribution of "Access" and "Watch" antibiotic usage out of total
    
    # Plot 1
    # combined plot antibiotic
    combined_plot <- df_plot %>%
      filter(text %in% c("Access antibiotics(%)", "Watch antibiotics(%)")) %>%
      ggplot(aes(x = value, fill = text)) +
      geom_histogram(color = "black", alpha = 0.6, position = "identity", binwidth = 1) +
      scale_fill_manual(
        values = c("Access antibiotics(%)" = "#009E73",  # green
                   "Watch antibiotics(%)"  = "#F0E442"), # yellow
        labels = c("Access antibiotics", "Watch antibiotics")  # Custom legend labels
      ) +
      theme_minimal() +
      labs(title = "Distribution of Expected Antibiotic Usage by AWaRe classification",
           x = "Percentage of Overall Usage", 
           y = "Frequency",
           fill = "AWaRe category") +
      scale_x_continuous(labels = scales::percent_format(scale = 1)) +
      theme(legend.position = "right",
            axis.text  = element_text(size = 10),
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 10))
    
    combined_plot  <- ggplotly(combined_plot)
    # AWaRe group by antibiotic class
    
    df_aware <- data.frame(
      text = c("Penicillins", "Beta-lactam antibiotics plus enzyme inhibitor",
               "First generation cephlosporins", "Aminoglycosides",
               "Nitroimidazoles", "Tetracyclines",
               "Amphenicols", "Lincosamides",
               "Second/ Third generation cephalosporins", "Macrolides",
               "Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal", "Fluroquinolones",
               "Carbapenems", "Glycopeptides"),
      aware = c(rep("Access", time = 8), rep("Watch", time = 6))
    )
    
    
    # Antibiotic class
    df_class <- df_numeric %>%
      select(Penicillins:last_col()) %>%
      gather(key = "text", value= "value") %>%
      mutate(value = round(as.numeric(value),1)) %>%
      left_join(df_aware, by = "text")
    
    # Plot 2
    # Access antibiotic by antibiotic class
    plot_access_class <- df_class %>%
      filter(aware == "Access") %>%
      ggplot( aes(x=value, fill=text)) +
      geom_histogram( color="black", alpha=0.5, position = 'identity', binwidth = 2) +
      scale_fill_viridis(discrete=TRUE) +
      theme_minimal() +
      labs(title = "Distribution of Expected Access Antibiotic Usage",
           x = "Expected usage (DDD)", 
           y = "Frequency",
           fill = "Antibiotic class") +
      theme(
            plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))

    
    plot_access_class <- ggplotly(plot_access_class)
    
    # Plot 3
    # Watch antibiotic by antibiotic class
    plot_watch_class <- df_class %>%
      filter(aware == "Watch") %>%
      ggplot( aes(x=value, fill=text)) +
      geom_histogram(color="black", alpha=0.5, position = 'identity', binwidth = 2) +
      scale_fill_viridis(discrete=TRUE) +
      theme_minimal() +
      labs(title = "Distribution of Expected Watch Antibiotic Usage",
           x = "Expected usage (DDD)", 
           y = "Frequency",
           fill = "Antibiotic class") +
      theme(plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
    
    plot_watch_class <- ggplotly(plot_watch_class)
    
    ## Plot 4 ------
    
    # Percent of overall use 
    percent_table_syndrome <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) 
    
    
    # Wide to long format 
    percent_table_syndrome_long <- as.data.frame(percent_table_syndrome) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "syndrome",         # create a column for the original column names
        values_to = "expected_use"     # create a column for the values
      )
    
    
    # Cleaning and formatting 
    percent_atb_syndrome <- percent_table_syndrome_long %>%
      mutate(
        AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
        syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
      ) %>%
      mutate(syndrome = case_when(
        syndrome == "CAP" ~ "Community acquired pneumonia",
        syndrome == "HAP(non-VAP)" ~ "Hospital acquired pneumonia (non-VAP)",
        syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
        syndrome == "SST" ~ "Skin and soft-tissue infection",
        syndrome == "Sepsis" ~ "Sepsis and septic shock",
        TRUE ~ syndrome # Keep other values unchanged
      ))
    
    
    
    # 1. Compute summary statistics by group
    
    percent_atb_syndrome <- percent_atb_syndrome %>%
      group_by(syndrome, AWaRe_category) %>%
      summarise(
        median_value = median(expected_use, na.rm = TRUE),
        lower = quantile(expected_use, 0.025, na.rm = TRUE),
        upper = quantile(expected_use, 0.975, na.rm = TRUE)
      ) %>%
      ungroup()
    
    
    # 2. Create the bar graph with error bars
    
    # Create an ordering for syndrome based on "Access" median_value
    syndrome_order <- percent_atb_syndrome %>%
      filter(AWaRe_category == "Access") %>%
      arrange(desc(median_value)) %>%
      pull(syndrome)
    
    # Reorder the syndrome factor in the main dataset
    percent_atb_syndrome <- percent_atb_syndrome %>%
      mutate(syndrome = factor(syndrome, levels = syndrome_order))
    # print(percent_atb_syndrome)
    
# Create a ggplot2 bar plot with error bars
    # Create a ggplot2 bar plot with error bars
    aware_syndrome_plot <- percent_atb_syndrome %>%
      ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
        "Syndrome: ", syndrome, "<br>",
        "AWaRe Category: ", AWaRe_category, "<br>",
        "Median: ", round(median_value, 1), "<br>",
        "Lower CI: ", round(lower, 1), "<br>",
        "Upper CI: ", round(upper, 1)
      ))) +
      geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
      geom_errorbar(aes(ymin = lower, ymax = upper),
                    position = position_dodge(width = 0.8),
                    width = 0.20,
                    size = 0.25)  +
      scale_fill_manual(
        values = c("Access" = "#009E73",    # Green for Access
                   "Watch"  = "#F0E442"),   # Yellow for Watch
        labels = c("Access" = "Access antibiotics", 
                   "Watch"  = "Watch antibiotics")
      ) +
      coord_flip() +
      labs(title = "Expected Antibiotic Use by Infection Syndrome",
           x = "",
           y = "Percentage of expected total use for all infection syndromes",
           fill = "AWaRe Category",
           caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
      theme_minimal(base_size = 10) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(axis.text.x = element_text(),
            plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
      )
    
    # Convert ggplot to plotly and customize hover template
    aware_syndrome_plot <- ggplotly(aware_syndrome_plot, tooltip = "text") %>%
      layout(hovermode = "closest")
    
    ## Plot 5 ------
    # Percent for CAP  
    percent_aware_cap <- (df_numeric[,9:10]/ (df_numeric[,9] + df_numeric[,10]) * 100) 
    
    # Percent for HAP (non-VAP)
    percent_aware_hap <- (df_numeric[,11:12]/ (df_numeric[,11] + df_numeric[,12]) * 100) 
    
    # Percent for Bacterial meningitis 
    percent_aware_bm <- (df_numeric[,13:14]/ (df_numeric[,13] + df_numeric[,14]) * 100) 
    
    # Percent for intra-abdominal infection 
    percent_aware_ia <- (df_numeric[,15:16]/ (df_numeric[,15] + df_numeric[,16]) * 100) 
    
    # Percent for acute pyelonephritis
    percent_aware_pye <- (df_numeric[,17:18]/ (df_numeric[,17] + df_numeric[,18]) * 100) 
    
    # Percent for skin and soft-tissue infection 
    percent_aware_sst <- (df_numeric[,19:20]/ (df_numeric[,19] + df_numeric[,20]) * 100) 
    
    # Percent for bone and joint infection
    percent_aware_bj <- (df_numeric[,21:22]/ (df_numeric[,21] + df_numeric[,22]) * 100) 
    
    # Percent for C. difficile infection 
    percent_aware_cdf <- (df_numeric[,23:24]/ (df_numeric[,23] + df_numeric[,24]) * 100) 
    
    # Percent for febrile neutropenia
    percent_aware_fn <- (df_numeric[,25:26]/ (df_numeric[,25] + df_numeric[,26]) * 100) 
    
    # Percent for sepsis  
    percent_aware_sepsis <- (df_numeric[,27:28]/ (df_numeric[,27] + df_numeric[,28]) * 100) 
    
    # Percent for surgical prophylaxis 
    percent_aware_sp <- (df_numeric[,29:30]/ (df_numeric[,29] + df_numeric[,30]) * 100) 
    
    
    percent_aware_syndrome <- cbind(percent_aware_cap, percent_aware_hap, percent_aware_bm, percent_aware_ia,
                                    percent_aware_pye, percent_aware_sst, percent_aware_bj, percent_aware_cdf,
                                    percent_aware_fn, percent_aware_sepsis, percent_aware_sp)
    
    
    
    
    
    # Wide to long format 
    percent_aware_syndrome_long <- as.data.frame(percent_aware_syndrome) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "syndrome",         # create a column for the original column names
        values_to = "expected_use"     # create a column for the values
      )
    
    
    # Cleaning and formatting 
    percent_aware_syndrome <- percent_aware_syndrome_long %>%
      mutate(
        AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
        syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
      ) %>%
      mutate(syndrome = case_when(
        syndrome == "CAP" ~ "Community acquired pneumonia",
        syndrome == "HAP(non-VAP)" ~ "Hospital acquired pneumonia (non-VAP)",
        syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
        syndrome == "SST" ~ "Skin and soft-tissue infection",
        syndrome == "Sepsis" ~ "Sepsis and septic shock",
        TRUE ~ syndrome # Keep other values unchanged
      ))
    
    
    
    # 1. Compute summary statistics by group
    
    percent_aware_syndrome <- percent_aware_syndrome %>%
      group_by(syndrome, AWaRe_category) %>%
      summarise(
        median_value = median(expected_use, na.rm = TRUE),
        lower = quantile(expected_use, 0.025, na.rm = TRUE),
        upper = quantile(expected_use, 0.975, na.rm = TRUE)
      ) %>%
      ungroup()
    
    
    # 2. Create the bar graph with error bars
    
    
    
    # Create an ordering for syndrome based on "Access" median_value
    syndrome_order <- percent_aware_syndrome %>%
      filter(AWaRe_category == "Access") %>%
      arrange(desc(median_value)) %>%
      pull(syndrome)
    
    # Reorder the syndrome factor in the main dataset
    percent_aware_syndrome <- percent_aware_syndrome %>%
      mutate(syndrome = factor(syndrome, levels = syndrome_order))
    
    # Then plot using the reordered factor levels
    aware_ind_syndrome_plot <- percent_aware_syndrome %>%
      ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
        "Syndrome: ", syndrome, "<br>",
        "AWaRe Category: ", AWaRe_category, "<br>",
        "Median: ", round(median_value, 1), "<br>",
        "Lower CI: ", round(lower, 1), "<br>",
        "Upper CI: ", round(upper, 1)
      ))) +
      geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
      geom_errorbar(aes(ymin = lower, ymax = upper),
                    position = position_dodge(width = 0.8),
                    width = 0.20,
                    size = 0.25) +
      scale_fill_manual(
        values = c("Access" = "#009E73",    # Green for Access
                   "Watch"  = "#F0E442"),   # Yellow for Watch
        labels = c("Access" = "Access antibiotics", 
                   "Watch"  = "Watch antibiotics")
      ) +
      coord_flip() + 
      labs(title = "Expected AWaRe Antibiotic Use by Infection Syndrome",
           x = "",
           y = "Percentage of expected total use for each infection syndrome",
           fill = "AWaRe Category",
           caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
      theme_minimal(base_size = 10) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(axis.text.x = element_text(),
            plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
    
    # Convert ggplot to plotly and customize hover template
    aware_ind_syndrome_plot <- ggplotly(aware_ind_syndrome_plot, tooltip = "text") %>%
      layout(hovermode = "closest")
    
    ## plot 6 ######
    # Percentage "Access" out of total use 
    access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100)
    
    # Wide to long format 
    percent_access_long <- as.data.frame(access_df) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "atb_class",         # create a column for the original column names
        values_to = "expected_use"     # create a column for the values
      ) %>%
      mutate(aware = "Access")
    
    
    # Percentage "Watch" out of total use 
    watch_df <- (df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100
    
    # Wide to long format 
    percent_watch_long <- as.data.frame(watch_df) %>%
      pivot_longer(
        cols = everything(),         # pivot all columns
        names_to = "atb_class",         # create a column for the original column names
        values_to = "expected_use"     # create a column for the values
      ) %>%
      mutate(aware = "Watch")
    
    # Append Access and Watch 
    
    
    percent_both <- rbind(percent_access_long, percent_watch_long)
    
    
    # 1. Compute summary statistics by group
    
    percent_aware_atb <- percent_both %>%
      group_by(atb_class) %>%
      mutate(
        median_value = median(expected_use, na.rm = TRUE),
        lower = quantile(expected_use, 0.025, na.rm = TRUE),
        upper = quantile(expected_use, 0.975, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(atb_class, aware, median_value, lower, upper) %>%
      distinct()
    
    # 2. Create the bar graph with error bars
    aware_atbclass_plot <- percent_aware_atb %>%
      ggplot(aes(x = fct_reorder(atb_class, median_value), y = median_value, fill = aware, text = paste(
        "Antibiotic Class: ", atb_class, "<br>",
        "AWaRe Category: ", aware, "<br>",
        "Median: ", round(median_value, 1), "<br>",
        "Lower CI: ", round(lower, 1), "<br>",
        "Upper CI: ", round(upper, 1)
      ))) +
      geom_bar(aes(text = NULL),stat = "identity", 
               position = position_dodge(width = 0.8), 
               color = "black",
               width = 0.8) +
      geom_errorbar(aes(ymin = lower, ymax = upper),
                    position = position_dodge(width = 0.8),
                    width = 0.2,
                    size = 0.25) +
      scale_fill_manual(
        values = c("Access" = "#009E73",    # Green for Access
                   "Watch"  = "#F0E442"),   # Yellow for Watch
        labels = c("Access" = "Access antibiotics", 
                   "Watch"  = "Watch antibiotics")
      ) +
      coord_flip() + 
      labs(
        title = "Expected Antibiotic Use by Antibiotic Class",
        x = "Antibiotic Class",
        y = "Percentage of expected total use for all infection syndromes",
        fill = "AWaRe Category",
        caption = "Note: The bar represents median value and the error bar represents 95% credible intervals."
      ) +
      theme_minimal(base_size = 10) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(
        axis.text.x = element_text(),
        plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
      )
    
    aware_atbclass_plot <- ggplotly(aware_atbclass_plot, tooltip = "text") %>%
      layout(hovermode = "closest")
    # Child #####
    } else if(input$choices_ac == "child"){
      # Create an empty dataframe to store model's output
      result_child <- data.frame()
      
      # Get the input_adult_df values (reactive values)
      
      
      # Run the model 1000 times
      for (i in 1:1000) {
        set.seed(1000+i)
        incProgress(1/1000)
        # set.seed(Sys.time())
        # SS
        model_input <- generate_input_dataframe_child(input_big()$child_cases,
                                                      input_big()$child_para_data, input_big()$std_err)
        
        temp <- as.data.frame(t(model_C(model_input)))
        result_child <- rbind(result_child, temp)
      }
      
      # Convert all columns to numeric
      df_numeric <- result_child %>%
        mutate_all(~ as.numeric(as.character(.)))
      
      # Summary tables----
      # Table 1
      summary_table_overall <- df_numeric[, 1:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median}",  # Display median
          missing = "no",
          digits = all_continuous() ~ 1  # Ensure 1 decimal place for median
        ) %>%
        modify_header(label="Description", stat_0 = "Expected_usage") %>%
        modify_footnote(all_stat_cols() ~ "Median (95% credible intervals), DDD = defined daily dose, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
        modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_tibble()
      
      # Calculate quantiles and add them to the summary table
      quantiles_overall <- df_numeric[, 1:8] %>%
        summarise(across(everything(), list(
          `0.025 Quantile` = ~ quantile(., probs = 0.025),
          `0.975 Quantile` = ~ quantile(., probs = 0.975)
        ))) %>%
        pivot_longer(cols = everything(), names_to = c("Description", ".value"), names_sep = "_") %>%
        mutate(across(where(is.numeric), ~ sprintf("%.1f", .)))
      
      # Combine the median and quantiles into a single column
      summary_table_overall <- summary_table_overall %>%
        left_join(quantiles_overall, by = "Description") %>%
        mutate(`Expected_usage (DOT)` = paste0(Expected_usage, " (", `0.025 Quantile`, ", ", `0.975 Quantile`, ")")) %>%
        select(Description, `Expected_usage (DOT)`)
      # For the 2 percentage rows
      summary_table_overall_percent_access_watch <- df_numeric[, 7:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median}% ({p25}%, {p75}%)",
          missing = "no",
          digits = all_continuous() ~ 1
        )%>%
        as_tibble()
      
      value_summary$antibiotic_access <- summary_table_overall_percent_access_watch[1,2]
      value_summary$antibiotic_watch <- summary_table_overall_percent_access_watch[2,2]
      
      # Table 2 ------
      # Expected use (DOT) for each syndrome 
      
      summary_table_syndrome_long <- as.data.frame(df_numeric[,9:(9+21)]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_syndrome_stat <- summary_table_syndrome_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      percent_table_syndrome_long <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_table_syndrome <- percent_table_syndrome_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      
      # Store as dataframe
      percent_table_syndrome_df <- percent_table_syndrome %>%
        as_tibble()
      
      # Combine DDD and percent use 
      summary_table_syndrome_dot_percent <- left_join(summary_table_syndrome_stat, percent_table_syndrome_df, by = "Description")
      
      # Data table preparation
      summary_table_syndrome_dot_percent <- summary_table_syndrome_dot_percent %>%
        mutate(
          AWaRe_category = str_extract(Description, "Access|Watch"), # Extract "Access" or "Watch"
          Infection_syndromes = str_remove(Description, ".*: ") # Remove everything before ": "
        ) %>%
        select(AWaRe_category, Infection_syndromes, expected_use.x, expected_use.y) %>% # Reorder columns
        mutate(Infection_syndromes = case_when(
          Infection_syndromes == "CAP" ~ "Community acquired pneumonia",
          Infection_syndromes == "HAP" ~ "Hospital acquired pneumonia (non-VAP)",
          Infection_syndromes == "Upper UTI" ~ "Community acquired pyelonephritis",
          Infection_syndromes == "SST" ~ "Skin and soft-tissue infection",
          Infection_syndromes == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ Infection_syndromes # Keep other values unchanged
        )) %>%
        rename(`AWaRe category`      = AWaRe_category,
               `Infection syndromes` = Infection_syndromes,
               `Expected use (DOT)` = expected_use.x,
               `Expected use (% of total use)` = expected_use.y)
      
      
      
      # Table 3 -------
      summary_table_class_access_long <- as.data.frame(df_numeric[, 31:38]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_class_access <- summary_table_class_access_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      summary_table_class_access <- summary_table_class_access %>%
        as_tibble()
      
      summary_table_class_access <- summary_table_class_access %>%
        mutate(`AWaRe category` = "Access")
      
      
      # Percentage "Access" out of total use 
      # expected use in percentage of total 
      access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_access_df <- access_df %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      percent_access_df <- percent_access_df %>%
        as_tibble()
      
      
      summary_access_ddd_percent <- left_join(summary_table_class_access,percent_access_df, by = "Description" ) %>%
        select(`AWaRe category`, everything())
      
      
      # DOT "Watch" out of total use 
      summary_table_class_Watch_long <- as.data.frame(df_numeric[, 39:ncol(df_numeric)]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_class_Watch <- summary_table_class_Watch_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      summary_table_class_watch <- summary_table_class_Watch %>%
        as_tibble()
      
      
      summary_table_class_watch <- summary_table_class_watch %>%
        mutate(`AWaRe category` = "Watch")
      
      
      # Percentage "Watch" out of total use 
      # expected use in percentage of total 
      watch_df <- ((df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_watch_df <- watch_df %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      percent_watch_df <- percent_watch_df %>%
        as_tibble()
      
      # Merge  Watch DDD and percent 
      summary_watch_ddd_percent <- left_join(summary_table_class_watch,percent_watch_df, by = "Description" ) %>%
        select(`AWaRe category`, everything())
      
      
      # Append Access and Watch 
      atb_class_summary_tbl <- rbind(summary_access_ddd_percent, summary_watch_ddd_percent)%>%
        rename(`AWaRe category`      = `AWaRe category`,
               `Antibiotic class` = Description,
               `Expected use (DOT)` = expected_use.x,
               `Expected use (% of total use)` = expected_use.y)
      value_summary$antibiotic_class_access <- atb_class_summary_tbl[atb_class_summary_tbl$`AWaRe category`=="Access",][1,c(2,4)]
      value_summary$antibiotic_class_watch <- atb_class_summary_tbl[atb_class_summary_tbl$`AWaRe category`=="Watch",][1,c(2,4)]
      
      #Plotting----
      # Change the dataformat for the plot
      df_plot <- df_numeric %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1))
      
      
      # Distribution of "Access" and "Watch" antibiotic usage out of total
      
      # Plot 1
      # combined plot antibiotic
      combined_plot <- df_plot %>%
        filter(text %in% c("Access antibiotics(%)", "Watch antibiotics(%)")) %>%
        ggplot(aes(x = value, fill = text)) +
        geom_histogram(color = "black", alpha = 0.6, position = "identity", binwidth = 1) +
        scale_fill_manual(
          values = c("Access antibiotics(%)" = "#009E73",  # green
                     "Watch antibiotics(%)"  = "#F0E442"), # yellow
          labels = c("Access antibiotics", "Watch antibiotics")  # Custom legend labels
        ) +
        theme_minimal() +
        labs(title = "Distribution of Expected Antibiotic Usage by AWaRe classification",
             x = "Percentage of Overall Usage", 
             y = "Frequency",
             fill = "AWaRe category") +
        scale_x_continuous(labels = scales::percent_format(scale = 1)) +
        theme(legend.position = "right",
              axis.text  = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10))
      
      combined_plot
      # AWaRe group by antibiotic class
      
      df_aware <- data.frame(
        text = c("Penicillins", "Beta-lactam antibiotics plus enzyme inhibitor",
                 "First generation cephlosporins", "Aminoglycosides",
                 "Nitroimidazoles", "Sulfonamides",
                 "Amphenicols", "Lincosamides",
                 "Second/ Third generation cephalosporins", "Macrolides",
                 "Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal", "Fluroquinolones",
                 "Carbapenems", "Glycopeptides"),
        aware = c(rep("Access", time = 8), rep("Watch", time = 6))
      )
      
      
      # Antibiotic class
      df_class <- df_numeric %>%
        select(Penicillins:last_col()) %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1)) %>%
        left_join(df_aware, by = "text")
      
      
      # Access antibiotic by antibiotic class
      plot_access_class <- df_class %>%
        filter(aware == "Access") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram( color="black", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_minimal() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      plot_access_class <- ggplotly(plot_access_class)
      
      # Plot 3
      # Watch antibiotic by antibiotic class
      plot_watch_class <- df_class %>%
        filter(aware == "Watch") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="black", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_minimal() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      plot_watch_class <- ggplotly(plot_watch_class)
      
      # Plot 4 ------
      
      # Percent of overall use 
      percent_table_syndrome <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) 
      
      
      # Wide to long format 
      percent_table_syndrome_long <- as.data.frame(percent_table_syndrome) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "syndrome",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        )
      
      
      # Cleaning and formatting 
      percent_atb_syndrome <- percent_table_syndrome_long %>%
        mutate(
          AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
          syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
        ) %>%
        mutate(syndrome = case_when(
          syndrome == "CAP" ~ "Community acquired pneumonia",
          syndrome == "HAP" ~ "Hospital acquired pneumonia (non-VAP)",
          syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
          syndrome == "SST" ~ "Skin and soft-tissue infection",
          syndrome == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ syndrome # Keep other values unchanged
        ))
      
      
      
      # 1. Compute summary statistics by group
      
      percent_atb_syndrome <- percent_atb_syndrome %>%
        group_by(syndrome, AWaRe_category) %>%
        summarise(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup()
      
      
      # 2. Create the bar graph with error bars
      
      # Create an ordering for syndrome based on "Access" median_value
      syndrome_order <- percent_atb_syndrome %>%
        filter(AWaRe_category == "Access") %>%
        arrange(desc(median_value)) %>%
        pull(syndrome)
      
      # Reorder the syndrome factor in the main dataset
      percent_atb_syndrome <- percent_atb_syndrome %>%
        mutate(syndrome = factor(syndrome, levels = syndrome_order))
      
      
      # Create a ggplot2 bar plot with error bars
      aware_syndrome_plot <- percent_atb_syndrome %>%
        ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
          "Syndrome: ", syndrome, "<br>",
          "AWaRe Category: ", AWaRe_category, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.20,
                      size = 0.25)  +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() +
        labs(title = "Expected Antibiotic Use by Infection Syndrome",
             x = "",
             y = "Percentage of expected total use for all infection syndromes",
             fill = "AWaRe Category",
             caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(axis.text.x = element_text(),
              plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
        )
      
      # Convert ggplot to plotly and customize hover template
      aware_syndrome_plot <- ggplotly(aware_syndrome_plot, tooltip = "text") %>%
        layout(hovermode = "closest")
      
      aware_syndrome_plot <- ggplotly(aware_syndrome_plot)
      
      # Plot 5 ------
      # Percent for CAP  
      percent_aware_cap <- (df_numeric[,9:10]/ (df_numeric[,9] + df_numeric[,10]) * 100) 
      
      # Percent for HAP (non-VAP)
      percent_aware_hap <- (df_numeric[,11:12]/ (df_numeric[,11] + df_numeric[,12]) * 100) 
      
      # Percent for Bacterial meningitis 
      percent_aware_bm <- (df_numeric[,13:14]/ (df_numeric[,13] + df_numeric[,14]) * 100) 
      
      # Percent for intra-abdominal infection 
      percent_aware_ia <- (df_numeric[,15:16]/ (df_numeric[,15] + df_numeric[,16]) * 100) 
      
      # Percent for acute pyelonephritis
      percent_aware_pye <- (df_numeric[,17:18]/ (df_numeric[,17] + df_numeric[,18]) * 100) 
      
      # Percent for skin and soft-tissue infection 
      percent_aware_sst <- (df_numeric[,19:20]/ (df_numeric[,19] + df_numeric[,20]) * 100) 
      
      # Percent for bone and joint infection
      percent_aware_bj <- (df_numeric[,21:22]/ (df_numeric[,21] + df_numeric[,22]) * 100) 
      
      # Percent for C. difficile infection 
      percent_aware_cdf <- (df_numeric[,23:24]/ (df_numeric[,23] + df_numeric[,24]) * 100) 
      
      # Percent for febrile neutropenia
      percent_aware_fn <- (df_numeric[,25:26]/ (df_numeric[,25] + df_numeric[,26]) * 100) 
      
      # Percent for sepsis  
      percent_aware_sepsis <- (df_numeric[,27:28]/ (df_numeric[,27] + df_numeric[,28]) * 100) 
      
      # Percent for surgical prophylaxis 
      percent_aware_sp <- (df_numeric[,29:30]/ (df_numeric[,29] + df_numeric[,30]) * 100) 
      
      
      percent_aware_syndrome <- cbind(percent_aware_cap, percent_aware_hap, percent_aware_bm, percent_aware_ia,
                                      percent_aware_pye, percent_aware_sst, percent_aware_bj, percent_aware_cdf,
                                      percent_aware_fn, percent_aware_sepsis, percent_aware_sp)
      
      
      
      # Wide to long format 
      percent_aware_syndrome_long <- as.data.frame(percent_aware_syndrome) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "syndrome",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        )
      
      
      # Cleaning and formatting 
      percent_aware_syndrome <- percent_aware_syndrome_long %>%
        mutate(
          AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
          syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
        ) %>%
        mutate(syndrome = case_when(
          syndrome == "CAP" ~ "Community acquired pneumonia",
          syndrome == "HAP" ~ "Hospital acquired pneumonia (non-VAP)",
          syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
          syndrome == "SST" ~ "Skin and soft-tissue infection",
          syndrome == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ syndrome # Keep other values unchanged
        ))
      
      
      
      # 1. Compute summary statistics by group
      
      percent_aware_syndrome <- percent_aware_syndrome %>%
        group_by(syndrome, AWaRe_category) %>%
        summarise(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup()
      
      
      # 2. Create the bar graph with error bars
      
      
      
      # Create an ordering for syndrome based on "Access" median_value
      syndrome_order <- percent_aware_syndrome %>%
        filter(AWaRe_category == "Access") %>%
        arrange(desc(median_value)) %>%
        pull(syndrome)
      
      # Reorder the syndrome factor in the main dataset
      percent_aware_syndrome <- percent_aware_syndrome %>%
        mutate(syndrome = factor(syndrome, levels = syndrome_order))
      
      # Then plot using the reordered factor levels
      # Then plot using the reordered factor levels
      aware_ind_syndrome_plot <- percent_aware_syndrome %>%
        ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
          "Syndrome: ", syndrome, "<br>",
          "AWaRe Category: ", AWaRe_category, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.20,
                      size = 0.25) +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() + 
        labs(title = "Expected AWaRe Antibiotic Use by Infection Syndrome",
             x = "",
             y = "Percentage of expected total use for each infection syndrome",
             fill = "AWaRe Category",
             caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(axis.text.x = element_text(),
              plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      # Convert ggplot to plotly and customize hover template
      aware_ind_syndrome_plot <- ggplotly(aware_ind_syndrome_plot, tooltip = "text") %>%
        layout(hovermode = "closest")
      
      # plot 6 ------
      # Percentage "Access" out of total use 
      access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100)
      
      # Wide to long format 
      percent_access_long <- as.data.frame(access_df) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "atb_class",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) %>%
        mutate(aware = "Access")
      
      
      # Percentage "Watch" out of total use 
      watch_df <- (df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100
      
      # Wide to long format 
      percent_watch_long <- as.data.frame(watch_df) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "atb_class",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) %>%
        mutate(aware = "Watch")
      
      # Append Access and Watch 
      percent_both <- rbind(percent_access_long, percent_watch_long)
      
      
      # 1. Compute summary statistics by group
      
      percent_aware_atb <- percent_both %>%
        group_by(atb_class) %>%
        mutate(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        select(atb_class, aware, median_value, lower, upper) %>%
        distinct()
      
      
      # 2. Create the bar graph with error bars
      
      aware_atbclass_plot <- percent_aware_atb %>%
        ggplot(aes(x = fct_reorder(atb_class, median_value), y = median_value, fill = aware, text = paste(
          "Antibiotic Class: ", atb_class, "<br>",
          "AWaRe Category: ", aware, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL),stat = "identity", 
                 position = position_dodge(width = 0.8), 
                 color = "black",
                 width = 0.8) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.2,
                      size = 0.25) +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() + 
        labs(
          title = "Expected Antibiotic Use by Antibiotic Class",
          x = "Antibiotic Class",
          y = "Percentage of expected total use for all infection syndromes",
          fill = "AWaRe Category",
          caption = "Note: The bar represents median value and the error bar represents 95% credible intervals."
        ) +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(
          axis.text.x = element_text(),
          plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
        )
      
      aware_atbclass_plot <- ggplotly(aware_atbclass_plot, tooltip = "text") %>%
        layout(hovermode = "closest")
      
    }else if (input$choices_ac == "both"){
      #### both adult ####
      # Create an empty dataframe to store model's output
      result_adult <- data.frame()
      
      # Get the input_adult_df values (reactive values)
      # Run the model 1000 times
      for (i in 1:1000) {
        set.seed(Sys.time() + i)
        incProgress(0.5/1000)
        # set.seed(Sys.time())
        # SS
        model_input <- generate_input_dataframe(input_big()$adult_cases,
                                                input_big()$adult_para_data, input_big()$std_err)
        
        temp <- as.data.frame(t(model_A(model_input)))
        result_adult <- rbind(result_adult, temp)
      }
      
      # Convert all columns to numeric
      df_numeric <- result_adult %>%
        mutate_all(~ as.numeric(as.character(.)))
      
      # Summary tables----
      # Table 1
      summary_table_overall <- df_numeric[, 1:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median}",  # Display median
          missing = "no",
          digits = all_continuous() ~ 1  # Ensure 1 decimal place for median
        ) %>%
        modify_header(label="Description", stat_0 = "Expected_usage") %>%
        modify_footnote(all_stat_cols() ~ "Median (95% credible intervals), DDD = defined daily dose, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
        modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_tibble()
      
      # Calculate quantiles and add them to the summary table
      quantiles_overall <- df_numeric[, 1:8] %>%
        summarise(across(everything(), list(
          `0.025 Quantile` = ~ quantile(., probs = 0.025),
          `0.975 Quantile` = ~ quantile(., probs = 0.975)
        ))) %>%
        pivot_longer(cols = everything(), names_to = c("Description", ".value"), names_sep = "_") %>%
        mutate(across(where(is.numeric), ~ sprintf("%.1f", .)))
      
      # Combine the median and quantiles into a single column
      summary_table_overall_adult <- summary_table_overall %>%
        left_join(quantiles_overall, by = "Description") %>%
        mutate(`Expected_usage (95% CI)` = paste0(Expected_usage, " (", `0.025 Quantile`, ", ", `0.975 Quantile`, ")")) %>%
        select(Description,`Expected_usage (95% CI)`)
      
      summary_table_overall_percent_access_watch_adult  <- df_numeric[, 7:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median}% ({p25}%, {p75}%)",
          missing = "no",
          digits = all_continuous() ~ 1
        )%>%
        as_tibble()
      
      value_summary$antibiotic_access_adult <- summary_table_overall_percent_access_watch_adult[1,2]
      value_summary$antibiotic_watch_adult <- summary_table_overall_percent_access_watch_adult[2,2]
      
      # Table 2 ------
      summary_table_syndrome_long <- as.data.frame(df_numeric[,9:(9+21)]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_syndrome_stat <- summary_table_syndrome_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      percent_table_syndrome_long <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_table_syndrome <- percent_table_syndrome_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      
      # # Combine the median and quantiles into a single column
      # summary_table_combined <- percent_table_syndrome %>%
      #   left_join(quantiles_overall, by = "Description") %>%
      #   mutate(`Expected_usage (95% CI)` = paste0(Expected_usage, " (", `0.025 Quantile`, ", ", `0.975 Quantile`, ")")) %>%
      #   select(Description, `Expected_usage (95% CI)`)
      
      
      # Store as dataframe
      percent_table_syndrome_df <- percent_table_syndrome %>%
        as_tibble()
      
      # Combine DDD and percent use 
      summary_table_syndrome_ddd_percent <- left_join(summary_table_syndrome_stat, percent_table_syndrome_df, by = "Description")
      
      
      summary_table_syndrome_ddd_percent_adult <- summary_table_syndrome_ddd_percent %>%
        mutate(
          AWaRe_category = str_extract(Description, "Access|Watch"), # Extract "Access" or "Watch"
          Infection_syndromes = str_remove(Description, ".*: ") # Remove everything before ": "
        ) %>%
        select(AWaRe_category, Infection_syndromes, expected_use.x, expected_use.y) %>% # Reorder columns
        mutate(Infection_syndromes = case_when(
          Infection_syndromes == "CAP" ~ "Community acquired pneumonia",
          Infection_syndromes == "HAP(non-VAP)" ~ "Hospital acquired pneumonia (non-VAP)",
          Infection_syndromes == "Upper UTI" ~ "Community acquired pyelonephritis",
          Infection_syndromes == "SST" ~ "Skin and soft-tissue infection",
          Infection_syndromes == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ Infection_syndromes # Keep other values unchanged
        )) %>%
        rename(`AWaRe category`      = AWaRe_category,
               `Infection syndromes` = Infection_syndromes,
               `Expected use (DDD)` = expected_use.x,
               `Expected use (% of total use)` = expected_use.y)
      
      
      # Table 3 -------
      summary_table_class_access_long <- as.data.frame(df_numeric[, 31:38]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_class_access <- summary_table_class_access_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      # summary_table_class_access <- df_numeric[, 31:38] %>%
      #   tbl_summary(
      #     by = NULL,
      #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      #     missing = "no",
      #     digits = all_continuous() ~ 0
      #   ) %>%
      #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (DOT)") %>%
      #   modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
      #   modify_caption("**Table 3: Expected empirical Access antibiotic usage by antibiotic class**")
      # 
      
      # Store as a dataframe
      summary_table_class_access <- summary_table_class_access %>%
        as_tibble()
      
      summary_table_class_access <- summary_table_class_access %>%
        mutate(`AWaRe category` = "Access")
      
      
      # Percentage "Access" out of total use 
      # expected use in percentage of total 
      access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_access_df <- access_df %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      # access_df <- (df_numeric[, 31:38]/ df_numeric[, 1])*100
      # 
      # percent_access_df <- access_df %>%
      #   tbl_summary(
      #     by = NULL,
      #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      #     missing = "no",
      #     digits = all_continuous() ~ 1 
      #   ) %>%
      #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (% of total use)") %>%
      #   modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = Days of Therapy") %>%
      #   modify_caption("**Table 3: Expected (Access) antibiotic usage by antibiotic class**") 
      
      
      
      # Store as a dataframe
      percent_access_df <- percent_access_df %>%
        as_tibble()
      
      
      summary_access_ddd_percent <- left_join(summary_table_class_access,percent_access_df, by = "Description" ) %>%
        select(`AWaRe category`, everything())
      
      
      # DOT "Watch" out of total use 
      summary_table_class_Watch_long <- as.data.frame(df_numeric[, 39:ncol(df_numeric)]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_class_Watch <- summary_table_class_Watch_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      # summary_table_class_watch <- df_numeric[, 39:ncol(df_numeric)] %>%
      #   tbl_summary(
      #     by = NULL,
      #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      #     missing = "no",
      #     digits = all_continuous() ~ 0
      #   ) %>%
      #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (DOT)") %>%
      #   modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
      #   modify_caption("**Table 4: Expected empirical Watch antibiotic usage by antibiotic class**") 
      # 
      
      
      # Store as a dataframe
      summary_table_class_watch <- summary_table_class_Watch %>%
        as_tibble()
      
      
      summary_table_class_watch <- summary_table_class_watch %>%
        mutate(`AWaRe category` = "Watch")
      
      
      # Percentage "Watch" out of total use 
      # expected use in percentage of total 
      watch_df <- ((df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_watch_df <- watch_df %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      # watch_df <- (df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100
      # 
      # 
      # percent_watch_df <- watch_df %>%
      #   tbl_summary(
      #     by = NULL,
      #     statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      #     missing = "no",
      #     digits = all_continuous() ~ 1 
      #   ) %>%
      #   modify_header(label="Antibiotic class", stat_0 = "Expected usage (% of total use)") %>%
      #   modify_footnote(all_stat_cols() ~ "Median (IQR), DTT = Days of Therapy") %>%
      #   modify_caption("**Table 3: Expected (Watch) antibiotic usage by antibiotic class**") 
      
      
      
      
      # Store as a dataframe
      percent_watch_df <- percent_watch_df %>%
        as_tibble()
      
      # Merge  Watch DDD and percent 
      summary_watch_ddd_percent <- left_join(summary_table_class_watch,percent_watch_df, by = "Description" ) %>%
        select(`AWaRe category`, everything())
      
      
      # Append Access and Watch 
      atb_class_summary_tbl_adult <- rbind(summary_access_ddd_percent, summary_watch_ddd_percent)%>%
        rename(`AWaRe category`      = `AWaRe category`,
               `Antibiotic class` = Description,
               `Expected use (DDD)` = expected_use.x,
               `Expected use (% of total use)` = expected_use.y)
      value_summary$antibiotic_class_access_adult <- atb_class_summary_tbl_adult[atb_class_summary_tbl_adult$`AWaRe category`=="Access",][1,c(2,4)]
      value_summary$antibiotic_class_watch_adult <- atb_class_summary_tbl_adult[atb_class_summary_tbl_adult$`AWaRe category`=="Watch",][1,c(2,4)]
      
      #Plotting----
      # Change the dataformat for the plot
      df_plot <- df_numeric %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1))
      
      
      # Distribution of "Access" and "Watch" antibiotic usage out of total
      
      # Plot 1
      # combined plot antibiotic
      combined_plot_adult  <- df_plot %>%
        filter(text %in% c("Access antibiotics(%)", "Watch antibiotics(%)")) %>%
        ggplot(aes(x = value, fill = text)) +
        geom_histogram(color = "black", alpha = 0.6, position = "identity", binwidth = 1) +
        scale_fill_manual(
          values = c("Access antibiotics(%)" = "#009E73",  # green
                     "Watch antibiotics(%)"  = "#F0E442"), # yellow
          labels = c("Access antibiotics", "Watch antibiotics")  # Custom legend labels
        ) +
        theme_minimal() +
        labs(title = "Distribution of Expected Antibiotic Usage by AWaRe classification",
             x = "Percentage of Overall Usage", 
             y = "Frequency",
             fill = "AWaRe category") +
        scale_x_continuous(labels = scales::percent_format(scale = 1)) +
        theme(legend.position = "right",
              axis.text  = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10))
      
      combined_plot_adult  <- ggplotly(combined_plot_adult)
      # AWaRe group by antibiotic class
      
      df_aware <- data.frame(
        text = c("Penicillins", "Beta-lactam antibiotics plus enzyme inhibitor",
                 "First generation cephlosporins", "Aminoglycosides",
                 "Nitroimidazoles", "Tetracyclines",
                 "Amphenicols", "Lincosamides",
                 "Second/ Third generation cephalosporins", "Macrolides",
                 "Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal", "Fluroquinolones",
                 "Carbapenems", "Glycopeptides"),
        aware = c(rep("Access", time = 8), rep("Watch", time = 6))
      )
      
      
      # Antibiotic class
      df_class <- df_numeric %>%
        select(Penicillins:last_col()) %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1)) %>%
        left_join(df_aware, by = "text")
      
      # Plot 2
      # Access antibiotic by antibiotic class
      plot_access_class_adult <- df_class %>%
        filter(aware == "Access") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram( color="black", alpha=0.5, position = 'identity', binwidth = 2) +
        scale_fill_viridis(discrete=TRUE) +
        theme_minimal() +
        labs(title = "Distribution of Expected Access Antibiotic Usage",
             x = "Expected usage (DDD)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      plot_access_class_adult <- ggplotly(plot_access_class_adult)
      
      # Plot 3
      # Watch antibiotic by antibiotic class
      plot_watch_class_adult <- df_class %>%
        filter(aware == "Watch") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="black", alpha=0.5, position = 'identity', binwidth = 2) +
        scale_fill_viridis(discrete=TRUE) +
        theme_minimal() +
        labs(title = "Distribution of Expected Watch Antibiotic Usage",
             x = "Expected usage (DDD)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      plot_watch_class_adult <- ggplotly(plot_watch_class_adult)
      
      # Plot 4 ------
      
      # Percent of overall use 
      percent_table_syndrome <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) 
      
      
      # Wide to long format 
      percent_table_syndrome_long <- as.data.frame(percent_table_syndrome) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "syndrome",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        )
      
      
      # Cleaning and formatting 
      percent_atb_syndrome <- percent_table_syndrome_long %>%
        mutate(
          AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
          syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
        ) %>%
        mutate(syndrome = case_when(
          syndrome == "CAP" ~ "Community acquired pneumonia",
          syndrome == "HAP(non-VAP)" ~ "Hospital acquired pneumonia (non-VAP)",
          syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
          syndrome == "SST" ~ "Skin and soft-tissue infection",
          syndrome == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ syndrome # Keep other values unchanged
        ))
      
      
      
      # 1. Compute summary statistics by group
      
      percent_atb_syndrome <- percent_atb_syndrome %>%
        group_by(syndrome, AWaRe_category) %>%
        summarise(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup()
      
      
      # 2. Create the bar graph with error bars
      
      # Create an ordering for syndrome based on "Access" median_value
      syndrome_order <- percent_atb_syndrome %>%
        filter(AWaRe_category == "Access") %>%
        arrange(desc(median_value)) %>%
        pull(syndrome)
      
      # Reorder the syndrome factor in the main dataset
      percent_atb_syndrome <- percent_atb_syndrome %>%
        mutate(syndrome = factor(syndrome, levels = syndrome_order))
      
      
      aware_syndrome_plot_adult <- percent_atb_syndrome %>%
        ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
          "Syndrome: ", syndrome, "<br>",
          "AWaRe Category: ", AWaRe_category, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.20,
                      size = 0.25)  +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() +
        labs(title = "Expected Antibiotic Use by Infection Syndrome",
             x = "",
             y = "Percentage of expected total use for all infection syndromes",
             fill = "AWaRe Category",
             caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(axis.text.x = element_text(),
              plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
        )
      
      # Convert ggplot to plotly and customize hover template
      aware_syndrome_plot_adult <- ggplotly(aware_syndrome_plot_adult, tooltip = "text") %>%
        layout(hovermode = "closest")
      
      # Plot 5 ------
      # Percent for CAP  
      percent_aware_cap <- (df_numeric[,9:10]/ (df_numeric[,9] + df_numeric[,10]) * 100) 
      
      # Percent for HAP (non-VAP)
      percent_aware_hap <- (df_numeric[,11:12]/ (df_numeric[,11] + df_numeric[,12]) * 100) 
      
      # Percent for Bacterial meningitis 
      percent_aware_bm <- (df_numeric[,13:14]/ (df_numeric[,13] + df_numeric[,14]) * 100) 
      
      # Percent for intra-abdominal infection 
      percent_aware_ia <- (df_numeric[,15:16]/ (df_numeric[,15] + df_numeric[,16]) * 100) 
      
      # Percent for acute pyelonephritis
      percent_aware_pye <- (df_numeric[,17:18]/ (df_numeric[,17] + df_numeric[,18]) * 100) 
      
      # Percent for skin and soft-tissue infection 
      percent_aware_sst <- (df_numeric[,19:20]/ (df_numeric[,19] + df_numeric[,20]) * 100) 
      
      # Percent for bone and joint infection
      percent_aware_bj <- (df_numeric[,21:22]/ (df_numeric[,21] + df_numeric[,22]) * 100) 
      
      # Percent for C. difficile infection 
      percent_aware_cdf <- (df_numeric[,23:24]/ (df_numeric[,23] + df_numeric[,24]) * 100) 
      
      # Percent for febrile neutropenia
      percent_aware_fn <- (df_numeric[,25:26]/ (df_numeric[,25] + df_numeric[,26]) * 100) 
      
      # Percent for sepsis  
      percent_aware_sepsis <- (df_numeric[,27:28]/ (df_numeric[,27] + df_numeric[,28]) * 100) 
      
      # Percent for surgical prophylaxis 
      percent_aware_sp <- (df_numeric[,29:30]/ (df_numeric[,29] + df_numeric[,30]) * 100) 
      
      
      percent_aware_syndrome <- cbind(percent_aware_cap, percent_aware_hap, percent_aware_bm, percent_aware_ia,
                                      percent_aware_pye, percent_aware_sst, percent_aware_bj, percent_aware_cdf,
                                      percent_aware_fn, percent_aware_sepsis, percent_aware_sp)
      
      
      
      
      
      # Wide to long format 
      percent_aware_syndrome_long <- as.data.frame(percent_aware_syndrome) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "syndrome",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        )
      
      
      # Cleaning and formatting 
      percent_aware_syndrome <- percent_aware_syndrome_long %>%
        mutate(
          AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
          syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
        ) %>%
        mutate(syndrome = case_when(
          syndrome == "CAP" ~ "Community acquired pneumonia",
          syndrome == "HAP(non-VAP)" ~ "Hospital acquired pneumonia (non-VAP)",
          syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
          syndrome == "SST" ~ "Skin and soft-tissue infection",
          syndrome == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ syndrome # Keep other values unchanged
        ))
      
      
      
      # 1. Compute summary statistics by group
      
      percent_aware_syndrome <- percent_aware_syndrome %>%
        group_by(syndrome, AWaRe_category) %>%
        summarise(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup()
      
      
      # 2. Create the bar graph with error bars
      
      
      
      # Create an ordering for syndrome based on "Access" median_value
      syndrome_order <- percent_aware_syndrome %>%
        filter(AWaRe_category == "Access") %>%
        arrange(desc(median_value)) %>%
        pull(syndrome)
      
      # Reorder the syndrome factor in the main dataset
      percent_aware_syndrome <- percent_aware_syndrome %>%
        mutate(syndrome = factor(syndrome, levels = syndrome_order))
      
      # Then plot using the reordered factor levels
      aware_ind_syndrome_plot <- percent_aware_syndrome %>%
        ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
          "Syndrome: ", syndrome, "<br>",
          "AWaRe Category: ", AWaRe_category, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.20,
                      size = 0.25) +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() + 
        labs(title = "Expected AWaRe Antibiotic Use by Infection Syndrome",
             x = "",
             y = "Percentage of expected total use for each infection syndrome",
             fill = "AWaRe Category",
             caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(axis.text.x = element_text(),
              plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      # Convert ggplot to plotly and customize hover template
      aware_ind_syndrome_plot_adult <- ggplotly(aware_ind_syndrome_plot, tooltip = "text") %>%
        layout(hovermode = "closest")
      
      # plot 6 ------
      # Percentage "Access" out of total use 
      access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100)
      
      # Wide to long format 
      percent_access_long <- as.data.frame(access_df) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "atb_class",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) %>%
        mutate(aware = "Access")
      
      
      # Percentage "Watch" out of total use 
      watch_df <- (df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100
      
      # Wide to long format 
      percent_watch_long <- as.data.frame(watch_df) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "atb_class",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) %>%
        mutate(aware = "Watch")
      
      # Append Access and Watch 
      
      
      percent_both <- rbind(percent_access_long, percent_watch_long)
      
      
      # 1. Compute summary statistics by group
      
      percent_aware_atb <- percent_both %>%
        group_by(atb_class) %>%
        mutate(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        select(atb_class, aware, median_value, lower, upper) %>%
        distinct()
      
      # 2. Create the bar graph with error bars
      aware_atbclass_plot <- percent_aware_atb %>%
        ggplot(aes(x = fct_reorder(atb_class, median_value), y = median_value, fill = aware, text = paste(
          "Antibiotic Class: ", atb_class, "<br>",
          "AWaRe Category: ", aware, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL),stat = "identity", 
                 position = position_dodge(width = 0.8), 
                 color = "black",
                 width = 0.8) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.2,
                      size = 0.25) +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() + 
        labs(
          title = "Expected Antibiotic Use by Antibiotic Class",
          x = "Antibiotic Class",
          y = "Percentage of expected total use for all infection syndromes",
          fill = "AWaRe Category",
          caption = "Note: The bar represents median value and the error bar represents 95% credible intervals."
        ) +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(
          axis.text.x = element_text(),
          plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
        )
      
      aware_atbclass_plot_adult <- ggplotly(aware_atbclass_plot, tooltip = "text") %>%
        layout(hovermode = "closest")
      
      #### both child ####
      # Create an empty dataframe to store model's output
      result_child <- data.frame()
      
      # Get the input_adult_df values (reactive values)
      
      # Run the model 1000 times
      for (i in 1:1000) {
        set.seed(1000+i)
        incProgress(0.5/1000)
        # set.seed(Sys.time())
        # SS
        
        model_input <- generate_input_dataframe_child(input_big()$child_cases,
                                                      input_big()$child_para_data, input_big()$std_err)
        
        temp <- as.data.frame(t(model_C(model_input)))
        result_child <- rbind(result_child, temp)
      }
      
      # Convert all columns to numeric
      df_numeric <- result_child %>%
        mutate_all(~ as.numeric(as.character(.)))
      
      # Summary tables----
      # Table 1
      summary_table_overall <- df_numeric[, 1:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median}",  # Display median
          missing = "no",
          digits = all_continuous() ~ 1  # Ensure 1 decimal place for median
        ) %>%
        modify_header(label="Description", stat_0 = "Expected_usage") %>%
        modify_footnote(all_stat_cols() ~ "Median (95% credible intervals), DDD = defined daily dose, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
        modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_tibble()
      
      # Calculate quantiles and add them to the summary table
      quantiles_overall <- df_numeric[, 1:8] %>%
        summarise(across(everything(), list(
          `0.025 Quantile` = ~ quantile(., probs = 0.025),
          `0.975 Quantile` = ~ quantile(., probs = 0.975)
        ))) %>%
        pivot_longer(cols = everything(), names_to = c("Description", ".value"), names_sep = "_") %>%
        mutate(across(where(is.numeric), ~ sprintf("%.1f", .)))
      
      # Combine the median and quantiles into a single column
      summary_table_overall_child <- summary_table_overall %>%
        left_join(quantiles_overall, by = "Description") %>%
        mutate(`Expected_usage (DOT)` = paste0(Expected_usage, " (", `0.025 Quantile`, ", ", `0.975 Quantile`, ")")) %>%
        select(Description, `Expected_usage (DOT)`)
      
      summary_table_overall_percent_access_watch_child <- df_numeric[, 7:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median}% ({p25}%, {p75}%)",
          missing = "no",
          digits = all_continuous() ~ 1
        )%>%
        as_tibble()
      
      value_summary$antibiotic_access_child <- summary_table_overall_percent_access_watch_child[1,2]
      value_summary$antibiotic_watch_child <- summary_table_overall_percent_access_watch_child[2,2]
      
      # Table 2 ------
      # Expected use (DOT) for each syndrome 
      
      summary_table_syndrome_long <- as.data.frame(df_numeric[,9:(9+21)]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_syndrome_stat <- summary_table_syndrome_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      percent_table_syndrome_long <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_table_syndrome <- percent_table_syndrome_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      
      # Store as dataframe
      percent_table_syndrome_df <- percent_table_syndrome %>%
        as_tibble()
      
      # Combine DDD and percent use 
      summary_table_syndrome_dot_percent <- left_join(summary_table_syndrome_stat, percent_table_syndrome_df, by = "Description")
      
      # Data table preparation
      summary_table_syndrome_dot_percent_child <- summary_table_syndrome_dot_percent %>%
        mutate(
          AWaRe_category = str_extract(Description, "Access|Watch"), # Extract "Access" or "Watch"
          Infection_syndromes = str_remove(Description, ".*: ") # Remove everything before ": "
        ) %>%
        select(AWaRe_category, Infection_syndromes, expected_use.x, expected_use.y) %>% # Reorder columns
        mutate(Infection_syndromes = case_when(
          Infection_syndromes == "CAP" ~ "Community acquired pneumonia",
          Infection_syndromes == "HAP" ~ "Hospital acquired pneumonia (non-VAP)",
          Infection_syndromes == "Upper UTI" ~ "Community acquired pyelonephritis",
          Infection_syndromes == "SST" ~ "Skin and soft-tissue infection",
          Infection_syndromes == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ Infection_syndromes # Keep other values unchanged
        )) %>%
        rename(`AWaRe category`      = AWaRe_category,
               `Infection syndromes` = Infection_syndromes,
               `Expected use (DOT)` = expected_use.x,
               `Expected use (% of total use)` = expected_use.y)
      
      
      
      # Table 3 -------
      summary_table_class_access_long <- as.data.frame(df_numeric[, 31:38]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_class_access <- summary_table_class_access_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      summary_table_class_access <- summary_table_class_access %>%
        as_tibble()
      
      summary_table_class_access <- summary_table_class_access %>%
        mutate(`AWaRe category` = "Access")
      
      
      # Percentage "Access" out of total use 
      # expected use in percentage of total 
      access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_access_df <- access_df %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      percent_access_df <- percent_access_df %>%
        as_tibble()
      
      
      summary_access_ddd_percent <- left_join(summary_table_class_access,percent_access_df, by = "Description" ) %>%
        select(`AWaRe category`, everything())
      
      
      # DOT "Watch" out of total use 
      summary_table_class_Watch_long <- as.data.frame(df_numeric[, 39:ncol(df_numeric)]) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) 
      
      # Compute summary statistics by syndrome 
      
      summary_table_class_Watch <- summary_table_class_Watch_long %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(expected_use, na.rm = TRUE), 1),
          lower = round(quantile(expected_use, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(expected_use, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(expected_use = paste0(median_value, " (", lower, ", ", upper, ")")) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      summary_table_class_watch <- summary_table_class_Watch %>%
        as_tibble()
      
      
      summary_table_class_watch <- summary_table_class_watch %>%
        mutate(`AWaRe category` = "Watch")
      
      
      # Percentage "Watch" out of total use 
      # expected use in percentage of total 
      watch_df <- ((df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "Description",         # create a column for the original column names
          values_to = "Expected use (% of total use)"     # create a column for the values
        ) 
      
      # expected use in percentage of total 
      percent_watch_df <- watch_df %>%
        group_by(Description) %>%
        mutate(
          median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
          lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
          upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        select(Description, median_value, lower, upper) %>%
        distinct() %>%
        mutate(
          median_value = paste0(median_value, "%"),
          lower = paste0(lower, "%"),
          upper = paste0(upper, "%"),
          expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
        ) %>%
        select(Description, expected_use)
      
      # Store as a dataframe
      percent_watch_df <- percent_watch_df %>%
        as_tibble()
      
      # Merge  Watch DDD and percent 
      summary_watch_ddd_percent <- left_join(summary_table_class_watch,percent_watch_df, by = "Description" ) %>%
        select(`AWaRe category`, everything())
      
      
      # Append Access and Watch 
      atb_class_summary_tbl_child <- rbind(summary_access_ddd_percent, summary_watch_ddd_percent)%>%
        rename(`AWaRe category`      = `AWaRe category`,
               `Antibiotic class` = Description,
               `Expected use (DOT)` = expected_use.x,
               `Expected use (% of total use)` = expected_use.y)
      
      value_summary$antibiotic_class_access_child <- atb_class_summary_tbl_child[atb_class_summary_tbl_child$`AWaRe category`=="Access",][1,c(2,4)]
      value_summary$antibiotic_class_watch_child <- atb_class_summary_tbl_child[atb_class_summary_tbl_child$`AWaRe category`=="Watch",][1,c(2,4)]
      
      
      #Plotting----
      # Change the dataformat for the plot
      df_plot <- df_numeric %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1))
      
      
      # Distribution of "Access" and "Watch" antibiotic usage out of total
      
      # Plot 1
      # combined plot antibiotic
      combined_plot_child <- df_plot %>%
        filter(text %in% c("Access antibiotics(%)", "Watch antibiotics(%)")) %>%
        ggplot(aes(x = value, fill = text)) +
        geom_histogram(color = "black", alpha = 0.6, position = "identity", binwidth = 1) +
        scale_fill_manual(
          values = c("Access antibiotics(%)" = "#009E73",  # green
                     "Watch antibiotics(%)"  = "#F0E442"), # yellow
          labels = c("Access antibiotics", "Watch antibiotics")  # Custom legend labels
        ) +
        theme_minimal() +
        labs(title = "Distribution of Expected Antibiotic Usage by AWaRe classification",
             x = "Percentage of Overall Usage", 
             y = "Frequency",
             fill = "AWaRe category") +
        scale_x_continuous(labels = scales::percent_format(scale = 1)) +
        theme(legend.position = "right",
              axis.text  = element_text(size = 10),
              axis.title = element_text(size = 10),
              plot.title = element_text(size = 10))
      
      combined_plot_child <- ggplotly(combined_plot_child)
      
      # AWaRe group by antibiotic class
      
      df_aware <- data.frame(
        text = c("Penicillins", "Beta-lactam antibiotics plus enzyme inhibitor",
                 "First generation cephlosporins", "Aminoglycosides",
                 "Nitroimidazoles", "Sulfonamides",
                 "Amphenicols", "Lincosamides",
                 "Second/ Third generation cephalosporins", "Macrolides",
                 "Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal", "Fluroquinolones",
                 "Carbapenems", "Glycopeptides"),
        aware = c(rep("Access", time = 8), rep("Watch", time = 6))
      )
      
      
      # Antibiotic class
      df_class <- df_numeric %>%
        select(Penicillins:last_col()) %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1)) %>%
        left_join(df_aware, by = "text")
      
      
      # Access antibiotic by antibiotic class
      plot_access_class <- df_class %>%
        filter(aware == "Access") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram( color="black", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_minimal() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      plot_access_class_child <- ggplotly(plot_access_class)
      
      # Plot 3
      # Watch antibiotic by antibiotic class
      plot_watch_class <- df_class %>%
        filter(aware == "Watch") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="black", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_minimal() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      plot_watch_class_child <- ggplotly(plot_watch_class)
      
      # Plot 4 ------
      
      # Percent of overall use 
      percent_table_syndrome <- ((df_numeric[,9:(9+21)]/ df_numeric[, 1]) * 100) 
      
      
      # Wide to long format 
      percent_table_syndrome_long <- as.data.frame(percent_table_syndrome) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "syndrome",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        )
      
      
      # Cleaning and formatting 
      percent_atb_syndrome <- percent_table_syndrome_long %>%
        mutate(
          AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
          syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
        ) %>%
        mutate(syndrome = case_when(
          syndrome == "CAP" ~ "Community acquired pneumonia",
          syndrome == "HAP" ~ "Hospital acquired pneumonia (non-VAP)",
          syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
          syndrome == "SST" ~ "Skin and soft-tissue infection",
          syndrome == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ syndrome # Keep other values unchanged
        ))
      
      
      
      # 1. Compute summary statistics by group
      
      percent_atb_syndrome <- percent_atb_syndrome %>%
        group_by(syndrome, AWaRe_category) %>%
        summarise(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup()
      
      
      # 2. Create the bar graph with error bars
      
      # Create an ordering for syndrome based on "Access" median_value
      syndrome_order <- percent_atb_syndrome %>%
        filter(AWaRe_category == "Access") %>%
        arrange(desc(median_value)) %>%
        pull(syndrome)
      
      # Reorder the syndrome factor in the main dataset
      aware_syndrome_plot_child <- percent_atb_syndrome %>%
        ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
          "Syndrome: ", syndrome, "<br>",
          "AWaRe Category: ", AWaRe_category, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.20,
                      size = 0.25)  +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() +
        labs(title = "Expected Antibiotic Use by Infection Syndrome",
             x = "",
             y = "Percentage of expected total use for all infection syndromes",
             fill = "AWaRe Category",
             caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(axis.text.x = element_text(),
              plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
        )
      
      # Convert ggplot to plotly and customize hover template
      aware_syndrome_plot_child <- ggplotly(aware_syndrome_plot_child, tooltip = "text") %>%
        layout(hovermode = "closest")
      
      aware_syndrome_plot_child <- ggplotly(aware_syndrome_plot_child)
      
      # Plot 5 ------
      # Percent for CAP  
      percent_aware_cap <- (df_numeric[,9:10]/ (df_numeric[,9] + df_numeric[,10]) * 100) 
      
      # Percent for HAP (non-VAP)
      percent_aware_hap <- (df_numeric[,11:12]/ (df_numeric[,11] + df_numeric[,12]) * 100) 
      
      # Percent for Bacterial meningitis 
      percent_aware_bm <- (df_numeric[,13:14]/ (df_numeric[,13] + df_numeric[,14]) * 100) 
      
      # Percent for intra-abdominal infection 
      percent_aware_ia <- (df_numeric[,15:16]/ (df_numeric[,15] + df_numeric[,16]) * 100) 
      
      # Percent for acute pyelonephritis
      percent_aware_pye <- (df_numeric[,17:18]/ (df_numeric[,17] + df_numeric[,18]) * 100) 
      
      # Percent for skin and soft-tissue infection 
      percent_aware_sst <- (df_numeric[,19:20]/ (df_numeric[,19] + df_numeric[,20]) * 100) 
      
      # Percent for bone and joint infection
      percent_aware_bj <- (df_numeric[,21:22]/ (df_numeric[,21] + df_numeric[,22]) * 100) 
      
      # Percent for C. difficile infection 
      percent_aware_cdf <- (df_numeric[,23:24]/ (df_numeric[,23] + df_numeric[,24]) * 100) 
      
      # Percent for febrile neutropenia
      percent_aware_fn <- (df_numeric[,25:26]/ (df_numeric[,25] + df_numeric[,26]) * 100) 
      
      # Percent for sepsis  
      percent_aware_sepsis <- (df_numeric[,27:28]/ (df_numeric[,27] + df_numeric[,28]) * 100) 
      
      # Percent for surgical prophylaxis 
      percent_aware_sp <- (df_numeric[,29:30]/ (df_numeric[,29] + df_numeric[,30]) * 100) 
      
      
      percent_aware_syndrome <- cbind(percent_aware_cap, percent_aware_hap, percent_aware_bm, percent_aware_ia,
                                      percent_aware_pye, percent_aware_sst, percent_aware_bj, percent_aware_cdf,
                                      percent_aware_fn, percent_aware_sepsis, percent_aware_sp)
      
      
      
      # Wide to long format 
      percent_aware_syndrome_long <- as.data.frame(percent_aware_syndrome) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "syndrome",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        )
      
      
      # Cleaning and formatting 
      percent_aware_syndrome <- percent_aware_syndrome_long %>%
        mutate(
          AWaRe_category = str_extract(syndrome, "Access|Watch"), # Extract "Access" or "Watch"
          syndrome = str_remove(syndrome, ".*: ") # Remove everything before ": "
        ) %>%
        mutate(syndrome = case_when(
          syndrome == "CAP" ~ "Community acquired pneumonia",
          syndrome == "HAP" ~ "Hospital acquired pneumonia (non-VAP)",
          syndrome == "Upper UTI" ~ "Community acquired pyelonephritis",
          syndrome == "SST" ~ "Skin and soft-tissue infection",
          syndrome == "Sepsis" ~ "Sepsis and septic shock",
          TRUE ~ syndrome # Keep other values unchanged
        ))
      
      
      
      # 1. Compute summary statistics by group
      
      percent_aware_syndrome <- percent_aware_syndrome %>%
        group_by(syndrome, AWaRe_category) %>%
        summarise(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup()
      
      
      # 2. Create the bar graph with error bars
      
      
      
      # Create an ordering for syndrome based on "Access" median_value
      syndrome_order <- percent_aware_syndrome %>%
        filter(AWaRe_category == "Access") %>%
        arrange(desc(median_value)) %>%
        pull(syndrome)
      
      # Reorder the syndrome factor in the main dataset
      percent_aware_syndrome <- percent_aware_syndrome %>%
        mutate(syndrome = factor(syndrome, levels = syndrome_order))
      
      # Then plot using the reordered factor levels
      aware_ind_syndrome_plot <- percent_aware_syndrome %>%
        ggplot(aes(x = syndrome, y = median_value, fill = AWaRe_category, text = paste(
          "Syndrome: ", syndrome, "<br>",
          "AWaRe Category: ", AWaRe_category, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL), stat = "identity", position = position_dodge(width = 0.8), color = "black") +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.20,
                      size = 0.25) +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() + 
        labs(title = "Expected AWaRe Antibiotic Use by Infection Syndrome",
             x = "",
             y = "Percentage of expected total use for each infection syndrome",
             fill = "AWaRe Category",
             caption = "Note: The bar represents median value and the error bar represents 95% credible intervals.") +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(axis.text.x = element_text(),
              plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10)))
      
      # Convert ggplot to plotly and customize hover template
      aware_ind_syndrome_plot_child <- ggplotly(aware_ind_syndrome_plot, tooltip = "text") %>%
        layout(hovermode = "closest")

      # plot 6 ------
      # Percentage "Access" out of total use 
      access_df <- ((df_numeric[, 31:38]/ df_numeric[, 1])*100)
      
      # Wide to long format 
      percent_access_long <- as.data.frame(access_df) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "atb_class",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) %>%
        mutate(aware = "Access")
      
      
      # Percentage "Watch" out of total use 
      watch_df <- (df_numeric[, 39:ncol(df_numeric)]/ df_numeric[, 1]) * 100
      
      # Wide to long format 
      percent_watch_long <- as.data.frame(watch_df) %>%
        pivot_longer(
          cols = everything(),         # pivot all columns
          names_to = "atb_class",         # create a column for the original column names
          values_to = "expected_use"     # create a column for the values
        ) %>%
        mutate(aware = "Watch")
      
      # Append Access and Watch 
      percent_both <- rbind(percent_access_long, percent_watch_long)
      
      
      # 1. Compute summary statistics by group
      
      percent_aware_atb <- percent_both %>%
        group_by(atb_class) %>%
        mutate(
          median_value = median(expected_use, na.rm = TRUE),
          lower = quantile(expected_use, 0.025, na.rm = TRUE),
          upper = quantile(expected_use, 0.975, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        select(atb_class, aware, median_value, lower, upper) %>%
        distinct()
      
      
      # 2. Create the bar graph with error bars
      
      aware_atbclass_plot <- percent_aware_atb %>%
        ggplot(aes(x = fct_reorder(atb_class, median_value), y = median_value, fill = aware, text = paste(
          "Antibiotic Class: ", atb_class, "<br>",
          "AWaRe Category: ", aware, "<br>",
          "Median: ", round(median_value, 1), "<br>",
          "Lower CI: ", round(lower, 1), "<br>",
          "Upper CI: ", round(upper, 1)
        ))) +
        geom_bar(aes(text = NULL),stat = "identity", 
                 position = position_dodge(width = 0.8), 
                 color = "black",
                 width = 0.8) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.8),
                      width = 0.2,
                      size = 0.25) +
        scale_fill_manual(
          values = c("Access" = "#009E73",    # Green for Access
                     "Watch"  = "#F0E442"),   # Yellow for Watch
          labels = c("Access" = "Access antibiotics", 
                     "Watch"  = "Watch antibiotics")
        ) +
        coord_flip() + 
        labs(
          title = "Expected Antibiotic Use by Antibiotic Class",
          x = "Antibiotic Class",
          y = "Percentage of expected total use for all infection syndromes",
          fill = "AWaRe Category",
          caption = "Note: The bar represents median value and the error bar represents 95% credible intervals."
        ) +
        theme_minimal(base_size = 10) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme(
          axis.text.x = element_text(),
          plot.caption = element_text(size = 8, hjust = 0, face = "italic", margin = margin(t = 10))
        )
      
      aware_atbclass_plot_child <- ggplotly(aware_atbclass_plot, tooltip = "text") %>%
        layout(hovermode = "closest")
      
      }
    # shinyjs::enable("run_model")
    # shinyjs::enable("Summary_model")
    if(input$choices_ac != "both"){
    # Render the summary table to UI
    output$summary_table_overall <- renderDT({
      if(input$choices_ac == "adult"){
        datatable(
          summary_table_overall,
          options = list(
            paging = TRUE,           # paginate the output
            pageLength = 12,         # number of rows per page
            scrollY = TRUE,          # enable scrolling on Y axis
            columnDefs = list(list(targets = 1, className = 'dt-center')) # center the second column
          ),
          extensions = 'Buttons',
          selection = 'single',      # enable selection of a single row
          filter = 'top',            # include column filters at the top
          rownames = FALSE,
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
            "This table shows the overall expected empirical antibiotic usage expressed as median (95% credible intervals). DDD = defined daily dose."
          )
        )
      }else if(input$choices_ac == "child"){
        datatable(
          summary_table_overall,
          options = list(
            paging = TRUE,           # paginate the output
            pageLength = 12,         # number of rows per page
            scrollY = TRUE,          # enable scrolling on Y axis
            columnDefs = list(list(targets = 1, className = 'dt-center')) # center the second column
          ),
          extensions = 'Buttons',
          selection = 'single',      # enable selection of a single row
          filter = 'top',            # include column filters at the top
          rownames = FALSE,
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
            "This table shows the overall expected empirical antibiotic usage expressed as median (95% credible intervals). DOT = Days of Therapy."
          )
        )
      }

    })
    
    output$summary_table_syndrome <- renderDT({
      if(input$choices_ac == "adult"){
        summary_table_syndrome_ddd_percent %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows expected empirical antibiotic usage, stratified by AWaRe category, for each infection syndrome expressed in median (95% credible intervals).DDD = Defined daily dose."
            )
          )
      }else if(input$choices_ac == "child"){
        summary_table_syndrome_dot_percent %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows expected empirical antibiotic usage, stratified by AWaRe category, for each infection syndrome expressed in median (95% credible intervals). DOT = Days of Therapy."
            )
          )
      }
      
      
      
    })
    
    output$summary_table_class <- renderDT({
      # Summary table in HTML format 
      if(input$choices_ac == "adult"){
        atb_class_summary_tbl %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows the expected empirical antibiotic usage, stratified by AWaRe category, for each antibiotic class expressed as median (95% credible intervals)."
            )
          )
      }else if(input$choices_ac == "child"){
        atb_class_summary_tbl %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows the expected empirical antibiotic usage, stratified by AWaRe category, for each antibiotic class expressed as median (95% credible intervals). DOT = Days of Therapy."
            )
          )
      }
      
    })
    

    output$combined_plot <- renderPlotly({
      combined_plot
    })
    
    output$plot_access_class <- renderPlotly({
      plot_access_class
    })
    
    output$plot_watch_class <- renderPlotly({
      plot_watch_class
    })
    
    output$aware_syndrome_plot <- renderPlotly({
      aware_syndrome_plot
    })
    
    output$aware_ind_syndrome_plot	 <- renderPlotly({
      aware_ind_syndrome_plot	
    })
    
    output$aware_atbclass_plot <- renderPlotly({
      aware_atbclass_plot
    })
    }else{
      # Render the summary table to UI both ####
      ##### adult#####
      
      output$summary_table_overall_adult <- renderDT({
          datatable(
            summary_table_overall_adult,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = 1, className = 'dt-center')) # center the second column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows the overall expected empirical antibiotic usage expressed as median (95% credible intervals). DDD = defined daily dose."
            )
          )
        
      })
      
      output$summary_table_syndrome_adult <- renderDT({
        summary_table_syndrome_ddd_percent_adult %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows expected empirical antibiotic usage, stratified by AWaRe category, for each infection syndrome expressed in median (95% credible intervals).DDD = Defined daily dose."
            )
          )
      })
      
      output$summary_table_class_adult <- renderDT({
        # Summary table in HTML format 
        
        atb_class_summary_tbl_adult %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows the expected empirical antibiotic usage, stratified by AWaRe category, for each antibiotic class expressed as median (95% credible intervals)."
            )
          )
        
      })
      
      
      
      output$combined_plot_adult <- renderPlotly({
        combined_plot_adult
      })
      
      output$plot_access_class_adult <- renderPlotly({
        plot_access_class_adult
      })
      
      output$plot_watch_class_adult <- renderPlotly({
        plot_watch_class_adult
      })
      
      output$aware_syndrome_plot_adult <- renderPlotly({
        aware_syndrome_plot_adult
      })
      
      output$aware_ind_syndrome_plot_adult	 <- renderPlotly({
        aware_ind_syndrome_plot_adult	
      })
      
      output$aware_atbclass_plot_adult <- renderPlotly({
        aware_atbclass_plot_adult
      })
      #####child #####
      output$summary_table_overall_child <- renderDT({
        datatable(
          summary_table_overall_child,
          options = list(
            paging = TRUE,           # paginate the output
            pageLength = 12,         # number of rows per page
            scrollY = TRUE,          # enable scrolling on Y axis
            columnDefs = list(list(targets = 1, className = 'dt-center')) # center the second column
          ),
          extensions = 'Buttons',
          selection = 'single',      # enable selection of a single row
          filter = 'top',            # include column filters at the top
          rownames = FALSE,
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
            "This table shows the overall expected empirical antibiotic usage expressed as median (95% credible intervals). DOT = Days of Therapy."
          )
        )
        
      })
      
      output$summary_table_syndrome_child <- renderDT({
        summary_table_syndrome_dot_percent_child %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows expected empirical antibiotic usage, stratified by AWaRe category, for each infection syndrome expressed in median (95% credible intervals). DOT = Days of Therapy."
            )
          )
      })
      
      output$summary_table_class_child <- renderDT({
        # Summary table in HTML format 
        
        atb_class_summary_tbl_child %>%
          formattable(
            list(
              `AWaRe category` = formatter("span", 
                                           style = x ~ ifelse(x == "Access", 
                                                              "color: green; font-weight: bold;", 
                                                              ifelse(x == "Watch", 
                                                                     "color: orange; font-weight: bold;", 
                                                                     "")))
            )
          ) %>%
          as.datatable(
            escape = FALSE,
            options = list(
              paging = TRUE,           # paginate the output
              pageLength = 12,         # number of rows per page
              scrollY = TRUE,          # enable scrolling on Y axis
              columnDefs = list(list(targets = c(2,3), className = 'dt-center')) # center the third and fourth column
            ),
            extensions = 'Buttons',
            selection = 'single',      # enable selection of a single row
            filter = 'top',            # include column filters at the top
            rownames = FALSE,
            caption = tags$caption(
              style = 'caption-side: bottom; text-align: left; font-style: italic; padding: 5px;',
              "This table shows the expected empirical antibiotic usage, stratified by AWaRe category, for each antibiotic class expressed as median (95% credible intervals). DOT = Days of Therapy."
            )
          )
        
      })
      
      
      
      output$combined_plot_child <- renderPlotly({
        combined_plot_child
      })
      
      output$plot_access_class_child <- renderPlotly({
        plot_access_class_child
      })
      
      output$plot_watch_class_child <- renderPlotly({
        plot_watch_class_child
      })
      
      output$aware_syndrome_plot_child <- renderPlotly({
        aware_syndrome_plot_child
      })
      
      output$aware_ind_syndrome_plot_child	 <- renderPlotly({
        aware_ind_syndrome_plot_child
      })
      
      output$aware_atbclass_plot_child <- renderPlotly({
        aware_atbclass_plot_child
      })
    }
    value_fin$finished <-1
    enable("Summary_model")
    enable("Visualization")
    show("Summary_bsButton")
    shinyjs::enable("run_model")
    shinyjs::enable("Summary_model")
    updateButton(session, "Summary_model",
                 icon = icon("chart-bar"))
    })
  })

  # Display the summary of inputs in a table in the "Summary Inputs" tab
  

  output$adult_input_table <- renderTable({
      req(!is.null(input_big()$adult_cases))
      setNames(input_big()$adult_cases[,c("syndrome2","cases")],c("Syndrome","Cases"))
    })
  
  output$child_input_table <- renderTable({
      req(!is.null(input_big()$child_cases))
      setNames(input_big()$child_cases[,c("syndrome2","cases")],c("Syndrome","Cases"))
    })
  
  output$summary_inputs_ui <- renderUI({
    
    if(input$choices_ac == "adult"){
      req(!is.null(input_big()$adult_cases))
      tableOutput("adult_input_table")
    }else if(input$choices_ac == "child"){
      req(!is.null(input_big()$child_cases))
      tableOutput("child_input_table")
    }else{
      req(!is.null(input_big()$adult_cases) & !is.null(input_big()$child_cases))
      tabsetPanel(
        tabPanel("Adult",
                 tableOutput("adult_input_table")
                 ),
        tabPanel("Child",
                 tableOutput("child_input_table")
                 )
      )
    }
  })
  
  
  # output$first_choice_table <- renderTable({
  #   first_choice_list
  # })
  
  output$adult_input_table2 <- renderTable({
    req(!is.null(input_big()$adult_para_data))
    setNames(input_big()$adult_para_data[,c("parameter2","value")],c("Parameter","Value"))
  })

  output$child_input_table2 <- renderTable({
    req(!is.null(input_big()$child_para_data))
    setNames(input_big()$child_para_data[,c("parameter2","value")],c("Parameter","Value"))
  })
  
  output$summary_inputs_ui2 <- renderUI({
    
    if(input$choices_ac == "adult"){
      req(!is.null(input_big()$adult_cases))
      tableOutput("adult_input_table2")
    }else if(input$choices_ac == "child"){
      req(!is.null(input_big()$child_cases))
      tableOutput("child_input_table2")
    }else{
      req(!is.null(input_big()$adult_cases) & !is.null(input_big()$child_cases))
      tabsetPanel(
        tabPanel("Adult",
                 tableOutput("adult_input_table2")
        ),
        tabPanel("Child",
                 tableOutput("child_input_table2")
        )
      )
    }
  })
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent(input$Summary_input, {
    show("Summary_input_table")
    hide("Summary_model_table")
    hide("Visualization_plot")
  })
  observeEvent(input$Summary_model, {
    show("Summary_model_table")
    hide("Summary_input_table")
    hide("Visualization_plot")
  })
  observeEvent(input$Visualization, {
    show("Visualization_plot")
    hide("Summary_model_table")
    hide("Summary_input_table")
  })
  
  observeEvent(input$load_params, {
    req(input$load_params)
    if(input$choices_ac != "both"){
    params <- read.csv(input$load_params$datapath)
    params_vec <- c(params[,2])
    names(params_vec) <- params[,1]

    # Load parameters into the input fields
    load_disease_inputs(params_vec,input$choices_ac, session)  # Load inputs function
    }else{
      params <- read.csv(input$load_params$datapath)
      params_vec <- params[,2]
      names(params_vec) <- params[,1]
      
      # Find the index of "END"
      end_idx <- which(names(params_vec) == "END_ADULT")

      if(length(end_idx) == 0){
        showModal(modalDialog(
          title = "Warning",
          html("An error occurred: CSV format isn't correct"),
          easyClose = TRUE,
          footer = modalButton("OK"),
          size = "m"  # Medium-sized modal
        ))
        return()
      }
      
      # Separate into two groups
      params_adult <- params_vec[1:(end_idx - 1)]
      params_child <- params_vec[(end_idx + 1):length(params_vec)]
      # print(params_adult)
      # print(params_child)
      # Load parameters into the input fields
      load_disease_inputs_adult(params_adult,input$choices_ac, session)  # Load inputs function
      load_disease_inputs_child(params_child,input$choices_ac, session)  # Load inputs function
    }
  })
  
  # UI - OUTCOME  -----------------------------------------------------
  
  output$summary_output <- renderUI({
    if(input$choices_ac != "both"){
    box(width =12,collapsible = T,
      title = "Empirical Usage",
      navset_card_underline(
        tabPanel(title = HTML("<b>Tables</b>"),
          tabsetPanel(
                tabPanel(title = HTML("<b>Table 1: Overall expected empirical antibiotic usage</b>"),
                DTOutput("summary_table_overall"),),
                tabPanel(title = HTML("<b>Table 2: Expected empirical usage by syndrome</b>"),
                DTOutput("summary_table_syndrome")),
                tabPanel(title = HTML("<b>Table 3: Expected empirical access/watch usage by antibiotic classes</b>"),
                DTOutput("summary_table_class"))
          )
              ),
        tabPanel(title = HTML("<b>Figures</b>"),
            box(width = 12,collapsible = T,
                title = HTML("<b> Expected empirical usage </b>"),
                navset_card_underline(
                  tabPanel(title = tagList(HTML("<b>Plot 1: Expected empirical usage by antibiotic classes</b>"),
                                           bsButton("plot_Figure1", icon("question-circle"), style = "default"),
                                           bsTooltip("plot_Figure1", HTML("<b>Figure 1. </b>The bar graph shows the distribution of expected percentages of “Access” and “Watch” antibiotics out of the total. The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular percentage is."),
                                                     trigger = "focus",
                                                     placement="right", options = list(container = "body") )
                  ),
                  plotlyOutput("combined_plot",height = "100%"),
                  p(strong("Figure 1.")," The bar graph shows the distribution of expected percentages of “Access” and “Watch” antibiotics out of the total. The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular percentage is.")
                  )
             )
            ),
            box(width = 12,collapsible = T,
                title = HTML("<b> Expected empirical Access/Watch Antibiotic Usage</b>"),
                navset_card_underline(
                  tabPanel(title = tagList(
                    HTML("<b>Plot 2: Expected empirical Access Antibiotic Usage</b>"),
                    bsButton("plot_Figure2", icon("question-circle"), style = "default"),
                    bsTooltip("plot_Figure2", HTML("<b>Figure 2. </b>The bar graph shows the distribution of expected empirical use of “Access” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is."),
                              trigger = "focus",
                              placement="right", options = list(container = "body") )
                  ),
                  plotlyOutput("plot_access_class",height = "100%"),
                  p(strong("Figure 2.")," The bar graph shows the distribution of expected empirical use of “Access” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is.")
                  ),
                  tabPanel(title = tagList(HTML("<b>Plot 3: Expected empirical Watch Antibiotic Usage</b>"),
                                           bsButton("plot_Figure3", icon("question-circle"), style = "default"),
                                           bsTooltip("plot_Figure3", HTML("<b>Figure 3. </b>The bar graph shows the distribution of expected empirical use of “Watch” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is."),
                                                     trigger = "focus",
                                                     placement="right", options = list(container = "body") )                               
                  ),
                  plotlyOutput("plot_watch_class",height = "100%"),
                  p(strong("Figure 3.")," The bar graph shows the distribution of expected empirical use of “Watch” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is. ")
                  )
                )
            ),
            box(width = 12,collapsible = T,
                title = HTML("<b> Expected AWaRe Antibiotic </b>"),
                navset_card_underline(
                  tabPanel(title = HTML("<b>Plot 4: Expected AWaRe Antibiotic Use by Infection Syndrome</b>"),
                           bsButton("plot_Figure4", icon("question-circle"), style = "default"),
                           bsTooltip("plot_Figure4", HTML("<b>Figure 4. </b>The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The values represent the proportion of total antibiotic use across all infection syndromes. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations."),
                                     trigger = "focus",
                                     placement="right", options = list(container = "body") ),
                           plotlyOutput("aware_syndrome_plot",height = "100%"),
                           p(strong("Figure 4.")," The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The values represent the proportion of total antibiotic use across all infection syndromes. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations.")
                  ),
                  tabPanel(title = tagList(HTML("<b>Plot 5: Expected AWaRe Antibiotic Use by Infection Syndrome</b>"),
                                           bsButton("plot_Figure5", icon("question-circle"), style = "default"),
                                           bsTooltip("plot_Figure5", HTML("<b>Figure 5. </b>The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations."),
                                                     trigger = "focus",
                                                     placement="right", options = list(container = "body") ) 
                  ),
                  plotlyOutput("aware_ind_syndrome_plot",height = "100%"),
                  p(strong("Figure 5.")," The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations.")
                  ),
                  tabPanel(title = tagList(HTML("<b>Plot 6: Expected Antibiotic Use by Antibiotic Class</b>"),
                                           bsButton("plot_Figure6", icon("question-circle"), style = "default"),
                                           bsTooltip("plot_Figure6", HTML("<b>Figure 6. </b>The bar graph shows the expected percentage use of each antibiotic class. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations."),
                                                     trigger = "focus",
                                                     placement="right", options = list(container = "body") ) 
                  ),
                  plotlyOutput("aware_atbclass_plot",height = "100%"),
                  p(strong("Figure 6.")," The bar graph shows the expected percentage use of each antibiotic class. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations.")
                  )
                )
            )
        ),
        tabPanel(title = HTML("<b>Summary</b>"),
                 box(width = 12,collapsible = T,
                     title = HTML("<b> Expected AWaRe Antibiotic </b>"),
                     htmlOutput("summary_guidelines_text")
                 )
        )
      )
    )
    }else{
      box(width =12,collapsible = T,
             title = "Empirical Usage Tables",
          navset_card_underline(
            tabPanel(title = HTML("<b>Table</b>"),
            tabsetPanel(
             tabPanel(title = HTML("<b>Table 1: Overall expected empirical antibiotic usage</b>"),
                      tabsetPanel(
                        tabPanel("Adult",DTOutput("summary_table_overall_adult")),
                        tabPanel("Child",DTOutput("summary_table_overall_child")),
                      )
                      ),
             tabPanel(title = HTML("<b>Table 2: Expected empirical usage by syndrome</b>"),
                      tabsetPanel(
                        tabPanel("Adult",DTOutput("summary_table_syndrome_adult")),
                        tabPanel("Child",DTOutput("summary_table_syndrome_child")),
                      )
                      ),
             tabPanel(title = HTML("<b>Table 3: Expected empirical access usage by antibiotic classes</b>"),
                      tabsetPanel(
                        tabPanel("Adult",DTOutput("summary_table_class_adult")),
                        tabPanel("Child",DTOutput("summary_table_class_child")),
                      )
                      )
            )
            ),
            tabPanel(title = HTML("<b>Figures</b>"),
                     box(width = 12,collapsible = T,
                         title =HTML("<b> Expected empirical usage </b>"),
                         navset_card_underline(
                           tabPanel(title =tagList(HTML("<b>Plot 1: Expected empirical usage by antibiotic classes</b>"),
                                                   bsButton("plot_Figure1_both", icon("question-circle"), style = "default"),
                                                   bsTooltip("plot_Figure1_both", "Figure 1. The bar graph shows the distribution of expected percentages of “Access” and “Watch” antibiotics out of the total. The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular percentage is.",
                                                             trigger = "focus",
                                                             placement="right", options = list(container = "body") )
                           ),
                           tabsetPanel(
                             tabPanel("Adult",plotlyOutput("combined_plot_adult",height = "100%")),
                             tabPanel("Child",plotlyOutput("combined_plot_child",height = "100%")),
                           ),
                           p(strong("Figure 1.")," The bar graph shows the distribution of expected percentages of “Access” and “Watch” antibiotics out of the total. The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular percentage is.")
                           ),
                         )
                     ),
                     box(width = 12,collapsible = T,
                         title = HTML("<b> Expected empirical Access/Watch Antibiotic Usage</b>"),
                         navset_card_underline(
                           tabPanel(title = tagList(
                             HTML("<b>Plot 2: Expected empirical Access Antibiotic Usage</b>"),
                             bsButton("plot_Figure2_both", icon("question-circle"), style = "default"),
                             bsTooltip("plot_Figure2_both", HTML("<b>Figure 2. </b>The bar graph shows the distribution of expected empirical use of “Access” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is."),
                                       trigger = "focus",
                                       placement="right", options = list(container = "body") )
                           ),
                           tabsetPanel(
                             tabPanel("Adult",plotlyOutput("plot_access_class_adult",height = "100%")),
                             tabPanel("Child",plotlyOutput("plot_access_class_child",height = "100%")),
                           ),
                           p(strong("Figure 2.")," The bar graph shows the distribution of expected empirical use of “Access” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is.")
                           ),
                           tabPanel(title = tagList(HTML("<b>Plot 3: Expected empirical Watch Antibiotic Usage</b>"),
                                                    bsButton("plot_Figure3_both", icon("question-circle"), style = "default"),
                                                    bsTooltip("plot_Figure3_both", HTML("<b>Figure 3. </b>The bar graph shows the distribution of expected empirical use of “Watch” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is."),
                                                              trigger = "focus",
                                                              placement="right", options = list(container = "body") )                               
                           ),
                           tabsetPanel(
                             tabPanel("Adult",plotlyOutput("plot_watch_class_adult",height = "100%")),
                             tabPanel("Child",plotlyOutput("plot_watch_class_child",height = "100%")),
                           ),
                           p(strong("Figure 3.")," The bar graph shows the distribution of expected empirical use of “Watch” antibiotics, measured in defined daily doses (DDD). The distribution is derived from 1,000 model iterations. The taller the bar, the more likely that particular usage value is. ")
                           )
                         )
                     ),
                     box(width = 12,collapsible = T,
                         title = HTML("<b> Expected AWaRe Antibiotic </b>"),
                         navset_card_underline(
                           tabPanel(title = HTML("<b>Plot 4: Expected AWaRe Antibiotic Use by Infection Syndrome</b>"),
                                    bsButton("plot_Figure4_both", icon("question-circle"), style = "default"),
                                    bsTooltip("plot_Figure4_both", HTML("<b>Figure 4. </b>The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The values represent the proportion of total antibiotic use across all infection syndromes. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations."),
                                              trigger = "focus",
                                              placement="right", options = list(container = "body") ) ,
                                    tabsetPanel(
                                      tabPanel("Adult",plotlyOutput("aware_syndrome_plot_adult",height = "100%")),
                                      tabPanel("Child",plotlyOutput("aware_syndrome_plot_child",height = "100%")),
                                    ),
                                    p(strong("Figure 4.")," The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The values represent the proportion of total antibiotic use across all infection syndromes. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations.")
                           ),
                           tabPanel(title = tagList(HTML("<b>Plot 5: Expected AWaRe Antibiotic Use by Infection Syndrome</b>"),
                                                    bsButton("plot_Figure5_both", icon("question-circle"), style = "default"),
                                                    bsTooltip("plot_Figure5_both", HTML("<b>Figure 5. </b>The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations."),
                                                              trigger = "focus",
                                                              placement="right", options = list(container = "body") ) 
                           ),
                           tabsetPanel(
                             tabPanel("Adult", plotlyOutput("aware_ind_syndrome_plot_adult",height = "100%")),
                             tabPanel("Child",plotlyOutput("aware_ind_syndrome_plot_child",height = "100%")),
                           ),
                           p(strong("Figure 5.")," The bar graph shows the expected percentages of “Access” and “Watch” antibiotics for each infection syndrome. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations.")
                           ),
                           tabPanel(title = tagList(HTML("<b>Plot 6: Expected Antibiotic Use by Antibiotic Class</b>"),
                                                    bsButton("plot_Figure6_both", icon("question-circle"), style = "default"),
                                                    bsTooltip("plot_Figure6_both", HTML("<b>Figure 6. </b>The bar graph shows the expected percentage use of each antibiotic class. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations."),
                                                              trigger = "focus",
                                                              placement="right", options = list(container = "body") ) 
                           ),
                           tabsetPanel(
                             tabPanel("Adult", plotlyOutput("aware_atbclass_plot_adult",height = "100%")),
                             tabPanel("Child",plotlyOutput("aware_atbclass_plot_child",height = "100%")),
                           ),
                           p(strong("Figure 6.")," The bar graph shows the expected percentage use of each antibiotic class. The lines on the bars represent 95% credible intervals derived from 1,000 model iterations.")
                           )
                         )
                     )
                     ),
            tabPanel(title = HTML("<b>Summary</b>"),
                     box(width = 12,collapsible = T,
                         title = HTML("<b> Expected AWaRe Antibiotic </b>"),
                         tabsetPanel(
                           tabPanel("Adult",htmlOutput("summary_guidelines_text_adult")),
                           tabPanel("Child",htmlOutput("summary_guidelines_text_child")),
                         )
                     )
            )
      )
      )
    }
  })

  output$type_severity_cases <- renderUI({
    tagList(
      sliderInput("cap_severe", tags$h5("Proportion of severe CAP cases"), min = 0, max = 1, value = 0.4, step = 0.01),
      sliderInput("abd_severe", tags$h5("Proportion of severe intra-abdominal infection cases"), min = 0, max = 1, value = 0.45, step = 0.01),
      sliderInput("uti_severe", tags$h5("Proportion of severe upper UTI cases"), min = 0, max = 1, value = 0.35, step = 0.01),
      sliderInput("cdf_severe", tags$h5("Proportion of severe C. difficile infection cases"), min = 0, max = 1, value = 0.25, step = 0.01),
      sliderInput("sst_nf", tags$h5("Proportion of necrotizing fasciitis cases in SST patients"), min = 0, max = 1, value = 0.04, step = 0.01)
    )
  })
  
  # Popover ####
  #### cap_severe #####
  addPopover(session,"cap_severe_adult",includeHTML("www/popover_text/cap_severe_adult.html"),
             placement = "bottom", trigger = "focus", options = NULL)
  
  addPopover(session,"cap_severe_single_adult",
             includeHTML("www/popover_text/cap_severe_adult.html"), 
             placement = "bottom", trigger = "focus", options = NULL)

  addPopover(session,"cap_severe_child",
             includeHTML("www/popover_text/cap_severe_child.html"), 
             placement = "bottom", trigger = "focus", options = NULL)
  
  addPopover(session,"cap_severe_single_child",
             includeHTML("www/popover_text/cap_severe_child.html"),
             placement = "bottom", trigger = "focus", options = NULL)
  
  ####abd_severe #####
  addPopover(session,"abd_severe",
             includeHTML("www/popover_text/abd_severe.html"),
             placement = "right", trigger = "focus", options = NULL)
  
  addPopover(session,"abd_severe_adult",
             includeHTML("www/popover_text/abd_severe.html"),
             placement = "right", trigger = "focus", options = NULL)
  
  addPopover(session,"abd_severe_child",
             includeHTML("www/popover_text/abd_severe.html"),
             placement = "right", trigger = "focus", options = NULL)
  
  #### uti_severe #####
  addPopover(session,"uti_severe",
             HTML("<p>The default value (0.16) is based on a study that reviewed clinical outcome and risk factors for mortality in patients with acute pyelonephritis admitted to a hospital in Hong Kong.
             (DOI:<a class='link-pop' target='_blank' href='https://doi.org/10.12809/hkmj134061'>10.12809/hkmj134061</a>)</p>
<p><strong>Severity classification (</strong>Harrison textbook of internal medicine, 20<sup>th</sup> edition)</p>
<p><strong>Mild case:</strong>&nbsp; low-grade fever with or without lower-back pain or costovertebral-angle pain.</p>
<p><strong>Severe case:</strong> high fever, rigors, nausea, vomiting, and flank and / or loin pain.</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  
  addPopover(session,"uti_severe_adult",
             HTML("<p>The default value (0.16) is based on a study that reviewed clinical outcome and risk factors for mortality in patients with acute pyelonephritis admitted to a hospital in Hong Kong.
             (DOI:<a class='link-pop' target='_blank' href='https://doi.org/10.12809/hkmj134061'>10.12809/hkmj134061</a>)</p>
<p><strong>Severity classification (</strong>Harrison textbook of internal medicine, 20<sup>th</sup> edition)</p>
<p><strong>Mild case:</strong>&nbsp; low-grade fever with or without lower-back pain or costovertebral-angle pain.</p>
<p><strong>Severe case:</strong> high fever, rigors, nausea, vomiting, and flank and / or loin pain.</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  
  addPopover(session,"uti_severe_child",
             HTML("<p>The default value (0.16) is based on a study that reviewed clinical outcome and risk factors for mortality in patients with acute pyelonephritis admitted to a hospital in Hong Kong.
             (DOI:<a class='link-pop' target='_blank' href='https://doi.org/10.12809/hkmj134061'>10.12809/hkmj134061</a>)</p>
<p><strong>Severity classification (</strong>Harrison textbook of internal medicine, 20<sup>th</sup> edition)</p>
<p><strong>Mild case:</strong>&nbsp; low-grade fever with or without lower-back pain or costovertebral-angle pain.</p>
<p><strong>Severe case:</strong> high fever, rigors, nausea, vomiting, and flank and / or loin pain.</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  
  #### cdf_severe #####
  addPopover(session,"cdf_severe",
             includeHTML("www/popover_text/cdf_severe.html"),
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"cdf_severe_adult",
             includeHTML("www/popover_text/cdf_severe.html"),
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"cdf_severe_child",
             includeHTML("www/popover_text/cdf_severe.html"),
             placement = "right", trigger = "focus", options = NULL)
  
  #### sst_nf #####
  addPopover(session,"sst_nf",
             HTML("<p>NF is an uncommon infection although not rare medical and surgical emergency. 
                  The estimated proportion of NF among SSTI patients is less than 0.05. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1016/S1473-3099(22)00583-7'>https://doi.org/10.1016/S1473-3099(22)00583-7</a>)</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"sst_nf_adult",
             HTML("<p>NF is an uncommon infection although not rare medical and surgical emergency. 
                  The estimated proportion of NF among SSTI patients is less than 0.05. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1016/S1473-3099(22)00583-7'>https://doi.org/10.1016/S1473-3099(22)00583-7</a>)</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"sst_nf_child",
             HTML("<p>NF is an uncommon infection although not rare medical and surgical emergency. 
                  The estimated proportion of NF among SSTI patients is less than 0.05. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1016/S1473-3099(22)00583-7'>https://doi.org/10.1016/S1473-3099(22)00583-7</a>)</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  
  #### hap_mdr ####
  addPopover(session,"hap_mdr",
             HTML("<p>The default value (0.37) is based on a 10-year prospective observational study on clinical and microbiological characteristics of Adolescents and adults with hospital-acquired pneumonia in China. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1007/s10096-020-04046-9'>https://doi.org/10.1007/s10096-020-04046-9</a> )</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"hap_mdr_adult",
             HTML("<p>The default value (0.37) is based on a 10-year prospective observational study on clinical and microbiological characteristics of Adolescents and adults with hospital-acquired pneumonia in China. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1007/s10096-020-04046-9'>https://doi.org/10.1007/s10096-020-04046-9</a> )</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"hap_mdr_child",
             HTML("<p>The default value (0.37) is based on a 10-year prospective observational study on clinical and microbiological characteristics of Adolescents and adults with hospital-acquired pneumonia in China. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1007/s10096-020-04046-9'>https://doi.org/10.1007/s10096-020-04046-9</a> )</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  
  #### strep_pyogenes ####
  addPopover(session,"strep_pyogenes",
             HTML("<p>Group A Streptococcus is implicated in ~ 60% of cases of necrotizing fasciitis. 
                  (Ref: Harrison&rsquo;s Principles of Internal Medicine, 20<sup>th</sup> Edition)</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"strep_pyogenes_adult",
             HTML("<p>Group A Streptococcus is implicated in ~ 60% of cases of necrotizing fasciitis. 
                  (Ref: Harrison&rsquo;s Principles of Internal Medicine, 20<sup>th</sup> Edition)</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  addPopover(session,"strep_pyogenes_child",
             HTML("<p>Group A Streptococcus is implicated in ~ 60% of cases of necrotizing fasciitis. 
                  (Ref: Harrison&rsquo;s Principles of Internal Medicine, 20<sup>th</sup> Edition)</p>"), 
             placement = "right", trigger = "focus", options = NULL)
  
  #### choices_ac ####
  observeEvent(input$choices_ac,{
    value_fin$finished <-0
    disable("Summary_model")
    disable("Visualization")
    hide("Summary_bsButton")
    show("Summary_input_table")
    hide("Summary_model_table")
    hide("Visualization_plot")
    if(input$choices_ac=="adult"){
      show("cap_severe_single_adult")
      hide("cap_severe_single_child")
      # Populate checkboxGroupInput dynamically with all antibiotics
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_antibiotics",
        choices = first_choice_list_adult$Antibiotic,  # Use Antibiotic column as choices
        selected = first_choice_list_adult$Antibiotic  # Select all by default
      )
      
      show("type_patients_inputs_single")
      show("severity_cases_inputs_single")
      show("availability_antibiotics_inputs_single")
      show("prevalence_amr_inputs_single")
      show("total_patients_inputs_single")
      hide("severity_cases_inputs_child")
      
      hide("type_patients_inputs_both")
      hide("severity_cases_inputs_both")
      hide("availability_antibiotics_inputs_both")
      hide("prevalence_amr_inputs_both")
      hide("total_patients_inputs_both")
    }else if(input$choices_ac=="child"){
      show("cap_severe_single_child")
      hide("cap_severe_single_adult")
      # Populate checkboxGroupInput dynamically with all antibiotics
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_antibiotics",
        choices = first_choice_list_child$Antibiotic,  # Use Antibiotic column as choices
        selected = first_choice_list_child$Antibiotic  # Select all by default
      )
      
      show("type_patients_inputs_single")
      show("severity_cases_inputs_single")
      show("availability_antibiotics_inputs_single")
      show("prevalence_amr_inputs_single")
      show("total_patients_inputs_single")
      show("severity_cases_inputs_child")
      
      hide("type_patients_inputs_both")
      hide("severity_cases_inputs_both")
      hide("availability_antibiotics_inputs_both")
      hide("prevalence_amr_inputs_both")
      hide("total_patients_inputs_both")
    }else{
      hide("cap_severe_single_adult")
      hide("cap_severe_single_child")
      
      hide("type_patients_inputs_single")
      hide("severity_cases_inputs_single")
      hide("availability_antibiotics_inputs_single")
      hide("prevalence_amr_inputs_single")
      hide("total_patients_inputs_single")
      
      show("type_patients_inputs_both")
      show("severity_cases_inputs_both")
      show("availability_antibiotics_inputs_both")
      show("prevalence_amr_inputs_both")
      show("total_patients_inputs_both")
    }
  })
  

  # Populate checkboxGroupInput dynamically with all antibiotics
  updateCheckboxGroupInput(
    session = session,
    inputId = "selected_antibiotics_adult",
    choices = first_choice_list_adult$Antibiotic,  # Use Antibiotic column as choices
    selected = first_choice_list_adult$Antibiotic  # Select all by default
  )
  # Populate checkboxGroupInput dynamically with all antibiotics
  updateCheckboxGroupInput(
    session = session,
    inputId = "selected_antibiotics_child",
    choices = first_choice_list_child$Antibiotic,  # Use Antibiotic column as choices
    selected = first_choice_list_child$Antibiotic  # Select all by default
  )
  # Display the count of selected antibiotics
  output$prob_1stchoice <- renderText({
    if(input$choices_ac=="adult"){
      paste("proportion of recommended first-choice antibiotics which are available: ", round(length(input$selected_antibiotics)/length(first_choice_list_adult$Antibiotic),2))
    }else if(input$choices_ac=="child"){
      paste("proportion of recommended first-choice antibiotics which are available: ", round(length(input$selected_antibiotics)/length(first_choice_list_child$Antibiotic),2))
    }
  })
  
  output$prob_1stchoice_adult <- renderText({
    paste("proportion of recommended first-choice antibiotics which are available: ", round(length(input$selected_antibiotics_adult)/length(first_choice_list_adult$Antibiotic),2))
  })
  
  output$prob_1stchoice_child <- renderText({
    paste("proportion of recommended first-choice antibiotics which are available: ", round(length(input$selected_antibiotics_child)/length(first_choice_list_child$Antibiotic),2))
  })
  
  #Estimated Empirical Antibiotic Use Patterns Based on WHO Guidelines
  output$summary_guidelines_text <- renderUI({
    HTML(paste("Based on the WHO AWaRe book guidance, around ",value_summary$antibiotic_access, " of empirical antibiotic use is expected to come from “Access” antibiotics 
          and ",value_summary$antibiotic_watch," from “Watch” antibiotics. ", 
          value_summary$antibiotic_class_access[,1] ," are expected to be used the most ",value_summary$antibiotic_class_access[,2]," followed by ",
          value_summary$antibiotic_class_watch[,1]," ",value_summary$antibiotic_class_watch[,2],". Percentages in the brackets reflect 95% uncertainty ranges from model estimates.<br><br>
          Please note that the expected empirical antibiotic use is estimated for a single day, based on the assumption that empirical treatment for the specified infection strictly adheres to the WHO guidelines and the input data. 
          "))
  })
  
  output$summary_guidelines_text_adult <- renderUI({
    HTML(paste("Based on the WHO AWaRe book guidance, around ",value_summary$antibiotic_access_adult, " of empirical antibiotic use is expected to come from “Access” antibiotics 
          and ",value_summary$antibiotic_watch_adult," from “Watch” antibiotics. ", 
          value_summary$antibiotic_class_access_adult[,1] ," are expected to be used the most ",value_summary$antibiotic_class_access_adult[,2]," followed by ",
          value_summary$antibiotic_class_watch_adult[,1]," ",value_summary$antibiotic_class_watch_adult[,2],". Percentages in the brackets reflect 95% uncertainty ranges from model estimates.<br><br>
          Please note that the expected empirical antibiotic use is estimated for a single day, based on the assumption that empirical treatment for the specified infection strictly adheres to the WHO guidelines and the input data. 
          "))
  })
  
  output$summary_guidelines_text_child <- renderUI({
    HTML(paste("Based on the WHO AWaRe book guidance, around ",value_summary$antibiotic_access_child, " of empirical antibiotic use is expected to come from “Access” antibiotics 
          and ",value_summary$antibiotic_watch_child," from “Watch” antibiotics. ", 
          value_summary$antibiotic_class_access_child[,1] ," are expected to be used the most ",value_summary$antibiotic_class_access_child[,2]," followed by ",
          value_summary$antibiotic_class_watch_child[,1]," ",value_summary$antibiotic_class_watch_child[,2],". Percentages in the brackets reflect 95% uncertainty ranges from model estimates.<br><br>
          Please note that the expected empirical antibiotic use is estimated for a single day, based on the assumption that empirical treatment for the specified infection strictly adheres to the WHO guidelines and the input data. 
          "))
  })
  
}
