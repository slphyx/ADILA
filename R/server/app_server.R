#SLPHYX@SHIFT-ENTER

library(shiny)
library(dplyr)
library(gtsummary)
library(gtools)
library(plotly)
library(shinyalert)

# Define server logic
app_server <- function(session,input, output) {
  # Load shinyjs to enable showing/hiding
  useShinyjs()
  value_fin$run <-0
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  observeEvent("", {
    show("Summary_input_table")
    hide("Summary_model_table")
    hide("Visualization_plot")
    hide("Summary_input")
    hide("Summary_model")
    hide("Visualization")
    hide("summary_inputs_ui")
    hide("summary_inputs_ui2")
    hide("about_text")
    hide("howto_text")
  }, once = TRUE)

  # goto_intro
  observeEvent(input$goto_intro,{
    value_fin$run <-0
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    hide("Summary_input")
    hide("Summary_input_table")
    hide("Summary_model")
    hide("Visualization")
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
    hide("Summary_input")
    hide("Summary_input_table")
    hide("Summary_model")
    hide("Visualization")
    hide("intro_text")
    hide("about_text")
    hide("Summary_model_table")
    hide("Visualization_plot")
    show("howto_text")
    
  })
  
  
  # goto_simulation
  observeEvent(input$goto_simulation,{
    value_fin$run <-1
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    show("Summary_input")


    if(value_fin$finished){
      show("Summary_model")
      show("Visualization")
    }
    show("Summary_input_table")
    show("summary_inputs_ui")
    show("summary_inputs_ui2")
    hide("intro_text")
    hide("howto_text")
    hide("about_text")
    hide("partnersImage")
    # show only when the simulation has run
    # show("Summary_model")
    # show("Visualization")
    
  })
  
  
  # goto_details
  observeEvent(input$goto_about,{
    value_fin$run <-0
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    disable("sidebar_button")
    hide("Summary_input")
    hide("Summary_input_table")
    hide("Summary_model")
    hide("Visualization")
    hide("intro_text")
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

  # Output the 'input.adult' data frame as a table
  output$input_summary <- renderTable({
    input_big()$input.adult
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
        #showNotification("Warning: The number of total admitted patients is below the sum of all patients with different infections !", type = "warning", duration = 5)
        shinyalert("Warning!", paste0("Simulated total must meet or exceed combined infected cases!\n 
                                      Total admitted patients mustn't be more than ","\"",input$admitted_patients, "\" total = ",value_fin$TotalAdmittedPatient ), type = "warning")
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
        #showNotification("Warning: The number of total admitted patients is below the sum of all patients with different infections !", type = "warning", duration = 5)
        shinyalert("Warning!", paste0("Simulated total must meet or exceed combined infected cases!\n 
                                      Total admitted patients (Adult) mustn't be more than ","\"",input$admitted_patients_adult , "\" total = ",value_fin$TotalAdmittedPatient_adult), type = "warning")
        disable("run_model")
      }else if(input$admitted_patients_child < value_fin$TotalAdmittedPatient_child){
        #showNotification("Warning: The number of total admitted patients is below the sum of all patients with different infections !", type = "warning", duration = 5)
        shinyalert("Warning!", paste0("Simulated total must meet or exceed combined infected cases!\n 
                                      Total admitted patients (Child) mustn't be more than ","\"",input$admitted_patients_child, "\" total = ",value_fin$TotalAdmittedPatient_child ), type = "warning")
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
    shinyjs::disable("run_model")

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
    # Table 1
    summary_table_overall <- df_numeric[,1:8] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
        type = all_categorical() ~ "continuous",
        missing = "no"
      ) %>%
      modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
      modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose") %>%
      modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
      ) %>%
      modify_table_body(
        ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
      ) %>%
      as_gt()
    
    # Table 2
    summary_table_syndrome <- df_numeric[,9:(9+21)] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
        type = all_categorical() ~ "continuous",
        missing = "no"
      ) %>%
      modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
      modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
      modify_caption("**Table 2: Expected empirical antibiotic usage by infection syndrome**") %>% 
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
      ) %>%
      modify_table_body(
        ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
      ) %>%
      as_gt()
    
    # Table 3
    summary_table_class_access <- df_numeric[, 31:38] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
        type = all_categorical() ~ "continuous",
        missing = "no"
      ) %>%
      modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DDD)**") %>%
      modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose") %>%
      modify_caption("**Table 3: Expected empirical access antibiotic usage by antibiotic class**") %>% 
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
      )%>%
      modify_table_body(
        ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
      ) %>%
      as_gt()
    
    
    # Table 4
    summary_table_class_watch <- df_numeric[, 39:ncol(df_numeric)] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
        type = all_categorical() ~ "continuous",
        missing = "no"
      ) %>%
      modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DDD)**") %>%
      modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose") %>%
      modify_caption("**Table 4: Expected empirical watch antibiotic usage by antibiotic class**") %>%
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
      ) %>%
      modify_table_body(
        ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
      ) %>%
      as_gt()
    
    
    #Plotting----
    # Change the dataformat for the plot
    df_plot <- df_numeric %>%
      gather(key = "text", value= "value") %>%
      mutate(value = round(as.numeric(value),1))
    
    
    # Distribution of "Access" and "Watch" antibiotic usage out of total
    
    # Plot 1
    # Access antibiotic
    plot_access <- df_plot %>%
      filter(text %in% c("Access antibiotics(%)")) %>%
      ggplot( aes(x=value, fill=text)) +
      geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 1) +
      scale_fill_manual(values = c("#009E73")) +
      theme_ipsum() +
      labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
           x = "Percentage of Overall Usage", 
           y = "Frequency") +
      scale_x_continuous(labels = scales::percent_format(scale = 1),
                         limits = c(0,100)) +
      theme(legend.position = "none",
            axis.text   = element_text(size = 8),
            axis.title  = element_text(size = 8),
            title  = element_text(size = 10))
    
    plot_access <- ggplotly(plot_access)
    
    # Plot 2
    # Watch antibiotic 
    plot_watch <- df_plot %>%
      filter(text %in% c("Watch antibiotics(%)")) %>%
      ggplot( aes(x=value, fill=text)) +
      geom_histogram(color="#5d5e5f", alpha=0.6, position = 'identity', binwidth = 1) +
      scale_fill_manual(values = c("#F0E442")) +
      theme_ipsum() +
      labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
           x = "Percentage of Overall Usage", 
           y = "Frequency") +
      scale_x_continuous(labels = scales::percent_format(scale = 1),
                         limits = c(0,100)
                         ) +
      theme(legend.position = "none",
            axis.text   = element_text(size = 8),
            axis.title  = element_text(size = 8),
            title  = element_text(size = 10))
    
    plot_watch <- ggplotly(plot_watch)
    
    
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
    
    # Plot 3
    # Access antibiotic by antibiotic class
    plot_access_class <- df_class %>%
      filter(aware == "Access") %>%
      #filter(!(text %in% c("Amphenicols","Tetracyclines", "Lincosamides"))) %>%
      ggplot( aes(x=value, fill=text)) +
      geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 2) +
      scale_fill_viridis(discrete=TRUE) +
      theme_ipsum() +
      labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
           x = "Expected usage (DDD)", 
           y = "Frequency",
           fill = "Antibiotic class") +
      theme(legend.text = element_text(size = 8),
            axis.text   = element_text(size = 8),
            axis.title  = element_text(size = 8),
            title  = element_text(size = 10))
    
    plot_access_class <- ggplotly(plot_access_class)
    
    # Plot 4
    # Watch antibiotic by antibiotic class
    plot_watch_class <- df_class %>%
      filter(aware == "Watch") %>%
      ggplot( aes(x=value, fill=text)) +
      geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 2) +
      scale_fill_viridis(discrete=TRUE) +
      theme_ipsum() +
      labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
           x = "Expected usage (DDD)", 
           y = "Frequency",
           fill = "Antibiotic class") +
      theme(legend.text = element_text(size = 8),
            axis.text   = element_text(size = 8),
            axis.title  = element_text(size = 8),
            title  = element_text(size = 10))
    
    plot_watch_class <- ggplotly(plot_watch_class)
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
      summary_table_overall <- df_numeric[,1:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
        modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        )  %>%
        as_gt()
      
      # Table 2
      summary_table_syndrome <- df_numeric[,9:(9+21)] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
        modify_caption("**Table 2: Expected empirical antibiotic usage by infection syndrome**") %>% 
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        )  %>%
        as_gt()
      
      # Table 3
      summary_table_class_access <- df_numeric[, 31:38] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DOT)**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
        modify_caption("**Table 3: Expected empirical Access antibiotic usage by antibiotic class**") %>% 
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        )%>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      
      # Table 4
      summary_table_class_watch <- df_numeric[, 39:ncol(df_numeric)] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DOT)**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
        modify_caption("**Table 4: Expected empirical Watch antibiotic usage by antibiotic class**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      
      #Plotting----
      # Change the dataformat for the plot
      df_plot <- df_numeric %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1))
      
      
      # Distribution of "Access" and "Watch" antibiotic usage out of total
      
      # Plot 1
      # Access antibiotic
      plot_access <- df_plot %>%
        filter(text %in% c("Access antibiotics(%)")) %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 1) +
        scale_fill_manual(values = c("#009E73")) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Percentage of Overall Usage", 
             y = "Frequency") +
        scale_x_continuous(labels = scales::percent_format(scale = 1),
                           limits = c(0,100)) +
        theme(legend.position = "none",
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_access <- ggplotly(plot_access)
      
      # Plot 2
      # Watch antibiotic 
      plot_watch <- df_plot %>%
        filter(text %in% c("Watch antibiotics(%)")) %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#5d5e5f", alpha=0.6, position = 'identity', binwidth = 1) +
        scale_fill_manual(values = c("#F0E442")) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Percentage of Overall Usage", 
             y = "Frequency") +
        scale_x_continuous(labels = scales::percent_format(scale = 1),
                           limits = c(0,100)) +
        theme(legend.position = "none",
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_watch <- ggplotly(plot_watch)
      
      
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
      
      # Plot 3
      # Access antibiotic by antibiotic class
      plot_access_class <- df_class %>%
        filter(aware == "Access") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(legend.text = element_text(size = 8),
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_access_class <- ggplotly(plot_access_class)
      
      # Plot 4
      # Watch antibiotic by antibiotic class
      plot_watch_class <- df_class %>%
        filter(aware == "Watch") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(legend.text = element_text(size = 8),
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_watch_class <- ggplotly(plot_watch_class)
    }else if (input$choices_ac == "both"){
      #### both adult ####
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
      # Table 1
      summary_table_overall_adult <- df_numeric[,1:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose") %>%
        modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      # Table 2
      summary_table_syndrome_adult <- df_numeric[,9:(9+21)] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
        modify_caption("**Table 2: Expected empirical antibiotic usage by infection syndrome**") %>% 
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      # Table 3
      summary_table_class_access_adult <- df_numeric[, 31:38] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DDD)**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose") %>%
        modify_caption("**Table 3: Expected empirical access antibiotic usage by antibiotic class**") %>% 
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        )%>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      
      # Table 4
      summary_table_class_watch_adult <- df_numeric[, 39:ncol(df_numeric)] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DDD)**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DDD = defined daily dose") %>%
        modify_caption("**Table 4: Expected empirical watch antibiotic usage by antibiotic class**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      
      #Plotting----
      # Change the dataformat for the plot
      df_plot <- df_numeric %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1))
      
      
      # Distribution of "Access" and "Watch" antibiotic usage out of total
      
      # Plot 1
      # Access antibiotic
      plot_access <- df_plot %>%
        filter(text %in% c("Access antibiotics(%)")) %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 1) +
        scale_fill_manual(values = c("#009E73")) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Percentage of Overall Usage", 
             y = "Frequency") +
        scale_x_continuous(labels = scales::percent_format(scale = 1),
                           limits = c(0,100)) +
        theme(legend.position = "none",
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_access_adult <- ggplotly(plot_access)
      
      # Plot 2
      # Watch antibiotic 
      plot_watch <- df_plot %>%
        filter(text %in% c("Watch antibiotics(%)")) %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#5d5e5f", alpha=0.6, position = 'identity', binwidth = 1) +
        scale_fill_manual(values = c("#F0E442")) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Percentage of Overall Usage", 
             y = "Frequency") +
        scale_x_continuous(labels = scales::percent_format(scale = 1),
                           limits = c(0,100)
        ) +
        theme(legend.position = "none",
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_watch_adult <- ggplotly(plot_watch)
      
      
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
      
      # Plot 3
      # Access antibiotic by antibiotic class
      plot_access_class <- df_class %>%
        filter(aware == "Access") %>%
        #filter(!(text %in% c("Amphenicols","Tetracyclines", "Lincosamides"))) %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 2) +
        scale_fill_viridis(discrete=TRUE) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Expected usage (DDD)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(legend.text = element_text(size = 8),
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_access_class_adult <- ggplotly(plot_access_class)
      
      # Plot 4
      # Watch antibiotic by antibiotic class
      plot_watch_class <- df_class %>%
        filter(aware == "Watch") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 2) +
        scale_fill_viridis(discrete=TRUE) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Expected usage (DDD)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(legend.text = element_text(size = 8),
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_watch_class_adult <- ggplotly(plot_watch_class)
      
      #### both child ####
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
      summary_table_overall_child <- df_numeric[,1:8] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
        modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0(0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0(0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        )  %>%
        as_gt()
      
      # Table 2
      summary_table_syndrome_child <- df_numeric[,9:(9+21)] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
        modify_caption("**Table 2: Expected empirical antibiotic usage by infection syndrome**") %>% 
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        )  %>%
        as_gt()
      
      # Table 3
      summary_table_class_access_child <- df_numeric[, 31:38] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DOT)**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
        modify_caption("**Table 3: Expected empirical Access antibiotic usage by antibiotic class**") %>% 
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        )%>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      
      # Table 4
      summary_table_class_watch_child <- df_numeric[, 39:ncol(df_numeric)] %>%
        tbl_summary(
          by = NULL,
          statistic = all_continuous() ~ "{median} ({p25}, {p75})",
          type = all_categorical() ~ "continuous",
          missing = "no"
        ) %>%
        modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DOT)**") %>%
        modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
        modify_caption("**Table 4: Expected empirical Watch antibiotic usage by antibiotic class**") %>%
        modify_table_body(
          ~ .x %>% mutate(stat_0 = if_else(stat_0  %in% c("0 (0%)", "1,000 (100%)"), "NA", stat_0))
        ) %>%
        modify_table_body(
          ~ .x %>% filter(!(label == "0" & (stat_0 == "1,000 (100%)" | stat_0 == "0 (0%)" | stat_0 == "NA"))) # Remove rows where label is "0       1,000 (100%)"
        ) %>%
        as_gt()
      
      
      #Plotting----
      # Change the dataformat for the plot
      df_plot <- df_numeric %>%
        gather(key = "text", value= "value") %>%
        mutate(value = round(as.numeric(value),1))
      
      
      # Distribution of "Access" and "Watch" antibiotic usage out of total
      
      # Plot 1
      # Access antibiotic
      plot_access <- df_plot %>%
        filter(text %in% c("Access antibiotics(%)")) %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 1) +
        scale_fill_manual(values = c("#009E73")) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Percentage of Overall Usage", 
             y = "Frequency") +
        scale_x_continuous(labels = scales::percent_format(scale = 1),
                           limits = c(0,100)) +
        theme(legend.position = "none",
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_access_child <- ggplotly(plot_access)
      
      # Plot 2
      # Watch antibiotic 
      plot_watch <- df_plot %>%
        filter(text %in% c("Watch antibiotics(%)")) %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#5d5e5f", alpha=0.6, position = 'identity', binwidth = 1) +
        scale_fill_manual(values = c("#F0E442")) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Percentage of Overall Usage", 
             y = "Frequency") +
        scale_x_continuous(labels = scales::percent_format(scale = 1),
                           limits = c(0,100)) +
        theme(legend.position = "none",
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_watch_child <- ggplotly(plot_watch)
      
      
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
      
      # Plot 3
      # Access antibiotic by antibiotic class
      plot_access_class <- df_class %>%
        filter(aware == "Access") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(legend.text = element_text(size = 8),
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_access_class_child <- ggplotly(plot_access_class)
      
      # Plot 4
      # Watch antibiotic by antibiotic class
      plot_watch_class <- df_class %>%
        filter(aware == "Watch") %>%
        ggplot( aes(x=value, fill=text)) +
        geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 1) +
        scale_fill_viridis(discrete=TRUE) +
        theme_ipsum() +
        labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
             x = "Expected usage (DOT)", 
             y = "Frequency",
             fill = "Antibiotic class") +
        theme(legend.text = element_text(size = 8),
              axis.text   = element_text(size = 8),
              axis.title  = element_text(size = 8),
              title  = element_text(size = 10))
      
      plot_watch_class_child <- ggplotly(plot_watch_class)
      }
    shinyjs::enable("run_model")
    if(input$choices_ac != "both"){
    # Render the summary table to UI
    output$summary_table_overall <- render_gt({
      summary_table_overall
    })
    output$summary_table_syndrome <- render_gt({
      summary_table_syndrome
    })
    output$summary_table_class_access <- render_gt({
      summary_table_class_access
    })
    output$summary_table_class_watch <- render_gt({
      summary_table_class_watch
    })
    

    output$plot_access <- renderPlotly({
      plot_access
    })
    
    output$plot_watch <- renderPlotly({
      plot_watch
    })
    
    output$plot_access_class <- renderPlotly({
      plot_access_class
    })
    
    output$plot_watch_class <- renderPlotly({
      plot_watch_class
    })
    }else{
      # Render the summary table to UI both ####
      ##### adult#####
      
      output$summary_table_overall_adult <- render_gt({
        summary_table_overall_adult
      })
      output$summary_table_syndrome_adult <- render_gt({
        summary_table_syndrome_adult
      })
      output$summary_table_class_access_adult <- render_gt({
        summary_table_class_access_adult
      })
      output$summary_table_class_watch_adult <- render_gt({
        summary_table_class_watch_adult
      })
      
      
      output$plot_access_adult <- renderPlotly({
        plot_access_adult
      })
      
      output$plot_watch_adult <- renderPlotly({
        plot_watch_adult
      })
      
      output$plot_access_class_adult <- renderPlotly({
        plot_access_class_adult
      })
      
      output$plot_watch_class_adult <- renderPlotly({
        plot_watch_class_adult
      })
      #####child #####
      output$summary_table_overall_child <- render_gt({
        summary_table_overall_child 
      })
      output$summary_table_syndrome_child  <- render_gt({
        summary_table_syndrome_child 
      })
      output$summary_table_class_access_child <- render_gt({
        summary_table_class_access_child 
      })
      output$summary_table_class_watch_child  <- render_gt({
        summary_table_class_watch_child 
      })
      
      
      output$plot_access_child  <- renderPlotly({
        plot_access_child 
      })
      
      output$plot_watch_child  <- renderPlotly({
        plot_watch_child 
      })
      
      output$plot_access_class_child  <- renderPlotly({
        plot_access_class_child 
      })
      
      output$plot_watch_class_child  <- renderPlotly({
        plot_watch_class_child 
      })
    }
    value_fin$finished <-1
    show("Summary_model")
    show("Visualization")
    
    
    })
  })

  # Display the summary of inputs in a table in the "Summary Inputs" tab

  # output$summary_inputs <- renderTable({
  # 
  #   if(input$choices_ac == "adult"){
  #     req(!is.null(input_big()$adult_cases))
  #   input_big()$adult_cases
  #   }else if(input$choices_ac != "adult"){
  #     req(!is.null(input_big()$child_cases))
  #     input_big()$child_cases
  #   }
  # })
  output$adult_input_table <- renderTable({
      req(!is.null(input_big()$adult_cases))
      input_big()$adult_cases
    })
  
  output$child_input_table <- renderTable({
      req(!is.null(input_big()$child_cases))
      input_big()$child_cases
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
    input_big()$adult_para_data
  })

  output$child_input_table2 <- renderTable({
    req(!is.null(input_big()$child_para_data))
    input_big()$child_para_data
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
    tabBox(width =12,
      title = "",
              tabPanel(title = HTML("<b>Overall expected empirical antibiotic usage</b>"),
              tableOutput("summary_table_overall"),),
              tabPanel(title = HTML("<b>Expected empirical usage by syndrome</b>"),
              tableOutput("summary_table_syndrome")),
              tabPanel(title = HTML("<b>Expected empirical access usage by antibiotic classes</b>"),
              tableOutput("summary_table_class_access")),
              tabPanel(title = HTML("<b>Expected empirical watch usage by antibiotic classes</b>"),
              tableOutput("summary_table_class_watch")),
    )
    }else{
      tabBox(width =12,
             title = "",
             
             tabPanel(title = HTML("<b>Overall expected empirical antibiotic usage</b>"),
                      tabsetPanel(
                        tabPanel("Adult",tableOutput("summary_table_overall_adult")),
                        tabPanel("Child",tableOutput("summary_table_overall_child")),
                      )
                      ),
             tabPanel(title = HTML("<b>Expected empirical usage by syndrome</b>"),
                      tabsetPanel(
                        tabPanel("Adult",tableOutput("summary_table_syndrome_adult")),
                        tabPanel("Child",tableOutput("summary_table_syndrome_child")),
                      )
                      ),
             tabPanel(title = HTML("<b>Expected empirical access usage by antibiotic classes</b>"),
                      tabsetPanel(
                        tabPanel("Adult",tableOutput("summary_table_class_access_adult")),
                        tabPanel("Child",tableOutput("summary_table_class_access_child")),
                      )
                      ),
             tabPanel(title = HTML("<b>Expected empirical watch usage by antibiotic classes</b>"),
                      tabsetPanel(
                        tabPanel("Adult",tableOutput("summary_table_class_watch_adult")),
                        tabPanel("Child",tableOutput("summary_table_class_watch_child")),
                      )
                      ),
      )
    }
  })

  output$Visualization_output1 <- renderUI({
    if(input$choices_ac != "both"){
    tabBox(
      title = "",
      tabPanel(title = HTML("<b>Expected empirical usage by antibiotic classes</b>"),
               plotlyOutput("plot_access",height = "100%")
      )
    )
    }else{
      tabBox(
        title = "",
        tabPanel(title = HTML("<b>Expected empirical usage by antibiotic classes</b>"),
                 tabsetPanel(
                 tabPanel("Adult",plotlyOutput("plot_access_adult",height = "100%")),
                 tabPanel("Child",plotlyOutput("plot_access_child",height = "100%")),
                 )
        )
      )
    }
  })
  output$Visualization_output2 <- renderUI({
    if(input$choices_ac != "both"){
    tabBox(
      title = "",
      tabPanel(title = HTML("<b>Expected empirical usage by antibiotic classes</b>"),
               plotlyOutput("plot_watch",height = "100%")
      )
    )
    }else{
          tabBox(
      title = "",
      tabPanel(title = HTML("<b>Expected empirical usage by antibiotic classes</b>"),
               tabsetPanel(
               tabPanel("Adult",plotlyOutput("plot_watch_adult",height = "100%")),
               tabPanel("Child",plotlyOutput("plot_watch_child",height = "100%")),
               )
      )
    )
    }
  })
  output$Visualization_output3 <- renderUI({
    if(input$choices_ac != "both"){
    tabBox(width = 12,
      title = "",
      tabPanel(title = HTML("<b>Expected empirical Access Antibiotic Usage</b>"),
               plotlyOutput("plot_access_class",height = "100%")
      ),
      tabPanel(title = HTML("<b>Expected empirical Watch Antibiotic Usage</b>"),
               plotlyOutput("plot_watch_class",height = "100%")
      )
    )
    }else{
      tabBox(width = 12,
             title = "",
             tabPanel(title = HTML("<b>Expected empirical Access Antibiotic Usage</b>"),

                      tabsetPanel(
                        tabPanel("Adult",plotlyOutput("plot_access_class_adult",height = "100%")),
                        tabPanel("Child",plotlyOutput("plot_access_class_child",height = "100%")),
                      )
             ),
             tabPanel(title = HTML("<b>Expected empirical Watch Antibiotic Usage</b>"),

                      tabsetPanel(
                        tabPanel("Adult",plotlyOutput("plot_watch_class_adult",height = "100%")),
                        tabPanel("Child",plotlyOutput("plot_watch_class_child",height = "100%")),
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
  popover_id <- reactiveVal("cap_severe_1")  # Initial popover ID
  #### cap_severe #####
  addPopover(session,"cap_severe_adult",includeHTML("www/popover_text/cap_severe_adult.html"),
             placement = "bottom", trigger = "click", options = NULL)
  
  addPopover(session,"cap_severe_single_adult",
             includeHTML("www/popover_text/cap_severe_adult.html"), 
             placement = "bottom", trigger = "click", options = NULL)

  addPopover(session,"cap_severe_child",
             includeHTML("www/popover_text/cap_severe_child.html"), 
             placement = "bottom", trigger = "click", options = NULL)
  
  addPopover(session,"cap_severe_single_child",
             includeHTML("www/popover_text/cap_severe_child.html"),
             placement = "bottom", trigger = "click", options = NULL)
  
  ####abd_severe #####
  addPopover(session,"abd_severe",
             includeHTML("www/popover_text/abd_severe.html"),
             placement = "right", trigger = "click", options = NULL)
  
  addPopover(session,"abd_severe_adult",
             includeHTML("www/popover_text/abd_severe.html"),
             placement = "right", trigger = "click", options = NULL)
  
  addPopover(session,"abd_severe_child",
             includeHTML("www/popover_text/abd_severe.html"),
             placement = "right", trigger = "click", options = NULL)
  
  #### uti_severe #####
  addPopover(session,"uti_severe",
             HTML("<p>The default value (0.16) is based on a study that reviewed clinical outcome and risk factors for mortality in patients with acute pyelonephritis admitted to a hospital in Hong Kong.
             (DOI:<a class='link-pop' target='_blank' href='https://doi.org/10.12809/hkmj134061'>10.12809/hkmj134061</a>)</p>
<p><strong>Severity classification (</strong>Harrison textbook of internal medicine, 20<sup>th</sup> edition)</p>
<p><strong>Mild case:</strong>&nbsp; low-grade fever with or without lower-back pain or costovertebral-angle pain.</p>
<p><strong>Severe case:</strong> high fever, rigors, nausea, vomiting, and flank and / or loin pain.</p>"), 
             placement = "right", trigger = "click", options = NULL)
  
  addPopover(session,"uti_severe_adult",
             HTML("<p>The default value (0.16) is based on a study that reviewed clinical outcome and risk factors for mortality in patients with acute pyelonephritis admitted to a hospital in Hong Kong.
             (DOI:<a class='link-pop' target='_blank' href='https://doi.org/10.12809/hkmj134061'>10.12809/hkmj134061</a>)</p>
<p><strong>Severity classification (</strong>Harrison textbook of internal medicine, 20<sup>th</sup> edition)</p>
<p><strong>Mild case:</strong>&nbsp; low-grade fever with or without lower-back pain or costovertebral-angle pain.</p>
<p><strong>Severe case:</strong> high fever, rigors, nausea, vomiting, and flank and / or loin pain.</p>"), 
             placement = "right", trigger = "click", options = NULL)
  
  addPopover(session,"uti_severe_child",
             HTML("<p>The default value (0.16) is based on a study that reviewed clinical outcome and risk factors for mortality in patients with acute pyelonephritis admitted to a hospital in Hong Kong.
             (DOI:<a class='link-pop' target='_blank' href='https://doi.org/10.12809/hkmj134061'>10.12809/hkmj134061</a>)</p>
<p><strong>Severity classification (</strong>Harrison textbook of internal medicine, 20<sup>th</sup> edition)</p>
<p><strong>Mild case:</strong>&nbsp; low-grade fever with or without lower-back pain or costovertebral-angle pain.</p>
<p><strong>Severe case:</strong> high fever, rigors, nausea, vomiting, and flank and / or loin pain.</p>"), 
             placement = "right", trigger = "click", options = NULL)
  
  #### cdf_severe #####
  addPopover(session,"cdf_severe",
             includeHTML("www/popover_text/cdf_severe.html"),
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"cdf_severe_adult",
             includeHTML("www/popover_text/cdf_severe.html"),
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"cdf_severe_child",
             includeHTML("www/popover_text/cdf_severe.html"),
             placement = "right", trigger = "click", options = NULL)
  
  #### sst_nf #####
  addPopover(session,"sst_nf",
             HTML("<p>NF is an uncommon infection although not rare medical and surgical emergency. 
                  The estimated proportion of NF among SSTI patients is less than 0.05. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1016/S1473-3099(22)00583-7'>https://doi.org/10.1016/S1473-3099(22)00583-7</a>)</p>"), 
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"sst_nf_adult",
             HTML("<p>NF is an uncommon infection although not rare medical and surgical emergency. 
                  The estimated proportion of NF among SSTI patients is less than 0.05. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1016/S1473-3099(22)00583-7'>https://doi.org/10.1016/S1473-3099(22)00583-7</a>)</p>"), 
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"sst_nf_child",
             HTML("<p>NF is an uncommon infection although not rare medical and surgical emergency. 
                  The estimated proportion of NF among SSTI patients is less than 0.05. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1016/S1473-3099(22)00583-7'>https://doi.org/10.1016/S1473-3099(22)00583-7</a>)</p>"), 
             placement = "right", trigger = "click", options = NULL)
  
  #### hap_mdr ####
  addPopover(session,"hap_mdr",
             HTML("<p>The default value (0.37) is based on a 10-year prospective observational study on clinical and microbiological characteristics of adults with hospital-acquired pneumonia in China. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1007/s10096-020-04046-9'>https://doi.org/10.1007/s10096-020-04046-9</a> )</p>"), 
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"hap_mdr_adult",
             HTML("<p>The default value (0.37) is based on a 10-year prospective observational study on clinical and microbiological characteristics of adults with hospital-acquired pneumonia in China. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1007/s10096-020-04046-9'>https://doi.org/10.1007/s10096-020-04046-9</a> )</p>"), 
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"hap_mdr_child",
             HTML("<p>The default value (0.37) is based on a 10-year prospective observational study on clinical and microbiological characteristics of adults with hospital-acquired pneumonia in China. 
                  (<a class='link-pop' target='_blank' href='https://doi.org/10.1007/s10096-020-04046-9'>https://doi.org/10.1007/s10096-020-04046-9</a> )</p>"), 
             placement = "right", trigger = "click", options = NULL)
  
  #### strep_pyogenes ####
  addPopover(session,"strep_pyogenes",
             HTML("<p>Group A Streptococcus is implicated in ~ 60% of cases of necrotizing fasciitis. 
                  (Ref: Harrison&rsquo;s Principles of Internal Medicine, 20<sup>th</sup> Edition)</p>"), 
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"strep_pyogenes_adult",
             HTML("<p>Group A Streptococcus is implicated in ~ 60% of cases of necrotizing fasciitis. 
                  (Ref: Harrison&rsquo;s Principles of Internal Medicine, 20<sup>th</sup> Edition)</p>"), 
             placement = "right", trigger = "click", options = NULL)
  addPopover(session,"strep_pyogenes_child",
             HTML("<p>Group A Streptococcus is implicated in ~ 60% of cases of necrotizing fasciitis. 
                  (Ref: Harrison&rsquo;s Principles of Internal Medicine, 20<sup>th</sup> Edition)</p>"), 
             placement = "right", trigger = "click", options = NULL)
  #### choices_ac ####
  observeEvent(input$choices_ac,{
    value_fin$finished <-0
    hide("Summary_model")
    hide("Visualization")
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

}
