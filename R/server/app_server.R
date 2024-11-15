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
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  observeEvent("", {
    show("Summary_input_table")
    hide("Summary_model_table")
    hide("Visualization_plot")
    hide("Summary_input")
    hide("Summary_model")
    hide("Visualization")
    hide("summary_inputs_ui")
    hide("about_text")
    hide("howto_text")
  }, once = TRUE)

  # goto_intro
  observeEvent(input$goto_intro,{
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
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    show("Summary_input")
    show("summary_inputs_ui")
    show("Summary_input_table")

    if(value_fin$finished){
      show("Summary_model")
      show("Visualization")
    }
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
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
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
  
  observeEvent(input$save_input, {
    input_data <-data.frame(name_parameter = c(adult_cases[,1],para.data[,1],"std_err"),
                     value = c(adult_cases[,2],para.data[,2],std_err)) 
    
    downloadHandler(
      filename = function() {
        paste0("input_data.csv")
      },
      content = function(file) {
        write.csv(input_data, file)
      }
    )
  })
  
  output$downloadInput <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("input_data.csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(data.frame(name_parameter = c(input_big()$adult_cases[,1],input_big()$para_data[,1],"std_err"),
                           value = c(input_big()$adult_cases[,2],input_big()$para_data[,2],input_big()$std_err*100)) , file,
                row.names=F)
    }
  )


  # Reactive block to compute input.adult
  input_big <- reactive({
    # write.csv

    # Inputs
    # std_err <- input$std_err / 100 # Convert percentage to decimal
    std_err <- 0.2   # fixed at 20%
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

    para.data <- data.frame(
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
      value = c(input$p_first, 
                input$cap_severe, 
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

    # Probability of each infection syndromes from the "adult_cases" data 
    # Probability of each infection syndromes from the "adult_cases" data 
    syd.adult <- rdirichlet(1, adult_cases$cases)
    
    # Total patients on antimicrobial
    pt.atb   <- sum(adult_cases$cases)
    # Total admitted patients
    pt.admt <- para.data$value[para.data$parameter=="total admitted patients"]
    
    # Probability of first-choice antibiotics
    p.first.est <- para.data$value[para.data$parameter=="probability of first-choice antibiotics"]
    p.first.para <- binomi.para(p.first.est, (p.first.est * std_err))
    p.first <- rbeta(1, p.first.para$alpha, p.first.para$beta)
    
    # Availability of first-choice antibiotic for specific infections
    p.first.cap <- p.first.bm <- p.first.abd <- p.first.bj <- p.first
    # Availability of first-choice antibiotic for different type of surgical prophylaxis
    p.first.bow.surg <- p.first.clean.cont.surg <- p.first.uro.surg <- p.first.cont.surg <- p.first
    
    
    # Prevalence of ESBL
    p.esbl.est  <- para.data$value[para.data$parameter=="prevalence of ESBL"]
    p.esbl.para <- binomi.para(p.esbl.est, (p.esbl.est * std_err))
    p.esbl <- rbeta(1, p.esbl.para$alpha, p.esbl.para$beta)
    
    # Assume the same prevalence/ risk of ESBL for different infection syndromes
    p.esbl.abd <- p.esbl.uti <- p.esbl.fn <- p.g.neg.fn <- p.esbl
    
    # Prevalence of MRSA
    p.mrsa.est  <- para.data$value[para.data$parameter=="prevalence of MRSA"]
    p.mrsa.para <- binomi.para(p.mrsa.est, (p.mrsa.est * std_err))
    p.mrsa <- rbeta(1, p.mrsa.para$alpha, p.mrsa.para$beta)
    
    # Assume the same prevalence/ risk of ESBL for different infection syndromes
    p.mrsa.sst <- p.mrsa.bj <- p.mrsa.fn <- p.mrsa
    
    # Prevalence of Strep pyogenes in patients with necrotizing fasciitis
    p.Strep.pyogene.est  <- para.data$value[para.data$parameter=="prevalence of Strep pyogenes infection in necrotizing fasciitis"]
    p.Strep.pyogene.para <- binomi.para(p.Strep.pyogene.est, (p.Strep.pyogene.est * std_err))
    p.Strep.pyogene      <- rbeta(1, p.Strep.pyogene.para$alpha, p.Strep.pyogene.para$beta)
    
    # Severity of cases
    # Probability of severe CAP cases
    p.sev.cap.est <- para.data$value[para.data$parameter=="proportion of severe cases in CAP patients"]
    alpha.cap <- p.sev.cap.est * adult_cases$cases[adult_cases$syndrome=="Patients with community acquired pneumonia (CAP)"]
    beta.cap  <- (1- p.sev.cap.est) * adult_cases$cases[adult_cases$syndrome=="Patients with community acquired pneumonia (CAP)"]
    p.sev.cap <- rbeta(1, 1+alpha.cap, 1+beta.cap)
    
    p.curb.high <- p.sev.cap # Assume CURB > 2 = proportion of severe CAP
    
    # Probability of high risk of multi-drug resistant infeciton in HAP
    p.highrisk.hap.est <- para.data$value[para.data$parameter=="probability(risk) of multi-drug resistant infection in HAP patients"]
    alpha.hap <- p.highrisk.hap.est * adult_cases$cases[adult_cases$syndrome=="Patients with hospital acquired pneumonia (HAP) Non-VAP"]
    beta.hap  <-  (1 - p.highrisk.hap.est) * adult_cases$cases[adult_cases$syndrome=="Patients with hospital acquired pneumonia (HAP) Non-VAP"]
    p.highrisk.hap <- rbeta(1, 1+alpha.hap, 1+beta.hap)
    
    
    # Probability of cases with severe intra-abdominal infection
    p.sev.abd.est <- para.data$value[para.data$parameter=="proportion of severe cases in patients with intra-abdominal infection"]
    alpha.abd <- p.sev.abd.est * adult_cases$cases[adult_cases$syndrome=="Patients with intra-abdominal infection (IA)"]
    beta.abd  <- (1 - p.sev.abd.est) * adult_cases$cases[adult_cases$syndrome=="Patients with intra-abdominal infection (IA)"]
    p.sev.abd <- rbeta(1, 1+alpha.abd, 1+beta.abd)
    
    # Probability of upper UTI (Pyelonephritis) with severe cases
    p.sev.uti.est <- para.data$value[para.data$parameter=="proportion of severe cases in patients with acute pyelonephritis (upper UTI)"]
    alpha.uti <- p.sev.uti.est * adult_cases$cases[adult_cases$syndrome=="Patients with upper UTI"]
    beta.uti  <- (1 - p.sev.uti.est) * adult_cases$cases[adult_cases$syndrome=="Patients with upper UTI"]
    p.sev.uti <- rbeta(1, 1+alpha.uti, 1+beta.uti)
    
    # Probability of C. difficile with severe cases
    p.sev.cdf.est <- para.data$value[para.data$parameter=="proportion of severe cases in patients with C. difficile infection"]
    alpha.cdf <- p.sev.cdf.est * adult_cases$cases[adult_cases$syndrome=="Patients with Clostridioides difficile infection (CDIF)"]
    beta.cdf  <- (1 - p.sev.cdf.est) * adult_cases$cases[adult_cases$syndrome=="Patients with Clostridioides difficile infection (CDIF)"]
    p.sev.cdf <- rbeta(1, 1+alpha.cdf, 1+beta.cdf)
    
    # Probability of "Necrotizing fasciitis" among patients with skin and soft-tissue infection
    p.nf.est <- para.data$value[para.data$parameter=="proportion of necrotizing fasciitis cases in patient with SST"]
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
        para_data = para.data,
        input.adult = input.adult
      )
    return(outlist)
  })

  # Output the 'input.adult' data frame as a table
  output$input_summary <- renderTable({
    input_big()$input.adult
  })
  
  
  # Observe changes in `admitted_patients` and show warning if below total enter cases
  observe({
    value_fin$TotalAdmittedPatient <- sum(input_big()$adult_cases[["cases"]])
  })
  
  observeEvent(c(input$admitted_patients,input$cap_cases , input$hap_cases,input$bm_cases,
                 input$ia_cases, input$uut_cases,    input$sst_cases,    input$bji_cases, 
                 input$cdif_cases,input$fn_cases,     input$sepsis_cases, input$sp_cases 
                 
                 ), {
    if (is.na(value_fin$TotalAdmittedPatient) | is.na(input$admitted_patients)) {
        disable("run_model")
    }else if(input$admitted_patients < value_fin$TotalAdmittedPatient){
      #showNotification("Warning: The number of total admitted patients is below the sum of all patients with different infections !", type = "warning", duration = 5)
      shinyalert("Warning!", paste0("Simulated total must meet or exceed combined infected cases!\n 
                                    Total admitted patients mustn't be less than ","\"",value_fin$TotalAdmittedPatient , "\""), type = "warning")
      disable("run_model")
    }
    else{
      enable("run_model")
    }
  })


  # Run the model when the button is clicked
  observeEvent(input$run_model, {
    withProgress(message = 'Simulation in progress…', value = 0, {
    shinyjs::disable("run_model")

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
                                              input_big()$para_data, input_big()$std_err)

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
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(is.na(stat_0), "NA", stat_0))
      )  %>%
      as_gt()
    
    # Table 2
    summary_table_syndrome <- df_numeric[,9:(9+21)] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
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
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(is.na(stat_0), "NA", stat_0))
      )  %>%
      as_gt()
    
    # Table 3
    summary_table_class_access <- df_numeric[, 31:38] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
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
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(is.na(stat_0), "NA", stat_0))
      )  %>%
      as_gt()
    
    
    # Table 4
    summary_table_class_watch <- df_numeric[, 39:ncol(df_numeric)] %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
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
      modify_table_body(
        ~ .x %>% mutate(stat_0 = if_else(is.na(stat_0), "NA", stat_0))
      )  %>%
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

    shinyjs::enable("run_model")

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
    value_fin$finished <-1
    show("Summary_model")
    show("Visualization")
    
    
    })
  })

  # Display the summary of inputs in a table in the "Summary Inputs" tab

  output$summary_inputs <- renderTable({
    input_big()$adult_cases
  })
  output$first_choice_table <- renderTable({
    first_choice_list
  })
  output$summary_inputs2 <- renderTable({
    input_big()$para_data
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

    params <- read.csv(input$load_params$datapath)
    params_vec <- c(params[,2])
    names(params_vec) <- params[,1]

    # Load parameters into the input fields
    load_disease_inputs(params_vec, session)  # Load inputs function
  })
  
  # UI - OUTCOME  -----------------------------------------------------
  
  output$summary_output <- renderUI({
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
  })

  output$Visualization_output1 <- renderUI({
    tabBox(
      title = "",
      tabPanel(title = HTML("<b>Expected empirical usage by antibiotic classes</b>"),
               plotlyOutput("plot_access",height = "100%")
      )
    )
  })
  output$Visualization_output2 <- renderUI({
    tabBox(
      title = "",
      tabPanel(title = HTML("<b>Expected empirical usage by antibiotic classes</b>"),
               plotlyOutput("plot_watch",height = "100%")
      )
    )
  })
  output$Visualization_output3 <- renderUI({
    tabBox(width = 12,
      title = "",
      tabPanel(title = HTML("<b>Expected empirical Access Antibiotic Usage</b>"),
               plotlyOutput("plot_access_class",height = "100%")
      ),
      tabPanel(title = HTML("<b>Expected empirical Watch Antibiotic Usage</b>"),
               plotlyOutput("plot_watch_class",height = "100%")
      )
    )
  })
  

}
