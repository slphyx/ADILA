library(shiny)
library(dplyr)
library(gtsummary)
library(gtools)



# Define server logic
app_server <- function(input, output) {

  # Load shinyjs to enable showing/hiding
  useShinyjs()
  hide("table1")
  # Toggle input parameters panel visibility when the button is clicked
  observeEvent(input$toggle_input, {
    shinyjs::toggle(id = "input_parameters")
  })


  # Reactive block to compute input.adult
  input_big <- reactive({

    # Inputs
    std_err <- input$std_err / 100 # Convert percentage to decimal

    adult_cases <- data.frame(
      syndrome = c("Patients with community acquired pneumonia (CAP)",
                   "Patients with hospital acquired pneumonia (HAP) non-VAP", 
                   "Patients with bacterial meningitis", 
                   "Patients with intra-abdominal infections", 
                   "Patients with upper urinary tract infection", 
                   "Patients with bone and joint infection (acute bacterial osteomyelitis and septic arthritis)", 
                   "Patients with skin and soft-tissue infection (necrotizing fasciitis and pyomyositis)", 
                   "Patients with febrile neutropenic", 
                   "Patients with sepsis & septic shock", 
                   "Patients with Clostridioides difficile infection", 
                   "Patients on surgical prophylaxis"),
      cases = c(input$cap_cases, input$hap_cases, input$cns_cases, input$ia_cases, input$pye_cases,
                input$sst_cases, input$bj_cases, input$cdif_cases, input$fn_cases, input$sepsis_cases, input$sp_cases)
    )

    para.data <- data.frame(
      parameter = c("probability of first-choice antibiotics", "proportion of severe cases in CAP patients", "probability(risk) of multi-drug resistant infection in HAP patients",
                    "proportion of severe cases in patients with intra-abdominal infection", "proportion of severe cases in patients with acute pyelonephritis (upper UTI)",
                    "proportion of severe cases in patients with C. difficile infection", "proportion of necrotizing fasciitis cases in patient with SST",
                    "prevalence of ESBL", "prevalence of MRSA", "prevalence of Strep pyogenes infection in necrotizing fasciitis", "total admitted patients"),
      value = c(input$p_first, input$cap_severe, input$hap_mdr, input$abd_severe,
                input$uti_severe, input$cdf_severe, input$sst_nf, input$esbl_prevalence,
                input$mrsa_prevalence, input$strep_pyogenes, input$admitted_patients)
    )

    # Probability of each infection syndromes from the "adult_cases" data
    syd.adult <- rdirichlet(1, adult_cases$cases)

    # Total patients on antimicrobial
    pt.atb   <- sum(adult_cases$cases)
    # Total admitted patients
    pt.admt <- para.data$value[para.data$parameter=="total admitted patients"]

    # Probability of first-choice antibiotics
    p.first.est <- para.data$value[para.data$parameter=="probability of first-choice antibiotics"]
    p.first.para <- binomi.para(p.first.est, std_err)
    p.first <- rbeta(1, p.first.para$alpha, p.first.para$beta)

    # Availability of first-choice antibiotic for specific infections
    p.first.cap <- p.first.bm <- p.first.abd <- p.first.bj <- p.first
    # Availability of first-choice antibiotic for different type of surgical prophylaxis
    p.first.bow.surg <- p.first.clean.cont.surg <- p.first.uro.surg <- p.first.cont.surg <- p.first


    # Prevalence of ESBL
    p.esbl.est  <- para.data$value[para.data$parameter=="prevalence of ESBL"]
    p.esbl.para <- binomi.para(p.esbl.est, std_err)
    p.esbl <- rbeta(1, p.esbl.para$alpha, p.esbl.para$beta)

    # Assume the same prevalence/ risk of ESBL for different infection syndromes
    p.esbl.abd <- p.esbl.uti <- p.esbl.fn <- p.g.neg.fn <- p.esbl

    # Prevalence of MRSA
    p.mrsa.est  <- para.data$value[para.data$parameter=="prevalence of MRSA"]
    p.mrsa.para <- binomi.para(p.mrsa.est, std_err)
    p.mrsa <- rbeta(1, p.mrsa.para$alpha, p.mrsa.para$beta)

    # Assume the same prevalence/ risk of ESBL for different infection syndromes
    p.mrsa.sst <- p.mrsa.bj <- p.mrsa.fn <- p.mrsa

    # Prevalence of Strep pyogenes in patients with necrotizing fasciitis
    p.Strep.pyogene.est  <- para.data$value[para.data$parameter=="prevalence of Strep pyogenes infection in necrotizing fasciitis"]
    p.Strep.pyogene.para <- binomi.para(p.Strep.pyogene.est, std_err)
    p.Strep.pyogene      <- rbeta(1, p.Strep.pyogene.para$alpha, p.Strep.pyogene.para$beta)

    # Severity of cases
    # Probability of severe CAP cases
    p.sev.cap.est <- para.data$value[para.data$parameter=="proportion of severe cases in CAP patients"]
    p.sev.cap.para <- binomi.para(p.sev.cap.est, std_err)
    p.sev.cap <- rbeta(1, p.sev.cap.para$alpha, p.sev.cap.para$beta)

    p.curb.high <- p.sev.cap # Assume CURB > 2 = proportion of severe CAP

    # Probability of high risk of multi-drug resistant infeciton in HAP
    p.highrisk.hap.est <- para.data$value[para.data$parameter=="probability(risk) of multi-drug resistant infection in HAP patients"]
    p.highrisk.hap.para <- binomi.para(p.highrisk.hap.est, std_err)
    p.highrisk.hap <- rbeta(1, p.highrisk.hap.para$alpha, p.highrisk.hap.para$beta)


    # Probability of cases with severe intra-abdominal infection
    p.sev.abd.est <- para.data$value[para.data$parameter=="proportion of severe cases in patients with intra-abdominal infection"]
    p.sev.abd.para <- binomi.para(p.sev.abd.est, std_err)
    p.sev.abd <- rbeta(1, p.sev.abd.para$alpha, p.sev.abd.para$beta)

    # Probability of upper UTI (Pyelonephritis) with severe cases
    p.sev.uti.est <- para.data$value[para.data$parameter=="proportion of sever cases in patients with acute pyelonephritis (upper UTI)"]
    p.sev.uti.para <- binomi.para(p.sev.abd.est, std_err)
    p.sev.uti <- rbeta(1, p.sev.uti.para$alpha, p.sev.uti.para$beta)

    # Probability of C. difficile with severe cases
    p.sev.cdf.est <- para.data$value[para.data$parameter=="proportion of severe cases in patients with C. difficile infection"]
    p.sev.cdf.para <- binomi.para(p.sev.cdf.est, std_err)
    p.sev.cdf <- rbeta(1, p.sev.cdf.para$alpha, p.sev.cdf.para$beta)

    # Probability of "Necrotizing fasciitis" among patients with skin and soft-tissue infection
    p.nf.est <- para.data$value[para.data$parameter=="proportion of necrotizing fasciitis cases in patient with SST"]
    p.nf.para <- binomi.para(p.nf.est, 0.02) # Assume std_err of 2%
    p.nf <- rbeta(1, p.nf.para$alpha, p.nf.para$beta)


    # Underlying causes of sepsis
    und.sepsis <- rdirichlet(1, c(10,10,10,10,10,10,10))

    # Underlying causes of sepsis
    type.surg <- rdirichlet(1, c(10,10,10,10))



    # Create a dataframe using the above model parameters
    input.adult <- data.frame(

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
      p.cap     <- syd.adult[1],
      p.hap     <- syd.adult[2],
      p.bmen    <- syd.adult[3],
      p.abd     <- syd.adult[4],
      p.uuti    <- syd.adult[5],
      p.sst     <- syd.adult[6],
      p.bj      <- syd.adult[7],
      p.clodiff <- syd.adult[8],
      p.fneut   <- syd.adult[9],
      p.sep     <- syd.adult[10],
      p.sug     <- syd.adult[11],

      # Severity for each syndrome

      p.sev.cap      <- p.sev.cap,
      p.curb.high    <- p.curb.high,
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
      p.snf           <- p.nf,           # Probability of suspected necrotizing fasciitis in sepsis patients with SST

      # Underlying causes of sepsis
      p.unk    <- und.sepsis[1],
      p.men    <- und.sepsis[2],
      p.lrti   <- und.sepsis[3],
      p.ent    <- und.sepsis[4],
      p.abd.s  <- und.sepsis[5],
      p.sst.s  <- und.sepsis[6],
      p.uti.s  <- und.sepsis[7],

      # Type of surgical prophylaxis
      p.bowsg  <- type.surg[1],
      p.clean  <- type.surg[2],
      p.urosg  <- type.surg[3],
      p.contsg <- type.surg[4]
    )


     # Store the inputs in a list
    outlist <-list(
        std_err = input$std_err/100,
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


  # Run the model when the button is clicked
  observeEvent(input$run_model, {
    withProgress(message = 'Making table', value = 0, {
    shinyjs::disable("run_model")

    # Create an empty dataframe to store model's output
    result_adult <- data.frame()

    # Get the input_adult_df values (reactive values)


    # Run the model 1000 times
    for (i in 1:1000) {
      set.seed(1000 + i)
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

    # Create summary table
    summary_table <- df_numeric %>%
      tbl_summary(
        by = NULL,
        statistic = all_continuous() ~ "{median} ({p25}, {p75})",
        missing = "no"
      ) %>%
      modify_header(label = "**Description**", stat_0 = "**Expected usage**") %>%
      modify_footnote(all_stat_cols() ~ "Median (IQR), 1000 iterations") %>%
      modify_caption("**Table: Expected antibiotic usage in hospital if the empirical prescription follows the WHO guidelines**") %>%
      # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
      as_gt()


    shinyjs::enable("run_model")

    # Render the summary table to UI
    output$summary_table <- render_gt({
      summary_table
    })
    show("table1")
    hide("summary_input_table1")
    hide("summary_input_table2")
    })
  })

  # Display the summary of inputs in a table in the "Summary Inputs" tab
  output$summary_inputs <- renderTable({
    input_big()$para_data
  })

  output$summary_inputs2 <- renderTable({
    input_big()$adult_cases
  })

}
