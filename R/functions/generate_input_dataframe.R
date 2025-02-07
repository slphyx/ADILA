
library(gtools)

# Function to generate the input dataframe
generate_input_dataframe <- function(adult_cases, adult_para.data, std_err) {

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
  
  return(input.adult)
}

# Function to generate the input dataframe
generate_input_dataframe_child <- function(child_cases, child.para.data, std_err) {
  
  # Probability of each infection syndromes from the "child_cases" data 
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
  p.sev.uti.est <- child.para.data$value[child.para.data$parameter=="proportion of sever cases in patients with acute pyelonephritis (upper UTI)"]
  alpha.uti <- p.sev.uti.est * child_cases$cases[child_cases$syndrome=="Patients with upper UTI"]
  beta.uti  <- (1 - p.sev.uti.est) * child_cases$cases[child_cases$syndrome=="Patients with upper UTI"]
  p.sev.uti <- rbeta(1, 1+alpha.uti, 1+beta.uti)
  
  # Probability of C. difficile with severe cases
  p.sev.cdf.est <- child.para.data$value[child.para.data$parameter=="proportion of sever cases in patients with C. difficle infection"]
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
    pt.atb   = pt.atb,
    # Total admitted patients
    pt.admt  = pt.admt,
    
    # Probability of first choice antibiotic 
    p.first.cap = p.first.cap, 
    p.first.bm  = p.first.bm,
    p.first.abd = p.first.abd,
    p.first.bj  = p.first.bj, 
    
    # Probability of each syndrome
    p.cap     = syd.child[1],
    p.hap     = syd.child[2],
    p.bmen    = syd.child[3],
    p.abd     = syd.child[4],
    p.uuti    = syd.child[5],
    p.sst     = syd.child[6],
    p.bj      = syd.child[7],
    p.clodiff = syd.child[8],
    p.fneut   = syd.child[9],
    p.sep     = syd.child[10],
    p.sug     = syd.child[11],
    
    # Severity for each syndrome 
    
    p.sev.cap      = p.sev.cap, 
    p.no.resp.cap  = p.no.resp.cap,
    p.hiv          = p.hiv,
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
    
    # Type of surgical prophylaxis 
    p.bowsg  = type.surg[1],
    p.clean  = type.surg[2],
    p.urosg  = type.surg[3],
    p.contsg = type.surg[4]
  )
  
  
  return(input.child)
}

