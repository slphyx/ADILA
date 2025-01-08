


# Decision tree for hospitalized children (1 month - 12 years)
# Management guideline based on WHO's AWaRe book 
# guidelines for 10 common infection syndrome plus surgical prophylaxis
# Detail outputs include number (proportion) for individual antibiotic under "Access"/ "Watch"
# Antibiotic class
# Route of administration 

model_C <- function(params){
  with(
    as.list(params),
    {
      # Community Acquired Pneumonia----
      # 3 days for low HIV prevalence and no chest indrawing
      # 5 days for high HIV prevalence and chest indrawing 
      # Mild-moderate cases (5 days)
      
      ep1 <- p.cap * (1-p.sev.cap)        # Access (Oral amoxicillin 80-90mg/kg/d)
      
      # Severe cases (5 days)
      # First choice if > 1yr and HIV (-)
      ep2 <- p.cap * p.sev.cap * (1-p.no.resp.cap) * (1-p.hiv) * 1/3 # A+A (P amox 50mg/kg 8h + P genta 7.5mg/kg/d)
      ep3 <- p.cap * p.sev.cap * (1-p.no.resp.cap) * (1-p.hiv) * 1/3 # A+A (P ampi 50mg/kg 8h + P genta 7.5mg/kg/d)
      ep4 <- p.cap * p.sev.cap * (1-p.no.resp.cap) * (1-p.hiv) * 1/3 # A+A (P benzyl pen 30mg/kg 8h + P genta 7.5mg/kg/d)
      
      # First choice if < 1yr and HIV (+)
      ep5 <- p.cap * p.sev.cap * (1-p.no.resp.cap) * p.hiv * 1/3  # A+A+Wa (P amox 50mg/kg 8h + P genta 7.5mg/kg/d + O/P septrin 40mg/kg 8h)
      ep6 <- p.cap * p.sev.cap * (1-p.no.resp.cap) * p.hiv * 1/3  # A+A+Wa (P ampi 50mg/kg 8h + P genta 7.5mg/kg/d + O/P septrin 40mg/kg 8h)
      ep7 <- p.cap * p.sev.cap * (1-p.no.resp.cap) * p.hiv * 1/3  # A+A+Wa (P benzyl pen 30mg/kg 8h + P genta 7.5mg/kg/d + O/P septrin 40mg/kg 8h) 
      
      # Second choice (if no clinical response to first choice after 48-72 hours)
      ep8  <- p.cap * p.sev.cap * p.no.resp.cap * 1/2     # Wa (P cefotaxime 50 mg/kg 8h)
      ep9  <- p.cap * p.sev.cap * p.no.resp.cap * 1/2     # Wa (P ceftrixone 80 mg/kg 24h)
      
      
      #Total probability
      p.tot.cap <- (ep1+ep2+ep3+ep4+ep5+ep6+ep7+ep8+ep9)/p.cap
      
      # Days of Therapy (DOT) of Access, Watch antibiotics
      # Access
      # Penicillin 
      cap.pen.dot   <- (ep1+ep2+ep3+ep4+ep5+ep6+ep7) * pt.atb
      # Gentamycin
      cap.amino.dot <- (ep2+ep3+ep4+ep5+ep6+ep7)     * pt.atb
      # Sulfamethoxazole-trimethoprim 
      cap.cotri.dot <- (ep5+ep6+ep7)                 * pt.atb
      
      cap.dot.ac <- cap.pen.dot + cap.amino.dot + cap.cotri.dot
      
      # Watch
      # Second-third-gen cephalosporins
      cap.ceph.send.third.dot <- (ep8+ep9) * pt.atb
      
      cap.dot.wa <- cap.ceph.send.third.dot
      
      
      # Hospital Acquired Pneumonia----
      # Low risk of multi-drug resistant infection
      ep10 <-  p.hap * (1-p.highrisk.hap)     # Access (P co-amoxiclav 80-90mg/kg/d)
      # High risk of multi-drug resistant infection
      ep11 <-  p.hap * p.highrisk.hap * 1/3     # Watch  (P cefotaxime 50mg/kg 8h)
      ep12 <-  p.hap * p.highrisk.hap * 1/3     # Watch  (P ceftriaxone 80mg/kg 24h)
      ep13 <-  p.hap * p.highrisk.hap * 1/3    # Watch  (P piperacillin+tazobactam 100mg/kg/d)
      
      #Total probability
      p.tot.hap <- (ep10+ep11+ep12+ep13)/p.hap
      
      # Days of Therapy (DOT) "Access" antibiotics
      # Pen-beta lactamase enzyme
      hap.pen.beta.dot <- ep10 * pt.atb
      
      hap.dot.ac  <- hap.pen.beta.dot
      
      # Days of Therapy (DOT) "Watch" antibiotics
      hap.ceph.sec.third.dot <- (ep11+ep12) * pt.atb
      hap.piper.dot <- ep13 * pt.atb
      
      hap.dot.wa <- (hap.ceph.sec.third.dot + hap.piper.dot)
      
      
      # Bacterial meningitis------
      # First choice 
      ep14 <- p.bmen * p.first.bm * 1/2       # Watch (P cefotaxime 50mg/kg 6h)
      ep15 <- p.bmen * p.first.bm * 1/2       # Watch (P ceftriaxone 80mg/kg 12h)
      # Second choice 
      ep16 <- p.bmen * (1-p.first.bm) * 1/4  # Access (P amoxicillin 50mg/kg 8h) 
      ep17 <- p.bmen * (1-p.first.bm) * 1/4  # Access (P ampicillin 50mg/kg 4h)
      ep18 <- p.bmen * (1-p.first.bm) * 1/4  # Access (P benzylpen 60mg/kg 4h)
      ep19 <- p.bmen * (1-p.first.bm) * 1/4  # Access (P chloramphenicol 25mg/kg 6h)
      
      #Total probability
      p.tot.cns <- (ep14+ep15+ep16+ep17+ep18+ep19)/p.bmen
      
      # Days of Therapy (DOT) "Access" antibiotics
      # Penicillin     
      cns.pen.dot   <- (ep16+ep17+ep18) * pt.atb
      # Chlorampehnicol
      cns.chlor.dot <- ep19 * pt.atb
      
      cns.dot.ac <- cns.pen.dot + cns.chlor.dot
      
      #Days of Therapy (DOT) "Watch" antibiotics
      # Second/Third-gen cephalosporins
      cns.ceph.sec.third.dot <- (ep14+ep15) * pt.atb
      
      cns.dot.wa <- cns.ceph.sec.third.dot
      
      # Intra-abdominal infection-------
      # 1) Acute cholecystitis and cholangitis/ acute appendicitis
      # 2) Pyogenic liver abscess
      # Mild cases
      # First choice 
      ep20 <- p.abd * (1-p.sev.abd) * p.first.abd * 1/3     # Access (Oral amoxicillin 80-90mg/kg/d)
      ep21 <- p.abd * (1-p.sev.abd) * p.first.abd * 1/3     # A+A+A  (P ampi 50mg/kg 8h + P genta 7.5mg/kg/d + O metro 7.5mg/kg/d)
      ep22 <- p.abd * (1-p.sev.abd) * p.first.abd * 1/3     # Wa+Ac  (P cefo/ ceftriaxone 80mg/kg/d + O metro 7.5mg/kg/d)
      # Second choice
      ep23 <- p.abd * (1-p.sev.abd) * (1-p.first.abd)        # Wa+Ac   (O ciproflox 15mg/kg 24h + O metro 7.5mg/kg 8h)
      
      # Severe cases
      # First choice = setting with low ESBL = (1-p.esbl.abd)
      ep24 <- p.abd * p.sev.abd * (1-p.esbl.abd) * 1/2  # A+A+A  (ampi 50mg/kg 8h + genta 7.5mg/kg/d + metro 7.5mg/kg/d)
      ep25 <- p.abd * p.sev.abd * (1-p.esbl.abd) * 1/2  # Watch  (pipercillin+tazobactam 100mg/kg 8h)
      
      # Second choice
      ep26 <- p.abd * p.sev.abd * p.esbl.abd         # Watch (Meropenem 20g/kg 8h)
      
      #Total probability
      p.tot.ia <- (ep20+ep21+ep22+ep23+ep24+ep25+ep26)/p.abd
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Penicillin 
      ia.pen.dot   <- (ep20+ep21+ep24) * pt.atb
      # Aminogylcosides
      ia.amino.dot <- (ep21+ep24) * pt.atb
      # Metronidazole
      ia.metro.dot <- (ep21+ep22+ep23+ep24) * pt.atb
      
      ia.dot.ac  <- ia.pen.dot + ia.amino.dot + ia.metro.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Quinolones
      ia.cipro.dot <- ep23 * pt.atb
      # Pipercillin + tazobactam
      ia.piper.dot <- ep25 * pt.atb
      # Meropenem
      ia.mero.dot  <- ep26 * pt.atb
      
      ia.dot.wa <- ia.cipro.dot + ia.piper.dot + ia.mero.dot
      
      # Upper UTI-----
      # Mild
      ep27 <- p.uuti * (1-p.sev.uti)   # Watch (Oral cipro 15mg/kg 12h)
      # Severe 
      # First choice (Low prevalence of ESBL)
      ep28 <- p.uuti * p.sev.uti * (1-p.esbl.uti) * 1/2   # Watch (P cefotaxime 50mg/kg 8h)
      ep29 <- p.uuti * p.sev.uti * (1-p.esbl.uti) * 1/2   # Watch (P ceftriaxone 80mg/kg 24h)
      # Second choice (High prevalence of ESBL)
      ep30 <- p.uuti * p.sev.uti * p.esbl.uti * 1/2       # Watch + Access (P cefotaxime 50mg/kg 8h + P amikacin 15mg/Kg 24h)
      ep31 <- p.uuti * p.sev.uti * p.esbl.uti * 1/2       # Watch + Access (P ceftriaxone 80mg/kg 24h + P gentamycin 7.5mg/kg 24h)
      
      #Total probability
      p.tot.pye <- (ep27+ep28+ep29+ep30+ep31)/p.uuti
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Aminoglycosides
      pye.amino.dot <- (ep30+ep31) * pt.atb
      
      pye.dot.ac <- pye.amino.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Second/third-gen cephalosporins 
      pye.ceph.sec.third.dot <- (ep28+ep29+ep30+ep31) * pt.atb
      # Ciprofloxacin 
      pye.cipro.dot <- ep27 * pt.atb
      
      pye.dot.wa <- pye.ceph.sec.third.dot + pye.cipro.dot
      
      # Skin and soft tissue infection-----
      # Necrotizing fasciitis
      # MRSA not suspected
      ep32 <- p.sst * p.nf * (1-p.mrsa.sst) * p.Strep.pyogene            # Ac + Wa (P piperacillin+tazobactam 100mg/kg 8h + P clindamycin 10mg/kg 8h) 
      ep33 <- p.sst * p.nf * (1-p.mrsa.sst) * (1- p.Strep.pyogene)       # Ac + Wa (P ceftriaxone 80mg/kg 24h + metro 7.5mg/kg 8h)
      # MRSA susptected 
      ep34 <- p.sst * p.nf * p.mrsa.sst * p.Strep.pyogene       # Ac+Wa+Wa  (P piperacillin+tazobactam 100mg/kg 8h + P clindamycin 10mg/kg 8h+ P vancomycin 15/kg 8h)
      ep35 <- p.sst * p.nf * p.mrsa.sst * (1- p.Strep.pyogene)            # Ac+Wa+Wa  (P ceftriaxone 80mg/kg 24h + P metro 7.5mg/kg 8h + P vancomycin 15/kg 8h)
      
      # Pyomyositis
      ep36 <- p.sst * p.pyomy * 1/3            # Access (P amoxi+clav 50mg/kg 8h)
      ep37 <- p.sst * p.pyomy * 1/3            # Access (O cefalexin 25 mg/kg 12h)
      ep38 <- p.sst * p.pyomy * 1/3            # Access (P cloxacillin 25 mg/kg 6h) 
      
      #Total probability
      p.tot.sst <- (ep32+ep33+ep34+ep35+ep36+ep37+ep38)/p.sst
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Penicillin 
      sst.pen.dot      <- ep38 * pt.atb
      # Penicillin + beta-lactamase
      sst.pen.beta.dot <- ep36 * pt.atb
      # First-gen cephalosporins
      sst.ceph.first.dot <- ep7 * pt.atb
      # Metronidazole
      sst.metro.dot <- (ep33+ep35) * pt.atb
      # Clindamycin 
      sst.clinda.dot <- (ep32+ep34) * pt.atb
      
      sst.dot.ac <- sst.pen.dot + sst.pen.beta.dot + sst.ceph.first.dot +
        sst.metro.dot + sst.clinda.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Second/third-gen cephalosporins
      sst.ceph.sec.third.dot <- (ep33+ep35) * pt.atb 
      # Pipercillin-tazobactam 
      sst.piper.dot <- (ep32+ep34) * pt.atb
      # Vancomycin 
      sst.vanco.dot <- (ep34+ep35) * pt.atb
      
      sst.dot.wa <- sst.ceph.sec.third.dot + sst.piper.dot + sst.vanco.dot
      
      
      # Bone and joint infection----- 
      # 1)Acute bacterial osteomyelitis 2) Septic arthritis
      # First choice
      ep39 <- p.bj * p.first.bj             # Access (P cloxacillin 25mg/kg 6h)
      # Second choice 
      # No MRSA suspected 
      ep40 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Access (P co-amoxiclav 80-90mg/kg/d)
      ep41 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Access (P cefazolin 25/mg/kg 8h)
      ep42 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Watch  (P cefotaxime 50mg/kg 8h)
      ep43 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Watch  (P ceftriaxone 80mg/kg 24h)
      # MRSA suspected 
      ep44 <- p.bj * (1-p.first.bj) * p.mrsa.bj              # Watch  (P vancomycin 15mg/kg/dose 8h)
      
      #Total probability
      p.tot.bj <- (ep39+ep40+ep41+ep42+ep43+ep44)/p.bj
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Penicillin 
      bj.pen.dot        <- ep39 * pt.atb
      # Penicillin- beta-lactamase 
      bj.pen.beta.dot   <- ep40 * pt.atb
      # First-gen cephalosporins
      bj.ceph.first.dot <- ep41 * pt.atb
      
      bj.dot.ac <- bj.pen.dot + bj.pen.beta.dot + bj.ceph.first.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Second/Third-gen cephalosporins
      bj.ceph.sec.third.dot <- ep42+ep43 * pt.atb
      # Vancomycin 
      bj.vanco.dot <- ep44 * pt.atb
      
      bj.dot.wa <- bj.ceph.sec.third.dot + bj.vanco.dot
      
      
      # Clostridioides difficile infection (CDI)-----
      ep45 <- p.clodiff * (1-p.sev.cdf)               #  Access (Oral Metro 7.5mg/kg 8h)
      ep46 <- p.clodiff * p.sev.cdf                   #  Watch  (Oral Vancomycin 5-10 mg/kg 6h)
      
      #Total probability
      p.tot.cd <- (ep45+ep46)/p.clodiff
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Metronidazole 
      cd.metro.dot <- ep45 * pt.atb
      
      cd.dot.ac <- cd.metro.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Vancomycin  
      cd.vanco.dot <- ep46 * pt.atb
      
      cd.dot.wa <- cd.vanco.dot
      
      # Febrile neutropenia-------
      # Low risk (for OPD treatment)
      # ep47 <- p.fneut * p.low    # Ac+ Wa (O amoxi+clav 50-90mg/kg/d + O cipro 15 mg/kg 12h)
      # High risk (Hospitalised patients)
      
      # First choice (setting with low prevalence of ESBL)
      # No suspected gram negative infection, No MRSA suspected
      ep47 <- p.fneut * (1- p.esbl.fn) * (1 - p.g.neg.fn) * (1- p.mrsa.fn)  # Wa (P piperacillin+tazobactam 100mg/kg 8h)
      # Suspected gram negative infection, No MRSA suspected
      ep48 <- p.fneut * (1- p.esbl.fn) * p.g.neg.fn * (1- p.mrsa.fn)        # Wa + Ac (P piperacillin+tazobactam 100mg/kg 8h + P amikacin 15 mg/Kg 24h IV)
      # No suspected gram negative infection, but MRSA suspected 
      ep49 <- p.fneut * (1- p.esbl.fn) * (1 - p.g.neg.fn) * p.mrsa.fn       # Wa + Wa (P piperacillin+tazobactam 100mg/kg 8h + vancomycin 15-20mg/kg 12h IV)
      # Suspected gram negative infection and MRSA 
      ep50 <- p.fneut * (1- p.esbl.fn) * p.g.neg.fn * p.mrsa.fn             # Wa + Ac + Wa (P piperacillin+tazobactam 100mg/kg 8h + P amikacin 15 mg/Kg 24h IV + vancomycin 15-20mg/kg 12h IV)
      
      # Second choice (setting with high prevalence of ESBL)
      # No suspected gram negative infection, No MRSA suspected
      ep51 <- p.fneut * p.esbl.fn * (1 - p.g.neg.fn) * (1- p.mrsa.fn)  # Wa (P meropenem 20mg/kg 8h)
      # Suspected gram negative infection, No MRSA suspected
      ep52 <- p.fneut * p.esbl.fn * p.g.neg.fn * (1- p.mrsa.fn)        # Wa + Ac (P meropenem 20mg/kg 8h + P amikacin 15 mg/Kg 24h IV)
      # No suspected gram negative infection, but MRSA suspected 
      ep53 <- p.fneut * p.esbl.fn * (1 - p.g.neg.fn) * p.mrsa.fn       # Wa + Wa (P meropenem 20mg/kg 8h + vancomycin 15-20mg/kg 12h IV)
      # Suspected gram negative infection and MRSA 
      ep54 <- p.fneut * p.esbl.fn * p.g.neg.fn * p.mrsa.fn             # Wa + Ac + Wa (P meropenem 20mg/kg 8h + P amikacin 15 mg/Kg 24h IV + vancomycin 15-20mg/kg 12h IV)
      
      
      #Total probability
      p.tot.fn <- (ep47+ep48+ep49+ep50+       
                     ep51+ep52+ep53+ep54)/p.fneut
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Aminoglycosides
      fn.amino.dot <- (ep48+ep50+ep52+ep54) * pt.atb
      
      fn.dot.ac <- fn.amino.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Piperacilli-tazobactam 
      fn.piper.dot <- (ep47+ep48+ep49+ep50) * pt.atb
      # Meropenem
      fn.mero.dot  <- (ep51+ep52+ep53+ep54) * pt.atb
      # Vancomycin 
      fn.vanco.dot <- (ep49+ep50+ep53+ep54) * pt.atb
      
      fn.dot.wa <- fn.piper.dot + fn.mero.dot + fn.vanco.dot
      
      
      # Sepsis and septic shock---- 
      # First choice 
      ep55 <- p.sep * p.first * 1/2       # A+A  (P ampi 50mg/kg 8h + P genta 7.5mg/kg/d)
      ep56 <- p.sep * p.first * 1/2       # A+A  (P benzyl pen 30mg/kg 8h + P genta 7.5mg/kg/d)
      # Second choice
      # No ESBL suspected
      ep57 <- p.sep * (1-p.first) * (1-p.esbl) * 1/3   # Watch (P cefotaxime 50mg/kg 8h)
      ep58 <- p.sep * (1-p.first) * (1-p.esbl) * 1/3   # Watch (P ceftriaxone 80mg/kg 24h)
      ep59 <- p.sep * (1-p.first) * (1-p.esbl) * 1/3   # A+A (P cloxacillin 25/mg/kg 6h + P amikacin 15 mg/kg 24h)
      # ESBL suspected
      ep60 <- p.sep * (1-p.first) * p.esbl             # Watch (P piperacillin+tazobactam 100mg/kg 8h)
      
      
      #Total probability
      p.tot.sepsis <- (ep55+ep56+ep57+ep58+ep59+ep60)/p.sep
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Aminoglycosides
      sepsis.amino.dot <- (ep55+ep56+ep59) * pt.atb 
      # Penicillin 
      sepsis.pen.dot   <- (ep55+ep56+ep59) * pt.atb
      
      sepsis.dot.ac    <- sepsis.amino.dot + sepsis.pen.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Second/third-gen cephalosporins
      sepsis.ceph.sec.third.dot <- (ep57+ep58) * pt.atb
      # Piperacillin-tazobactam 
      sepsis.piper.dot <- ep60 * pt.atb
      
      sepsis.dot.wa <- sepsis.ceph.sec.third.dot + sepsis.piper.dot
      
      # Surgical prophylaxis-------
      # Bowel surgery
      ep61 <- p.sug * p.bowsg * p.first.bow.surg      # Access (P cefazolin 50mg/kg + P metro 7.5mg/kg) 
      ep62 <- p.sug * p.bowsg * (1-p.first.bow.surg)  # Access (P amox-clav 50mg/kg)
      # Clean/ clean-contaminated 
      ep63 <- p.sug * p.clean * p.first.clean.cont.surg     # Access (P cefazolin 50mg/kg)
      ep64 <- p.sug * p.clean * (1-p.first.clean.cont.surg) # Watch  (P cefuroxime 50mg/kg)
      # Urological procedure
      ep65 <- p.sug * p.urosg * p.first.uro.surg      # Access (P cefazolin 50mg/kg)
      ep66 <- p.sug * p.urosg * (1-p.first.uro.surg)  # Access (P gentamycin 7.5mg/Kg)
      # Contaminated procedure 
      ep67 <- p.sug * p.contsg * p.first.cont.surg              # Ac + Ac  (P cefazolin 50mg/kg + P metro 7.5 mg/kg)
      ep68 <- p.sug * p.contsg * (1-p.first.cont.surg) * 1/2    # Ac + Ac  (P amox+clav 50 mg/kg + P metro 7.5 mg/kg)
      ep69 <- p.sug * p.contsg * (1-p.first.cont.surg) * 1/2    # Ac + Ac  (P genta 7.5mg/kg + P metro 7.5 mg/kg)
      
      
      #Total probability
      p.tot.sp <- (ep61+ep62+ep63+ep64+ep65+      
                     ep66+ep67+ep68+ep69)/p.sug
      
      # Days of Therapy (DOT) of "Access" antibiotics
      # Penicillin-beta lactamase
      sp.pen.beta.dot <- (ep62+ep68) * pt.atb 
      # Metronidazole 
      sp.metro.dot    <- (ep61+ep67+ep68+ep69) * pt.atb
      # Aminogylcosides
      sp.amino.dot    <- (ep66+ep69) * pt.atb
      # First-gen cephalosporins
      sp.ceph.first.dot <- (ep61+ep63+ep65+ep67) * pt.atb
      
      sp.dot.ac <- sp.pen.beta.dot + sp.metro.dot + sp.amino.dot + sp.ceph.first.dot
      
      # Days of Therapy (DOT) of "Watch" antibiotics
      # Second-gen cephalosporins
      sp.ceph.sec.third.dot <-  ep64 * pt.atb
      
      sp.dot.wa <- sp.ceph.sec.third.dot
      
      
      
      
      # Total Access antibiotic class (DOT)
      pen.dot       <- (cap.pen.dot + cns.pen.dot + ia.pen.dot + sst.pen.dot +
                          bj.pen.dot + sepsis.pen.dot)
      beta.enz.dot  <- (hap.pen.beta.dot + sst.pen.beta.dot + bj.pen.beta.dot + 
                          sp.pen.beta.dot)
      cep.first.dot <- (sst.ceph.first.dot + bj.ceph.first.dot + sp.ceph.first.dot)
      amino.dot     <- (cap.amino.dot + ia.amino.dot + pye.amino.dot +
                          fn.amino.dot+ sepsis.amino.dot + sp.amino.dot)
      imida.dot     <- (ia.metro.dot + sst.metro.dot + cd.metro.dot +
                          sp.metro.dot)
      amphe.dot     <- (cns.chlor.dot)
      linco.dot     <- (sst.clinda.dot)
      sulpha.dot    <- (cap.cotri.dot) 
      
      # Class of Access antibiotic (DDD) per 1000 admitted patients
      
      pen.dot.1000       <- (pen.dot/ pt.admt) * 1000
      beta.enz.dot.1000  <- (beta.enz.dot/ pt.admt) * 1000
      cep.first.dot.1000 <- (cep.first.dot/ pt.admt) * 1000
      amino.dot.1000     <- (amino.dot/ pt.admt) * 1000
      imida.dot.1000     <- (imida.dot/ pt.admt) * 1000
      amphe.dot.1000     <- (amphe.dot/ pt.admt) * 1000
      linco.dot.1000     <- (linco.dot/ pt.admt) * 1000
      sulpha.dot.1000    <- (sulpha.dot/ pt.admt) * 1000
      
      
      # Total Access antibiotic
      total.dot.ac <- (pen.dot + beta.enz.dot + cep.first.dot + amino.dot +
                         imida.dot + amphe.dot + linco.dot + sulpha.dot) 
      
      # Proportion of class of Access antibiotic out of total Access usage
      pen.dot.prop       <- (pen.dot/ total.dot.ac) 
      beta.enz.dot.prop  <- (beta.enz.dot/ total.dot.ac) 
      cep.first.dot.prop <- (cep.first.dot/ total.dot.ac) 
      amino.dot.prop     <- (amino.dot/ total.dot.ac) 
      imida.dot.prop     <- (imida.dot/ total.dot.ac) 
      amphe.dot.prop     <- (amphe.dot/ total.dot.ac) 
      linco.dot.prop     <- (linco.dot/ total.dot.ac) 
      sulpha.dot.prop    <- (sulpha.dot/ total.dot.ac) 
      
      
      
      # Total Watch antibiotic class (DOT)
      cep.second.third.dot <- (hap.ceph.sec.third.dot + cap.ceph.send.third.dot + cns.ceph.sec.third.dot +
                                 pye.ceph.sec.third.dot + sst.ceph.sec.third.dot + bj.ceph.sec.third.dot +
                                 sp.ceph.sec.third.dot)
      piper.dot  <- (hap.piper.dot + ia.piper.dot + sst.piper.dot + fn.piper.dot + sepsis.piper.dot)
      quino.dot <- (ia.cipro.dot + pye.cipro.dot)
      carba.dot <- (ia.mero.dot + fn.mero.dot)
      glyco.dot <- (sst.vanco.dot + bj.vanco.dot + cd.vanco.dot + fn.vanco.dot)
      
      # Class of Watch antibiotic (DDD) per 1000 admitted patients
      cep.second.third.dot.1000 <- (cep.second.third.dot/ pt.admt) * 1000
      piper.dot.1000   <- (piper.dot/ pt.admt) * 1000
      quino.dot.1000   <- (quino.dot/ pt.admt) * 1000
      carba.dot.1000   <- (carba.dot/ pt.admt) * 1000
      glyco.dot.1000   <- (glyco.dot/ pt.admt) * 1000
      
      
      # Total Watch antibiotic
      total.dot.wa <- (cep.second.third.dot + piper.dot +
                         quino.dot + carba.dot + glyco.dot)
      
      # Proportion of class of Watch antibiotic out of total Watch usage
      cep.second.third.dot.prop <- (cep.second.third.dot/ total.dot.wa) 
      piper.dot.prop   <- (piper.dot/ total.dot.wa) 
      quino.dot.prop   <- (quino.dot/ total.dot.wa) 
      carba.dot.prop   <- (carba.dot/ total.dot.wa) 
      glyco.dot.prop   <- (glyco.dot/ total.dot.wa) 
      
      
      
      # Total antibiotics (both Access and Watch)
      total.dot.both <- total.dot.ac + total.dot.wa
      
      
      # DOT (Relative quantity)
      prop.ac.wa <- total.dot.ac/total.dot.wa
      
      # Percentage of Access out of total AMU
      percent.ac <- (total.dot.ac/total.dot.both)*100
      # Percentage of Access out of total AMU
      percent.wa <- (total.dot.wa/total.dot.both)*100
      
      
      # DOT (per 1000 admitted patients)
      dot.tot.1000 <- (total.dot.both/pt.admt)*1000
      dot.ac.1000  <- (total.dot.ac/pt.admt)*1000
      dot.wa.1000  <- (total.dot.wa/pt.admt)*1000 
      
      
      # Labelling output
      names(p.tot.cap)   <- paste("Probability: CAP")
      names(cap.dot.ac)  <- paste("Access antibiotic usage (DOT): CAP")
      names(cap.dot.wa)  <- paste("Watch antibiotic usage (DOT): CAP")
      
      names(p.tot.hap)   <- paste("Probability: HAP")
      names(hap.dot.ac)  <- paste("Access antibiotic usage (DOT): HAP")
      names(hap.dot.wa)  <- paste("Watch antibiotic usage (DOT): HAP")
      
      names(p.tot.cns)   <- paste("Probability: Bacterial meningitis")
      names(cns.dot.ac)  <- paste("Access antibiotic usage (DOT): Bacterial meningitis")
      names(cns.dot.wa)  <- paste("Watch antibiotic usage (DOT): Bacterial meningitis")
      
      names(p.tot.ia)   <- paste("Probability: Intra-abdominal infection")
      names(ia.dot.ac)  <- paste("Access antibiotic usage (DOT): Intra-abdominal infection")
      names(ia.dot.wa)  <- paste("Watch antibiotic usage (DOT): Intra-abdominal infection")
      
      names(p.tot.pye)   <- paste("Probability: Upper UTI")
      names(pye.dot.ac)  <- paste("Access antibiotic usage (DOT): Upper UTI")
      names(pye.dot.wa)  <- paste("Watch antibiotic usage (DOT): Upper UTI")
      
      names(p.tot.sst)   <- paste("Probability: SST")
      names(sst.dot.ac)  <- paste("Access antibiotic usage (DOT): SST")
      names(sst.dot.wa)  <- paste("Watch antibiotic usage (DOT): SST")
      
      names(p.tot.bj)   <- paste("Probability: Bone and joint infection")
      names(bj.dot.ac)  <- paste("Access antibiotic usage (DOT): Bone and joint infection")
      names(bj.dot.wa)  <- paste("Watch antibiotic usage (DOT): Bone and joint infection")
      
      names(p.tot.cd)   <- paste("Probability: C. difficile infection")
      names(cd.dot.ac)  <- paste("Access antibiotic usage (DOT): C. difficile infection")
      names(cd.dot.wa)  <- paste("Watch antibiotic usage (DOT): C. difficile infection")
      
      names(p.tot.fn)   <- paste("Probability: Febrile neutropenia")
      names(fn.dot.ac)  <- paste("Access antibiotic usage (DOT): Febrile neutropenia")
      names(fn.dot.wa)  <- paste("Watch antibiotic usage (DOT): Febrile neutropenia")
      
      names(p.tot.sepsis)   <- paste("Probability: Sepsis")
      names(sepsis.dot.ac)  <- paste("Access antibiotic usage (DOT): Sepsis")
      names(sepsis.dot.wa)  <- paste("Watch antibiotic usage (DOT): Sepsis")
      
      names(p.tot.sp)   <- paste("Probability: Surgical prophylaxis")
      names(sp.dot.ac)  <- paste("Access antibiotic usage (DOT): Surgical prophylaxis")
      names(sp.dot.wa)  <- paste("Watch antibiotic usage (DOT): Surgical prophylaxis")
      
      names(total.dot.both)   <- paste("Total antibiotic usage (DOT)")
      names(total.dot.ac)     <- paste("Total Access antibiotic usage (DOT)")
      names(total.dot.wa)     <- paste("Total Watch antibiotic usage (DOT)")
      
      names(prop.ac.wa)       <- paste("Overall Access:Watch ratio")
      names(percent.ac)       <- paste("Access antibiotics(%)")
      names(percent.wa)       <- paste("Watch antibiotics(%)")
      
      names(dot.tot.1000)     <- paste("Antibiotic usage (DOT) per 1000 admitted patients")
      names(dot.ac.1000)      <- paste("Access antibiotic usage (DOT) per 1000 admitted patients")
      names(dot.wa.1000)      <- paste("Watch antibiotic usage (DOT) per 1000 admitted patients")
      
      names(pen.dot)       <- paste("Penicillins")
      names(beta.enz.dot)  <- paste("Beta-lactam antibiotics plus enzyme inhibitor")
      names(cep.first.dot) <- paste("First generation cephalosporins")
      names(amino.dot)     <- paste("Aminoglycosides")
      names(imida.dot)     <- paste("Nitroimidazoles")
      names(amphe.dot)     <- paste("Amphenicols")
      names(linco.dot)     <- paste("Lincosamides")
      names(sulpha.dot)    <- paste("Sulfonamides")
      
      names(pen.dot.1000)       <- paste("Penicillins per 1000")
      names(beta.enz.dot.1000)  <- paste("Beta-lactam antibiotics plus enzyme inhibitor per 1000")
      names(cep.first.dot.1000) <- paste("First generation cephalosporins per 1000")
      names(amino.dot.1000)     <- paste("Aminoglycosides per 1000")
      names(imida.dot.1000)     <- paste("Nitroimidazoles per 1000")
      names(amphe.dot.1000)     <- paste("Amphenicols per 1000")
      names(linco.dot.1000)     <- paste("Lincosamides per 1000")
      names(sulpha.dot.1000)    <- paste("Sulfonamides per 1000")
      
      names(pen.dot.prop)       <- paste("Penicillins proportion")
      names(beta.enz.dot.prop)  <- paste("Beta-lactam antibiotics plus enzyme inhibitor proportion")
      names(cep.first.dot.prop) <- paste("First generation cephalosporins proportion")
      names(amino.dot.prop)     <- paste("Aminoglycosides proportion")
      names(imida.dot.prop)     <- paste("Nitroimidazoles proportion")
      names(amphe.dot.prop)     <- paste("Amphenicols proportion")
      names(linco.dot.prop)     <- paste("Lincosamides proportion")
      names(sulpha.dot.prop)    <- paste("Sulfonamides proportion")
      
      
      names(cep.second.third.dot) <- paste("Second/ Third generation cephalosporins")
      names(piper.dot)            <- paste("Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal")
      names(quino.dot)            <- paste("Fluroquinolones")
      names(carba.dot)            <- paste("Carbapenems")
      names(glyco.dot)            <- paste("Glycopeptides")
      
      
      names(cep.second.third.dot.1000) <- paste("Second/ Third generation cephalosporins per 1000")
      names(piper.dot.1000)            <- paste("Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal per 1000")
      names(quino.dot.1000)            <- paste("Fluroquinolones per 1000")
      names(carba.dot.1000)            <- paste("Carbapenems per 1000")
      names(glyco.dot.1000)            <- paste("Glycopeptides per 1000")
      
      
      names(cep.second.third.dot.prop) <- paste("Second/ Third generation cephalosporins proportion")
      names(piper.dot.prop)            <- paste("Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal proportion")
      names(quino.dot.prop)            <- paste("Fluroquinolones proportion")
      names(carba.dot.prop)            <- paste("Carbapenems proportion")
      names(glyco.dot.prop)            <- paste("Glycopeptides proportion")
      
      
      # Output------- 
      return(format(round(c(
        
        # Overall expected Access AMU
        total.dot.both, total.dot.ac, total.dot.wa,
        # per 1000 admitted patients
        dot.tot.1000, dot.ac.1000, dot.wa.1000, 
        # % of Access and Watch out of total AMU
        percent.ac, percent.wa,
        # CAP
        cap.dot.ac, cap.dot.wa, 
        # HAP
        hap.dot.ac, hap.dot.wa,
        # CNS (Bacterial meningitis)
        cns.dot.ac, cns.dot.wa, 
        # Intra-abd infection
        ia.dot.ac, ia.dot.wa,
        # Upper UTI (Pyelonephritis)
        pye.dot.ac, pye.dot.wa,
        # Skin and soft-tissue infection
        sst.dot.ac, sst.dot.wa, 
        # Bone and joints infection 
        bj.dot.ac, bj.dot.wa, 
        # Clostridiodes difficle infection 
        cd.dot.ac, cd.dot.wa, 
        # Febrile neutropenia
        fn.dot.ac, fn.dot.wa, 
        # Sepsis
        sepsis.dot.ac, sepsis.dot.wa, 
        # Surgical prophylaxis
        sp.dot.ac, sp.dot.wa,  
        
        
        # Overall expected Access antibiotic class
        pen.dot, beta.enz.dot, cep.first.dot,amino.dot,
        imida.dot, sulpha.dot, amphe.dot, linco.dot,
        
        # Overall expected Watch antibiotic class
        cep.second.third.dot, piper.dot, quino.dot,
        carba.dot, glyco.dot 
      ), 1), nsmall = 1))  # formatting into two decimal places
      
    }
  )
}


# Function to estimate "alpha" and "beta" value from the "point estimate" and "standard error"
binomi.para <- function(point_est, std_err){
  # Estimating alpha value 
  alpha <- point_est * (((point_est * (1 - point_est)) / std_err^2) - 1)
  
  # Estimating beta value 
  beta <- alpha * ((1 - point_est) / point_est)
  
  # Returning both alpha and beta in a list
  return(list(alpha = alpha, beta = beta))
}


