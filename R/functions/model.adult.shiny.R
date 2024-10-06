

# Decision tree for hospitalized adults (> 12 years)
# Management guideline based on WHO's AWaRe book 
# Decision tree model is built based on 10 clinical infection syndromes (as per the diagnosis mapping)
# Plus surgical prophylaxis
# Detail outputs include DDD for individual antibiotic class as well as "Access"/ "Watch" group
# Duration of treatment not considered in the model 
# AMR (ESBL, MRSA) considered in the model 
# Route of administration (O = Oral, P = Parental) 

model_A <- function(params){
  with(
    as.list(params),
    {
      # CAP----
      # Mild-moderate cases (5 days Oral antibiotics) 
      # First choice 
      ep1 <- p.cap * (1-p.sev.cap) * p.first.cap * 1/2     # Access (Oral amoxicillin 1 g 8h)
      ep2 <- p.cap * (1-p.sev.cap) * p.first.cap * 1/2     # Access (Oral pen V 500 mg 6h)
      # Second choice
      ep3 <- p.cap * (1-p.sev.cap) * (1-p.first.cap) * 1/2 # Access (Oral co-amoxiclav 875 mg 8h)
      ep4 <- p.cap * (1-p.sev.cap) * (1-p.first.cap) * 1/2 # Access (Oral doxy 100 mg 12h)
      
      # Severe cases (5 days)
      # First choice (CURB-65 < 2)
      ep5 <- p.cap * p.sev.cap * p.first.cap * (1- p.curb.high) * 1/2 # Watch (P cefotaxime 2 g 8h)
      ep6 <- p.cap * p.sev.cap * p.first.cap * (1- p.curb.high) * 1/2 # Watch (P ceftriaxone 2 g 24h)
      # First choice (CURB-65 >= 2)
      ep7 <- p.cap * p.sev.cap * p.first.cap * p.curb.high * 1/2 # Watch+ Watch (P cefotaxime 2 g 8h) + O/P clarithromycin 500 mg 12h
      ep8 <- p.cap * p.sev.cap * p.first.cap * p.curb.high * 1/2 # Watch+ Watch (P ceftriaxone 2 g 24h) + O/P clarithromycin 500 mg 12h
      # Second choice (CURB-65 < 2)
      ep9  <- p.cap * p.sev.cap * (1-p.first.cap) * (1- p.curb.high)     # Access (P co-amoxiclav 1 g 8h)
      # Second choice (CURB-65 >= 2)
      ep10 <- p.cap * p.sev.cap * (1-p.first.cap) * p.curb.high # Access + Watch  (P co-amoxiclav 1 g 8h) + O/P clarithromycin 500 mg 12h
      
      # Total probability (CAP)
      p.tot.cap <- (ep1+ep2+ep3+ep4+ep5+ep6+ep7+ep8+ep9+ep10)/p.cap
      
      # Define Daily Dose(DDD) 
      # Community Acquired Pneumonia 
      d1 <- (3/1.5)                 # Access O amoxicillin 1.5 g per day
      d2 <- (2/2)                   # Access O phenoxymethylpenicillin 2 g per day 
      d3 <- ((875*3)/1500)          # Access O amoxicillin 1.5 g per day
      d4 <- (0.2/0.1)               # Access O doxycycline 0.1 g per day 
      
      d5 <- (6/4)                  # Watch P cefotaxime 4 g per day
      d6 <- (2/2)                  # Watch P ceftriaxone 2 g per day 
      d7w1 <- (6/4)                # Watch P cefotaxime 4 g per day + 
      d7w2 <- (1/0.5)              # Watch O clarithromycin 0.5 g per day
      d8w1 <- (2/2)                # Watch P ceftriaxone 2 g per day
      d8w2 <- (1/0.5)              # Watch O clarithromycin 0.5 g per day
      
      d9   <- (3/3)                # Access P amoxicillin + beta-lactamase inhibitor  3 g per day 
      d10a <- (3/3)                # Access P amoxicillin + beta-lactamase inhibitor  3 g per day
      d10w <- (1/0.5)              # Watch O clarithromycin 0.5 g per day
      
      # Individual antibiotics (DDD)
      # Access
      # Penicillin 
      cap.pen.ddd        <- ((ep1*d1)+(ep2*d2))   * pt.atb
      # Penicillin + beta-lactamase inhibitor
      cap.pen.beta.ddd   <- ((ep3*d3)+(ep9*d9)+(ep10*d10a)) * pt.atb
      # Tetracycline
      cap.tetra.ddd      <- (ep4*d4) * pt.atb
      
      cap.ddd.ac     <- cap.pen.ddd + cap.pen.beta.ddd + cap.tetra.ddd
      cap.ddd.ac.100 <- (cap.ddd.ac * 100)/pt.admt
      
      # Watch
      # second/ third generation cephalosporin
      cap.ceph.sec.third.ddd   <- ((ep5*d5)+(ep6*d6)+(ep7*d7w1)+(ep8*d8w1)) * pt.atb
      # Clarithromycin
      cap.clari.ddd      <- ((ep7*d7w2)+(ep8*d8w2)+(ep10*d10w)) * pt.atb
      
      cap.ddd.wa     <- cap.ceph.sec.third.ddd + cap.clari.ddd
      
      
      # HAP----
      # Low-risk MDR infection 
      ep11 <- p.hap * (1-p.highrisk.hap)        # Access (P co-amoxiclav 1 g 8h)
      # High-risk MDR infection 
      ep12 <- p.hap *  p.highrisk.hap * 1/3     # Watch  (P cefotaxime 2 g 8h)
      ep13 <- p.hap *  p.highrisk.hap * 1/3     # Watch  (P ceftriaxone 2 g 24h)
      ep14 <- p.hap *  p.highrisk.hap * 1/3     # Watch  (P piperacillin+tazobactam 4 g 6h)
      
      #Total probability (HAP)
      p.tot.hap <- (ep11+ep12+ep13+ep14)/p.hap
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Hospital Acquired Pneumonia  (7 days)
      d11 <- (3/3)                 # P amoxicillin + beta-lactamase inhibitor  3 g per day
      d12 <- (6/4)                 # P cefotaxime 4 g per day
      d13 <- (2/2)                 # P ceftriaxone 2 g per day     
      d14 <- (16/14)               # P piperacillin + beta-lactamase inhibitor  14 g per day
      
      # Individual antibiotics (DDD)
      # Access
      hap.pen.beta.ddd  <- (ep11*d11) * pt.atb
      
      hap.ddd.ac        <- hap.pen.beta.ddd
      hap.ddd.ac.100    <- (hap.ddd.ac * 100)/pt.admt
      # Watch
      hap.ceph.sec.third.ddd <- ((ep12*d12)+(ep13*d13)) * pt.atb
      hap.piper.ddd  <- (ep14*d14) * pt.atb
      
      hap.ddd.wa     <- hap.ceph.sec.third.ddd + hap.piper.ddd
      
      
      
      # Bacterial meningitis------
      # First choice antibiotic
      ep15 <- p.bmen * p.first.bm * 1/2       # Watch (P cefotaxime 2g 6h)
      ep16 <- p.bmen * p.first.bm * 1/2       # Watch (P ceftriaxone 2g 12h)
      # Second choice antibiotic
      ep17 <- p.bmen * (1-p.first.bm) * 1/4   # Access (P amoxicillin 2g 4h) 
      ep18 <- p.bmen * (1-p.first.bm) * 1/4   # Access (P ampicillin 2g 4h)
      ep19 <- p.bmen * (1-p.first.bm) * 1/4   # Access (P benzylpen 2.4g 4h)
      ep20 <- p.bmen * (1-p.first.bm) * 1/4   # Access (P chloramphenicol 1g 6h)
      
      
      #Total probability (CNS)
      p.tot.cns <- (ep15+ep16+ep17+ep18+ep19+ep20)/p.bmen
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Bacterial meningitis (10 days for unknown pathogens)
      d15 <- (6/4)                # P cefotaxime 4 g
      d16 <- (2/2)                # P ceftriaxone 2 g
      d17 <- (12/3)               # P amoxicillin 3 g
      d18 <- (12/6)               # P ampicillin 6 g
      d19 <- (14.4/3.6)           # P benzylpenicillin 3.6 g
      d20 <- (4/3)                # P chloramphenicol 3 g
      
      # Access 
      # Penicillin
      cns.pen.ddd   <- ((ep17*d17)+(ep18*d18)+(ep19*d19)) * pt.atb
      # Chloramphenicol 
      cns.chlor.ddd <- (ep20*d20) * pt.atb
      
      cns.ddd.ac     <- cns.pen.ddd + cns.chlor.ddd
      cns.ddd.ac.100 <- (cns.ddd.ac * 100)/pt.admt
      
      # Watch
      cns.ceph.sec.third.ddd <- ((ep15*d15)+(ep16*d16)) * pt.atb
      
      cns.ddd.wa     <- cns.ceph.sec.third.ddd
      
      
      
      # Intra-abdominal infection ----
      # 1) Acute cholecystitis and cholangitis, 2) Pyogenic liver abscess, 3) Acute appendicitis 
      # 4) Acute diverticulitis 
      # Mild cases
      # First choice 
      ep21 <- p.abd * (1-p.sev.abd) * p.first.abd * 1/2     # Access (P co-amoxiclav 1 g 8h)
      ep22 <- p.abd * (1-p.sev.abd) * p.first.abd * 1/2     # Watch + Access  (P cefotaxime 2 g 8h/ ceftriaxone 2 g 24r) + O metro 500 mg 8h
      # Second choice
      ep23 <- p.abd * (1-p.sev.abd) * (1-p.first.abd)       # Watch + Access (O ciproflox 500 mg 8h + O metro 500 mg 8h)
      
      # Severe cases
      # First choice = setting with low esbl = (1-p.esbl.abd)
      ep24 <- p.abd * p.sev.abd * (1-p.esbl.abd) * 1/2     # Watch + Access  (P cefotaxime 2 g 8h/ ceftriaxone 2 g 24r) + O metro 500 mg 8h
      ep25 <- p.abd * p.sev.abd * (1-p.esbl.abd) * 1/2     # Watch (P pipercillin+tazobactam 4 g 6h)
      
      # Second choice
      ep26 <- p.abd * p.sev.abd * p.esbl.abd              # Watch (P Meropenem 1g 8h)
      
      #Total probability
      p.tot.ia <- (ep21+ep22+ep23+ep24+ep25+ep26)/p.abd
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Intra-abdominal infection (5 days is adequate in most cases with good clinical recovery and source control)
      d21  <- (3/3)                # P amoxicillin + beta-lactamase inhibitor  3 g per day
      d22w <- (2/2)                # P ceftriaxone 2 g per day
      d22a <- (1.5/1.5)            # P metronidazole 1.5 g per day
      
      d23a <- (1.5/1.5)            # P metronidazole 1.5 g per day
      d23w <- (1.5/0.8)            # P ciprofloxacin 0.8 g per day
      
      d24a <- (1.5/1.5)            # P metronidazole 1.5 g per day
      d24w <- (2/2)                # P ceftriaxone 2 g per day
      d25  <- (16/14)              # P piperacillin + beta-lactamase inhibitor  14 g per day
      d26  <- (3/3)                # P meropenem 3 g 
      
      # Access 
      # Penicillin
      ia.pen.beta.ddd <- (ep21*d21) * pt.atb
      # Metronidazole
      ia.metro.ddd    <- ((ep22*d22a)+(ep23*d23a)+(ep24*d24a)) * pt.atb
      
      ia.ddd.ac      <- ia.pen.beta.ddd + ia.metro.ddd
      ia.ddd.ac.100  <- (ia.ddd.ac * 100)/pt.admt
      
      # Watch 
      ia.ceph.sec.third.ddd <- ((ep22*d22w)+(ep24*d24w)) * pt.atb
      ia.cipro.ddd          <- (ep23*d23w) * pt.atb
      ia.piper.ddd          <- (ep25*d25) * pt.atb
      ia.mero.ddd           <- (ep26*d26) * pt.atb
      
      ia.ddd.wa     <- ia.ceph.sec.third.ddd + ia.cipro.ddd + ia.piper.ddd + ia.mero.ddd
      
      
      # Upper UTI-----
      # Mild cases 
      ep27 <- p.uuti * (1-p.sev.uti)                  # Watch (O cipro 500 mg 12h)
      # Severe cases 
      # Low risk of ESBL
      ep28 <- p.uuti * p.sev.uti * (1-p.esbl.uti) * 1/2   # Watch (P cefotaxime 1g 8h)
      ep29 <- p.uuti * p.sev.uti * (1-p.esbl.uti) * 1/2   # Watch (P ceftriaxone 1g 24h)
      # High risk of ESBL
      ep30 <- p.uuti * p.sev.uti * p.esbl.uti * 1/2       # Watch + Access (P cefotaxime 1g 8h + P amikacin 15mg/Kg 24h)
      ep31 <- p.uuti * p.sev.uti * p.esbl.uti * 1/2       # Watch + Access (P ceftriaxone 1g 24h + P gentamycin 5mg/kg 24h)
      
      #Total probability
      p.tot.pye <- (ep27+ep28+ep29+ep30+ep31)/p.uuti
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Upper UTI  (7 days)
      d27  <- (1/1)                 # O ciprofloxacin 1 g 
      d28  <- (3/4)                 # P cefotaxime 4 g 
      d29  <- (1/2)                 # P ceftriaxone 2 g 
      d30a <- (1/1)                 # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d30w <- (3/4)                 # P cefotaxime 4 g
      d31a <- (0.32/0.24)           # P gentamycin  0.24 g (5mg/kg for 65 Kg ~ 320 mg)
      d31w <- (1/2)                 # P ceftriaxone 2 g 
      
      # Access
      pye.amino.ddd  <- ((ep30*d30a)+(ep31*d31a)) * pt.atb
      
      pye.ddd.ac     <-  pye.amino.ddd 
      pye.ddd.ac.100 <- (pye.amino.ddd * 100)/pt.admt
      
      # Watch 
      pye.ceph.sec.third.ddd  <- ((ep28*d28)+(ep29*d29)+(ep30*d30w)+(ep31*d31w)) * pt.atb
      pye.cipro.ddd           <- (ep27*d27) * pt.atb
      
      pye.ddd.wa     <- pye.ceph.sec.third.ddd + pye.cipro.ddd
      
      
      
      
      # Skin and soft tissue infection---- 
      # 1) Necrotizing fasciitis
      # MRSA not suspected
      ep32 <- p.sst * p.nf * (1-p.mrsa.sst) * p.Strep.pyogene             # Ac + Wa (P piperacillin+tazobactam 4g 6h + P clindamycin 900 mg 8h) 
      ep33 <- p.sst * p.nf * (1-p.mrsa.sst) * (1- p.Strep.pyogene)        # Ac + Wa (P ceftriaxone 2g 24h + P metro 500 mg 8h)
      # MRSA suspected 
      ep34 <- p.sst * p.nf * p.mrsa.sst * p.Strep.pyogene                 # Ac+ Wa+ Wa (P piperacillin+tazobactam 4g 6h + P clindamycin 900 mg 8h+ P vancomycin 15-20/kg)
      ep35 <- p.sst * p.nf * p.mrsa.sst * (1- p.Strep.pyogene)            # Ac+ Wa+ Wa (P ceftriaxone 2g 24h+ P metro 500mg 8h + P vancomycin 15-20/kg)
      
      
      # 2) Pyomyositis
      ep36 <- p.sst * p.pyomy * 1/3            # Access (P amoxi+clav 1g +125 mg 8h)
      ep37 <- p.sst * p.pyomy * 1/3            # Access (O cefalexin 500 mg 8h)
      ep38 <- p.sst * p.pyomy * 1/3            # Access (P cloxacillin 2g 6h) 
      
      #Total probability
      p.tot.sst <- (ep32+ep33+ep34+ep35+ep36+ep37+ep38)/p.sst
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Skin and soft-tissue infection 
      # Necrotizing fasciitis (2-3 weeks)
      # MRSA not suspected
      d32a <- (1.8/1.8)          # P clindamycin 1.8 g 
      d32w <- (16/14)            # P piperacillin + beta-lactamase inhibitor  14 g per day
      d33a <- (1.5/1.5)          # P metronidazole 1.5 g per day
      d33w <- (2/2)              # P ceftriaxone 2 g 
      # MRSA suspected 
      d34a  <- (2.7/1.8)         # P clindamycin 1.8 g
      d34w1 <- (16/14)           # P piperacillin + beta-lactamase inhibitor  14 g per day
      d34w2 <- (1.3/2)           # P vancomycin 2 g (20 mg/kg for 65 Kg ~ 1.3 gm)
      
      d35a  <- (1.5/1.5)         # P metronidazole 1.5 g per day
      d35w1 <- (2/2)             # P ceftriaxone 2 g 
      d35w2 <- (1.3/2)           # P vancomycin 2 g (20 mg/kg for 65 Kg ~ 1.3 gm)
      
      # Pyomyositis (2-3 weeks)
      d36 <- (3/1.5)                # O amoxicillin + beta-lactamase inhibitor  1.5 g per day
      d37 <- (1.5/2)                # O cefalexin 2 g
      d38 <- (2/2)                  # O cloxacillin 2g 
      
      # Access 
      # Clindamycin
      sst.clinda.ddd <- ((ep32*d32a) + (ep34*d34a)) * pt.atb
      # Metro
      sst.metro.ddd  <- ((ep33*d33a) + (ep35*d35a)) * pt.atb
      # Penicillin
      sst.pen.ddd    <- (ep38*d38) * pt.atb
      # Penicillin + beta-lactmase 
      sst.pen.beta.ddd   <- (ep36*d36) * pt.atb
      # First-generation cephalosporin 
      sst.ceph.first.ddd <- (ep37*d37) * pt.atb
      
      sst.ddd.ac <- (sst.clinda.ddd + sst.metro.ddd + sst.pen.ddd + 
                       sst.pen.beta.ddd + sst.ceph.first.ddd)
      sst.ddd.ac.100 <- (sst.ddd.ac * 100)/pt.admt
      
      
      # Watch 
      # Piperacillin 
      sst.piper.ddd  <- ((ep32*d32w) + (ep34*d34w2)) * pt.atb
      # Third.gen.cephalosporin
      sst.ceph.sec.third.ddd <- ((ep33*d33w) + (ep35*d35w1)) * pt.atb
      # Vancomycin
      sst.vanco.ddd <- ((ep34*d34w1)+(ep35*d35w2)) * pt.atb
      
      sst.ddd.wa     <- sst.piper.ddd + sst.ceph.sec.third.ddd + sst.vanco.ddd
      
      
      
      # Bone and joint infections ------
      # 1) Acute bacterial osteomyelitis, 2) Septic arthritis
      # First choice 
      ep39 <- p.bj * p.first.bj             # Access (P cloxacillin 2g 6h)
      # Second choice 
      # No MRSA suspect
      ep40 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Access (P co-amoxiclav 1g 8h)
      ep41 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Access (P cefazolin 2g 8h)
      ep42 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Watch  (P cefotaxime 2g 8h)
      ep43 <- p.bj * (1-p.first.bj) * (1- p.mrsa.bj) * 1/4   # Watch  (P ceftriaxone 2g 24h)
      # MRSA suspect 
      ep44 <- p.bj * (1-p.first.bj) * p.mrsa.bj              # Watch (IV vanomycin 10-20 mg 12hr)
      
      #Total probability
      p.tot.bj <- (ep39+ep40+ep41+ep42+ep43+ep44)/p.bj
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Bone and joint infection (4 - 6 weeks)
      d39 <- (8/2)             # P cloxacillin 2 g  
      d40 <- (3/3)             # P amoxicillin + beta-lactamase inhibitor  3 g per day 
      d41 <- (6/3)             # P cefazolin 3 g    
      d42 <- (6/4)             # P cefotaxime 4 g   
      d43 <- (2/2)             # P ceftriaxone 2 g
      d44 <- (1.3/2)           # P vancomycin 2 g (20 mg/kg for 65 Kg ~ 1.3 gm)
      
      # Access
      # Penicillin 
      bj.pen.ddd        <- (ep39*d39) * pt.atb
      bj.pen.beta.ddd   <- (ep40*d40) * pt.atb
      # First-gen cephalosporins
      bj.ceph.first.ddd <- (ep41*d41) * pt.atb
      
      bj.ddd.ac     <-  bj.pen.ddd + bj.pen.beta.ddd + bj.ceph.first.ddd 
      bj.ddd.ac.100 <-  (bj.ddd.ac * 100)/pt.admt
      
      # Watch 
      # Second and third-gen cephalosporins
      bj.ceph.sec.third.ddd <-  ((ep42*d42)+(ep43*d43)) * pt.atb
      # Vancomycin 
      bj.vanco.ddd <- (ep44*d44) * pt.atb
      
      bj.ddd.wa     <- bj.ceph.sec.third.ddd + bj.vanco.ddd
      
      
      
      # Clostridioides difficile infection (CDI)-----
      ep45 <- p.clodiff * (1-p.sev.cdf)               #  Access (Oral Metro 500 mg 8h)
      ep46 <- p.clodiff * p.sev.cdf                   #  Watch (Oral Vancomycin 125 mg 6h)
      
      #Total probability
      p.tot.cd <- (ep45+ep46)/p.clodiff
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Clostridioides difficile infection (10 days)
      d45 <- (1.5/2)             # Oral metronidazole 2 g
      d46 <- (750/2000)          # Oral vancomycin 2 g 
      
      # Access 
      # Metronidazole
      cd.metro.ddd <- (ep45*d45) * pt.atb
      
      cd.ddd.ac     <-  cd.metro.ddd 
      cd.ddd.ac.100 <-  (cd.ddd.ac * 100)/pt.admt
      
      # Watch 
      cd.vanco.ddd <- (ep46*d46) * pt.atb
      
      cd.ddd.wa     <- cd.vanco.ddd 
      
      
      
      # Febrile neutropenia------
      # Low risk (Out patient setting)
      # High risk (Hospitalised patients)
      # First choice (setting with low prevalence of ESBL)
      # No suspected gram negative infection, No MRSA suspected
      ep47 <- p.fneut * (1- p.esbl.fn) * (1 - p.g.neg.fn) * (1- p.mrsa.fn)  # Wa (P piperacillin+tazobactam 4g 6h)
      # Suspected gram negative infection, No MRSA suspected
      ep48 <- p.fneut * (1- p.esbl.fn) * p.g.neg.fn * (1- p.mrsa.fn)        # Wa + Ac (P piperacillin+tazobactam 4g 6h + P amikacin 15 mg/Kg 24h IV)
      # No suspected gram negative infection, but MRSA suspected 
      ep49 <- p.fneut * (1- p.esbl.fn) * (1 - p.g.neg.fn) * p.mrsa.fn       # Wa + Wa (P piperacillin+tazobactam 4g 6h + vancomycin 15-20mg/kg 12h IV)
      # Suspected gram negative infection and MRSA 
      ep50 <- p.fneut * (1- p.esbl.fn) * p.g.neg.fn * p.mrsa.fn             # Wa + Ac + Wa (P piperacillin+tazobactam 4g 6h + P amikacin 15 mg/Kg 24h IV + vancomycin 15-20mg/kg 12h IV)
      
      # Second choice (setting with high prevalence of ESBL)
      # No suspected gram negative infection, No MRSA suspected
      ep51 <- p.fneut * p.esbl.fn * (1 - p.g.neg.fn) * (1- p.mrsa.fn)  # Wa (P meropenem 1g 8h)
      # Suspected gram negative infection, No MRSA suspected
      ep52 <- p.fneut * p.esbl.fn * p.g.neg.fn * (1- p.mrsa.fn)        # Wa + Ac (P meropenem 1g 8h + P amikacin 15 mg/Kg 24h IV)
      # No suspected gram negative infection, but MRSA suspected 
      ep53 <- p.fneut * p.esbl.fn * (1 - p.g.neg.fn) * p.mrsa.fn       # Wa + Wa (P meropenem 1g 8h + vancomycin 15-20mg/kg 12h IV)
      # Suspected gram negative infection and MRSA 
      ep54 <- p.fneut * p.esbl.fn * p.g.neg.fn * p.mrsa.fn             # Wa + Ac + Wa (P meropenem 1g 8h + P amikacin 15 mg/Kg 24h IV + vancomycin 15-20mg/kg 12h IV)
      
      #Total probability
      p.tot.fn <- (ep47+ep48+ep49+ep50+ep51+ep52+ep53+ep54)/p.fneut
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Febrile neutropenia
      # Low risk (7 days) Tx in outpatient setting
      # High risk  (until clinical signs of infection resolved and no fever 48 hr)
      # Assumed 2 weeks ( 14 days)
      # First choice (low ESBL)
      d47   <- (16/14)               # P piperacillin + beta-lactamase inhibitor  14 g per day
      d48w  <- (16/14)               # P piperacillin + beta-lactamase inhibitor  14 g per day
      d48a  <- (1/1)                 # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d49w1 <- (16/14)               # P piperacillin + beta-lactamase inhibitor  14 g per day
      d49w2 <- (2.6/2)               # P vancomycin 2 g (20 mg/Kg for 65 12 hr ~ 2.6 g daily)
      d50w1 <- (16/14)               # P piperacillin + beta-lactamase inhibitor  14 g per day
      d50a  <- (1/1)                 # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d50w2 <- (2.6/2)               # P vancomycin 2 g (20 mg/Kg for 65 12 hr ~ 2.6 g daily)
      
      # Second choice (high prevalence of ESBL)
      d51   <- (6/3)                  # P meropenem 3 g
      d52a  <- (1/1)                  # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d52w  <- (6/3)                  # P meropenem 3 g                  
      d53w1 <- (6/3)                  # P meropenem 3 g
      d53w2 <- (2.6/2)                # P vancomycin 2 g (20 mg/Kg for 65 12 hr ~ 2.6 g daily)
      d54a  <- (1/1)                  # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d54w1 <- (3/3)                  # P meropenem 3 g
      d54w2 <- (2.6/2)                # P vancomycin 2 g (20 mg/Kg for 65 12 hr ~ 2.6 g daily)
      
      # Access 
      # Aminoglycosides
      fn.amino.ddd   <- ((ep48*d48a)+(ep50*d50a)+(ep52*d52a)+(ep54*d54a)) * pt.atb
      
      fn.ddd.ac     <- fn.amino.ddd
      fn.ddd.ac.100 <- (fn.ddd.ac * 100)/pt.admt
      
      # Watch 
      # Piperacillin-beta-lactamase inhibitor
      fn.piper.ddd  <- ((ep47*d47)+(ep48*d48w)+(ep49*d49w1)+(ep50*d50w1)) * pt.atb
      # Meropenem
      fn.mero.ddd   <- ((ep51*d51)+(ep52*d52w)+(ep53*d53w1)+(ep54*d54w1)) * pt.atb
      # Vancomycin
      fn.vanco.ddd  <- ((ep49*d49w2)+(ep50*d50w2)+(ep53*d53w2)+(ep54*d54w2)) * pt.atb
      
      fn.ddd.wa     <- fn.piper.ddd + fn.mero.ddd + fn.vanco.ddd
      
      # Sepsis and septic shock---- 
      # Underlying cause 
      # Unknown origin 
      ep55 <- p.sep * p.unk * 1/4   # Access + Watch (P cefotaxime 2g 8h + P amikacin 15 mg/kg 24h)
      ep56 <- p.sep * p.unk * 1/4   # Access + Watch (P cefotaxime 2g 8h + P gentamicin 5 mg/kg 24h)
      ep57 <- p.sep * p.unk * 1/4   # Access + Watch (P ceftriaxone 2g 24h + P amikacin 15 mg/kg 24h)
      ep58 <- p.sep * p.unk * 1/4   # Access + Watch (P ceftriaxone 2g 24h + P gentamicin 5 mg/kg 24h)
      
      # Meningitis
      # First choice
      ep59 <- p.sep * p.men * p.first.bm * 1/2       # Watch (P cefotaxime 2g 6h)
      ep60 <- p.sep * p.men * p.first.bm * 1/2       # Watch (P ceftriaxone 2g 12h)
      # Second choice
      ep61 <- p.sep * p.men * (1-p.first.bm) * 1/4   # Access (P amoxicillin 2g 4h) 
      ep62 <- p.sep * p.men * (1-p.first.bm) * 1/4   # Access (P ampicillin 2g 4h)
      ep63 <- p.sep * p.men * (1-p.first.bm) * 1/4   # Access (P benzylpen 2.4g 4h)
      ep64 <- p.sep * p.men * (1-p.first.bm) * 1/4   # Access (P chloramphenicol 1g 6h)
      
      # LRTI 
      ep65 <- p.sep * p.lrti * 1/2    # Watch + Watch (P cefotaxime 2g 8h + P clarithromycin 500mg 12h) 
      ep66 <- p.sep * p.lrti * 1/2    # Watch + Watch (P ceftriaxone 2g 24h + P clarithromycin 500mg 12h)
      
      # Enteric fever 
      ep67 <- p.sep * p.ent           # Watch (P ceftriaxone 2g 24h)
      
      # Intra-abdominal infection 
      # First choice (low ESBL)
      ep68 <- p.sep * p.abd.s * (1-p.esbl.abd) * 1/3            # Access+Watch (P cefotaxime 2g 8h + P metro 500mg 8h)
      ep69 <- p.sep * p.abd.s * (1-p.esbl.abd) * 1/3            # Access+Watch (P ceftriaxone 2g 24h + P metro 500mg 8h)
      ep70 <- p.sep * p.abd.s * (1-p.esbl.abd) * 1/3            # Watch (P pipercillin + tazobactam 4 g)
      # Second choice (high ESBL)
      ep71 <- p.sep * p.abd.s * p.esbl.abd                       # Watch (P meropenem 2g 8h)
      
      # Skin and soft tissue infection 
      # If MRSA not suspected 
      # Confirmed/ suspected necrotizing fasciitis
      ep72 <- p.sep * p.sst.s * (1-p.mrsa.sst) * p.snf       # Access+Watch (P piperacillin+tazobactam 4g 6h + P clindamycin 900 mg 8h)
      # Otherwise
      ep73 <- p.sep * p.sst.s * (1-p.mrsa.sst) * (1-p.snf)   # Access+Watch (P ceftriaxone 2g 24h + P metro 500 mg8h)
      # MRSA suspected 
      # Confirmed/ suspected necrotizing fasciitis
      ep74 <- p.sep * p.sst.s * p.mrsa.sst * p.snf           # Ac+Wa+Wa  (P piperacillin+tazobactam 4g 6h + P clindamycin 900 mg 8h+ vancomycin 15-20/kg)
      # Otherwise
      ep75 <- p.sep * p.sst.s * p.mrsa.sst * (1-p.snf)       # Ac+Wa+Wa  (P ceftriaxone 2g 24h+ metro 500mg 8h + P vancomycin 15-20/kg)
      
      # Urinary tract infection 
      ep76 <- p.sep * p.uti.s * 1/2    # Ac+Wa (P cefotaxime 2g 8h + P amikacin 15mg/kg)
      ep77 <- p.sep * p.uti.s * 1/2    # Ac+Wa (P ceftriaxone 2g 24h + P amikacin 15 mg/kg)
      
      
      #Total probability
      p.tot.sepsis <- (ep55+ep56+ep57+ep58+ep59+ep60+
                         ep61+ep62+ep63+ep64+ep65+ep66+ep67+ep68+ep69+
                         ep70+ep71+ep72+ep73+ep74+ep75+ep76+ep77)/p.sep
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Sepsis and septic shock 
      # Unknown origin  (7 days)
      d55a <- (1/1)                  # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)    
      d55w <- (6/4)                  # P cefotaxime 4 g
      d56a <- (0.32/0.24)            # P gentamycin  0.24 g (5mg/kg for 65 Kg ~ 320 mg)
      d56w <- (6/4)                  # P cefotaxime 4 g
      d57a <- (1/1)                  # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d57w <- (2/2)                  # P ceftriaxone 2 g
      d58a <- (0.32/0.24)            # P gentamycin  0.24 g (5mg/kg for 65 Kg ~ 320 mg)
      d58w <- (2/2)                  # P ceftriaxone 2 g
      
      # Meningitis (10 days)
      d59 <- (6/4)                 # P cefotaxime 4 g
      d60 <- (2/2)                 # P ceftriaxone 2 g
      d61 <- (12/3)                # P amoxicillin 3 g
      d62 <- (12/6)                # P ampicillin 6 g 
      d63 <- (14.4/3.6)            # P benzylpenicillin 3.6 g
      d64 <- (4/3)                 # P chloramphenicol 3 g 
      
      # LRTI (5 days)
      d65w1 <- (6/4)             # P cefotaxime 4 g
      d65w2 <- (1/1)             # P clarithromycin 1 g
      d66w1 <- (2/2)             # P ceftriaxone 2 g
      d66w2 <- (1/1)             # P clarithromycin 1 g
      
      # Enteric fever (10 days)
      d67 <- (2/2)               # P ceftriaxone 2 g
      
      # Intra-abdominal infection (generally 7 days)
      d68a <- (1.5/1.5)          # P metronidazole 1.5 g per day
      d68w <- (6/4)              # P cefotaxime 4 g
      d69a <- (1.5/1.5)          # P metronidazole 1.5 g per day
      d69w <- (2/2)              # P ceftriaxone 2 g
      d70  <- (16/14)            # P piperacillin + beta-lactamase inhibitor  14 g per day
      d71  <- (6/3)              # P meropenem 3 g
      
      # Skin and soft tissue infection (generally 7 days)
      # If MRSA not suspected 
      # Confirmed/ suspected necrotizing fasciitis
      d72a <- (2.7/1.8)           # P clindamycin 1.8 g 
      d72w <- (16/14)             # P piperacillin + beta-lactamase inhibitor  14 g per day
      d73a <- (1.5/1.5)           # P metronidazole 1.5 g per day
      d73w <- (2/2)               # P ceftriaxone 2 g
      
      # MRSA suspected
      d74a  <- (2.7/1.8)          # P clindamycin 1.8 g
      d74w1 <- (16/14)            # P piperacillin + beta-lactamase inhibitor  14 g per day 
      d74w2 <- (1.3/2)            # P vancomycin 2 g (20 mg/kg for 65 Kg ~ 1.3 gm)
      d75a  <- (1.5/1.5)          # P metronidazole 1.5 g per day
      d75w1 <- (2/2)              # P ceftriaxone 2 g
      d75w2 <- (1.3/2)            # P vancomycin 2 g (20 mg/kg for 65 Kg ~ 1.3 gm)
      
      # Urinary tract infection (7 days)
      d76a <- (1/1)              # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d76w <- (6/4)              # P cefotaxime 4 g
      d77a <- (1/1)              # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d77w <- (2/2)              # P ceftriaxone 2 g
      
      # Access antibiotic
      # Penicillin 
      sepsis.pen.ddd   <- ((ep61*d61)+(ep62*d62)+(ep63*d63)) * pt.atb
      # Aminoglycosides
      sepsis.amino.ddd <- ((ep55*d55a)+(ep56*d56a)+(ep57*d57a)+
                             (ep58*d58a)+(ep76*d76a)+(ep77*d77a)) * pt.atb
      # Chloramphenicol
      sepsis.chlor.ddd <- (ep64*d64) * pt.atb
      # Metronidazole
      sepsis.metro.ddd <- ((ep68*d68a)+(ep69*d69a)+(ep73*d73a)+(ep75*d75a)) * pt.atb
      # Clindamycin 
      sepsis.clinda.ddd <- ((ep72*d72a)+(ep74*d74a)) * pt.atb
      
      sepsis.ddd.ac <- (sepsis.pen.ddd + sepsis.amino.ddd + sepsis.chlor.ddd +
                          sepsis.metro.ddd + sepsis.clinda.ddd) 
      
      sepsis.ddd.ac.100 <- (sepsis.ddd.ac * 100)/pt.admt
      
      # Watch antibiotic
      # Second/ third-gen cephalosporins
      sepsis.ceph.sec.third.ddd <- ((ep55*d55w)+(ep56*d56w)+(ep57*d57w)+(ep58*d58w) +
                                      (ep59*d59) +(ep60*d60) +(ep65*d65w1)+(ep66*d66w1)+
                                      (ep67*d67) +(ep68*d68w)+(ep69*d69w) +(ep73*d73w) +
                                      (ep75*d75w1)+(ep76*d76w)+(ep77*d77w)) * pt.atb
      # Piperacillin-beta-lactam antibiotic
      sepsis.piper.ddd <- ((ep70*d70) + (ep72*d72w) + (ep74*d74w1)) * pt.atb
      # Meropenem
      sepsis.mero.ddd  <- (ep71*d71) * pt.atb
      # Vancomycin 
      sepsis.vanco.ddd <- ((ep74*d74w2)+(ep75*d75w2)) * pt.atb
      # Clarithromycin
      sepsis.clari.ddd <- ((ep65*d65w2)+(ep66*d66w2)) * pt.atb
      
      sepsis.ddd.wa     <- (sepsis.ceph.sec.third.ddd + sepsis.piper.ddd + sepsis.mero.ddd +
                              sepsis.vanco.ddd + sepsis.clari.ddd)
      
      # Surgical prophylaxis-------
      # Bowel surgery 
      ep78 <- p.sug * p.bowsg * p.first.bow.surg        # Access + Access (P cefazolin 2g + P metro 500 mg) 
      ep79 <- p.sug * p.bowsg * (1-p.first.bow.surg)    # Access (P amox-clav 2g + 200 mg)
      # Clean/ clean-contaminated 
      ep80 <- p.sug * p.clean * p.first.clean.cont.surg        # Access (P cefazolin 2g)
      ep81 <- p.sug * p.clean * (1-p.first.clean.cont.surg)    # Watch  (P cefuroxime 1.5g)
      # Urological procedure 
      ep82 <- p.sug * p.urosg * p.first.uro.surg        # Access (P cefazolin 2g)
      ep83 <- p.sug * p.urosg * (1-p.first.uro.surg)    # Access (P gentamycin 5mg/Kg)
      # Contaminated procedure 
      ep84 <- p.sug * p.contsg * p.first.cont.surg              # Access + Access (P cefazolin 2g + P metro 500 mg)
      ep85 <- p.sug * p.contsg * (1-p.first.cont.surg) * 1/2    # Access + Access (P amox+clav 2g + P metro 500 mg)
      ep86 <- p.sug * p.contsg * (1-p.first.cont.surg) * 1/2    # Access + Access (P genta 5mg/kg + P metro 500 mg)
      
      #Total probability
      p.tot.sp <- (ep78+ep79+ep80+ep81+ep82+
                     ep83+ep84+ep85+ep86)/p.sug
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Surgical prophylaxis (A single dose)
      # Bowel surgery 
      d78a  <- 2/3         # P cefazoline 3 g 
      d78a1 <- 0.5/1.5     # P metronidazole 1.5 g 
      d79   <- 2/3         # P amoxicillin + beta-lactamase inhibitor  3 g per day
      # Clean/ clean-contaminated
      d80 <- 2/3           # P cefazoline 3 g 
      d81 <- 1.5/3         # P cefuroxime 3 g
      # Urological procedure 
      d82 <- 2/3           # P cefazoline 3 g
      d83 <- 0.32/0.24     # P gentamycin  0.24 g (5mg/kg for 65 Kg ~ 320 mg)
      # Contaminated procedure
      d84a   <- 2/3          # P cefazoline 3 g
      d84a1  <- 0.5/1.5      # P metronidazole 1.5 g
      d85a   <- 2/3          # P amoxicillin + beta-lactamase inhibitor  3 g per day
      d85a1  <- 0.5/1.5      # P metronidazole 1.5 g
      d86a   <- 0.32/0.24    # P gentamycin  0.24 g (5mg/kg for 65 Kg ~ 320 mg)
      d86a1  <- 0.5/1.5      # P metronidazole 1.5 g
      
      # Access
      # First-gen cephalosporins
      sp.ceph.first.ddd <- ((ep78*d78a)+(ep80*d80)+(ep82*d82)+(ep84*d84a)) * pt.atb
      # Metronidazole
      sp.metro.ddd    <- ((ep78*d78a1)+(ep84*d84a1)+(ep85*d85a1)+(ep86*d86a1)) * pt.atb
      # Aminoglycosides
      sp.amino.ddd    <- ((ep83*d83)+(ep86*d86a)) * pt.atb
      # Penicillin-beta-lactamase 
      sp.pen.beta.ddd <- (ep79*d79)+(ep85*d85a)
      
      sp.ddd.ac     <- sp.ceph.first.ddd + sp.metro.ddd + sp.amino.ddd + sp.pen.beta.ddd
      
      
      # Watch
      # Second/third-gen cephalosporins
      sp.ceph.sec.third.ddd <- (ep81*d81) * pt.atb
      
      sp.ddd.wa     <- sp.ceph.sec.third.ddd
      
      
      # Class of Access antibiotic (DDD)
      pen.ddd       <- (cap.pen.ddd+cns.pen.ddd+sst.pen.ddd+bj.pen.ddd+sepsis.pen.ddd)
      beta.enz.ddd  <- (cap.pen.beta.ddd+hap.pen.beta.ddd+ia.pen.beta.ddd+sst.pen.beta.ddd+
                          bj.pen.beta.ddd+sp.pen.beta.ddd)
      cep.first.ddd <- (sst.ceph.first.ddd+bj.ceph.first.ddd+sp.ceph.first.ddd)
      amino.ddd     <- (pye.amino.ddd+fn.amino.ddd+sepsis.amino.ddd+sp.amino.ddd)
      imida.ddd     <- (ia.metro.ddd+sst.metro.ddd+cd.metro.ddd+sepsis.metro.ddd+sp.metro.ddd)
      tetra.ddd     <- (cap.tetra.ddd)
      amphe.ddd     <- (cns.chlor.ddd+sepsis.chlor.ddd)
      linco.ddd     <- (sst.clinda.ddd+sepsis.clinda.ddd)
      
      
      # Class of Watch antibiotic (DDD)
      cep.second.third.ddd      <-  (cap.ceph.sec.third.ddd+hap.ceph.sec.third.ddd+cns.ceph.sec.third.ddd+
                                       ia.ceph.sec.third.ddd+pye.ceph.sec.third.ddd+sst.ceph.sec.third.ddd+
                                       bj.ceph.sec.third.ddd+sepsis.ceph.sec.third.ddd+sp.ceph.sec.third.ddd)
      
      macro.ddd           <-  cap.clari.ddd+sepsis.clari.ddd 
      beta.enz.pseudo.ddd <-  (hap.piper.ddd+ia.piper.ddd+sst.piper.ddd+
                                 fn.piper.ddd+sepsis.piper.ddd)
      quino.ddd           <-  (ia.cipro.ddd+pye.cipro.ddd)
      carba.ddd           <-  (ia.mero.ddd+fn.mero.ddd+sepsis.mero.ddd)
      glyco.ddd           <-  (sst.vanco.ddd+bj.vanco.ddd+cd.vanco.ddd+fn.vanco.ddd+
                                 sepsis.vanco.ddd)
      
      
      
      # Total Access antibiotic
      total.ddd.ac <- (pen.ddd + beta.enz.ddd + cep.first.ddd + amino.ddd +
                         imida.ddd + tetra.ddd + amphe.ddd + linco.ddd) 
      
      # Total Watch antibiotic
      total.ddd.wa <- (cep.second.third.ddd + macro.ddd + beta.enz.pseudo.ddd +
                         quino.ddd + carba.ddd + glyco.ddd)
      
      # Total antibiotics
      total.ddd.both <- total.ddd.ac + total.ddd.wa
      
      # Access:Watch (Relative quantity)
      prop.ac.wa <- total.ddd.ac/total.ddd.wa
      
      # Percentage of Access out of total AMU
      percent.ac <- (total.ddd.ac/total.ddd.both)*100
      
      
      # DDD (per 1000 admitted patients)
      ddd.tot.1000 <- (total.ddd.both/pt.admt)*1000
      ddd.ac.1000  <- (total.ddd.ac/pt.admt)*1000
      ddd.wa.1000  <- (total.ddd.wa/pt.admt)*1000 
      
      
      # Labeling output
      names(p.tot.cap)   <- paste("Probability: CAP")
      names(cap.ddd.ac)  <- paste("Access antibiotic usage (DDD): CAP")
      names(cap.ddd.wa)  <- paste("Watch antibiotic usage (DDD): CAP")
      
      names(p.tot.hap)   <- paste("Probability: HAP(non-VAP)")
      names(hap.ddd.ac)  <- paste("Access antibiotic usage (DDD): HAP(non-VAP)")
      names(hap.ddd.wa)  <- paste("Watch antibiotic usage (DDD): HAP(non-VAP)")
      
      names(p.tot.cns)   <- paste("Probability: Bacterial meningitis")
      names(cns.ddd.ac)  <- paste("Access antibiotic usage (DDD): Bacterial meningitis")
      names(cns.ddd.wa)  <- paste("Watch antibiotic usage (DDD): Bacterial meningitis")
      
      names(p.tot.ia)   <- paste("Probability: Intra-abdominal infection")
      names(ia.ddd.ac)  <- paste("Access antibiotic usage (DDD): Intra-abdominal infection")
      names(ia.ddd.wa)  <- paste("Watch antibiotic usage (DDD): Intra-abdominal infection")
      
      names(p.tot.pye)   <- paste("Probability: Upper UTI")
      names(pye.ddd.ac)  <- paste("Access antibiotic usage (DDD): Upper UTI")
      names(pye.ddd.wa)  <- paste("Watch antibiotic usage (DDD): Upper UTI")
      
      names(p.tot.sst)   <- paste("Probability: SST")
      names(sst.ddd.ac)  <- paste("Access antibiotic usage (DDD): SST")
      names(sst.ddd.wa)  <- paste("Watch antibiotic usage (DDD): SST")
      
      names(p.tot.bj)   <- paste("Probability: Bone and joint infection")
      names(bj.ddd.ac)  <- paste("Access antibiotic usage (DDD): Bone and joint infection")
      names(bj.ddd.wa)  <- paste("Watch antibiotic usage (DDD): Bone and joint infection")
      
      names(p.tot.cd)   <- paste("Probability: C. difficile infection")
      names(cd.ddd.ac)  <- paste("Access antibiotic usage (DDD): C. difficile infection")
      names(cd.ddd.wa)  <- paste("Watch antibiotic usage (DDD): C. difficile infection")
      
      names(p.tot.fn)   <- paste("Probability: Febrile neutropenia")
      names(fn.ddd.ac)  <- paste("Access antibiotic usage (DDD): Febrile neutropenia")
      names(fn.ddd.wa)  <- paste("Watch antibiotic usage (DDD): Febrile neutropenia")
      
      names(p.tot.sepsis)   <- paste("Probability: Sepsis")
      names(sepsis.ddd.ac)  <- paste("Access antibiotic usage (DDD): Sepsis")
      names(sepsis.ddd.wa)  <- paste("Watch antibiotic usage (DDD): Sepsis")
      
      names(p.tot.sp)   <- paste("Probability: Surgical prophylaxis")
      names(sp.ddd.ac)  <- paste("Access antibiotic usage (DDD): Surgical prophylaxis")
      names(sp.ddd.wa)  <- paste("Watch antibiotic usage (DDD): Surgical prophylaxis")
      
      names(total.ddd.both)   <- paste("Total antibiotic usage (DDD)")
      names(total.ddd.ac)     <- paste("Total Access antibiotic usage (DDD)")
      names(total.ddd.wa)     <- paste("Total Watch antibiotic usage (DDD)")
      
      names(prop.ac.wa)       <- paste("Overall Access:Watch ratio")
      names(percent.ac)       <- paste("Percentage of Access antibiotic usage")
      
      names(ddd.tot.1000)     <- paste("Antibiotic usage (DDD) per 1000 admitted patients")
      names(ddd.ac.1000)      <- paste("Access antibiotic usage (DDD) per 1000 admitted patients")
      names(ddd.wa.1000)      <- paste("Watch antibiotic usage (DDD) per 1000 admitted patients")
      
      names(pen.ddd)       <- paste("Penicillins (DDD)")
      names(beta.enz.ddd)  <- paste("Beta-lactam antibiotics plus enzyme inhibitor (DDD)")
      names(cep.first.ddd) <- paste("First generation cephalosporins (DDD)")
      names(amino.ddd)     <- paste("Aminoglycosides (DDD)")
      names(imida.ddd)     <- paste("Nitroimidazoles (DDD)")
      names(tetra.ddd)     <- paste("Tetracyclines (DDD)")
      names(amphe.ddd)     <- paste("Amphenicols (DDD)")
      names(linco.ddd)     <- paste("Lincosamides (DDD)")
      
      names(cep.second.third.ddd) <- paste("Second/ Third generation cephalosporins (DDD)")
      names(macro.ddd)            <- paste("Macrolides (DDD)")
      names(beta.enz.pseudo.ddd)  <- paste("Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal (DDD)")
      names(quino.ddd)            <- paste("Fluroquinolones (DDD)")
      names(carba.ddd)            <- paste("Carbapenems (DDD)")
      names(glyco.ddd)            <- paste("Glycopeptides (DDD)")
      
      # Output-----
      return(format(round(c(
        
        # Overall expected AMU
        total.ddd.both, total.ddd.ac, total.ddd.wa,
        # AMU per 1000 admitted patients
        ddd.tot.1000, ddd.ac.1000, ddd.wa.1000,
        # Overall Access to Watch ratio
        prop.ac.wa, 
        # % of Access out of total AMU
        percent.ac, 
        # CAP
        cap.ddd.ac, cap.ddd.wa, 
        # HAP
        hap.ddd.ac, hap.ddd.wa,
        # CNS (Bacterial meningitis)
        cns.ddd.ac, cns.ddd.wa, 
        # Intra-abd infection
        ia.ddd.ac, ia.ddd.wa,
        # Upper UTI (Pyeonephritis)
        pye.ddd.ac, pye.ddd.wa,
        # Skin and soft-tissue infection
        sst.ddd.ac, sst.ddd.wa, 
        # Bone and joints infection 
        bj.ddd.ac, bj.ddd.wa, 
        # Clostridiodes difficle infection 
        cd.ddd.ac, cd.ddd.wa, 
        # Febrile neutropenia
        fn.ddd.ac, fn.ddd.wa, 
        # Sepsis
        sepsis.ddd.ac, sepsis.ddd.wa, 
        # Surgical prophylaxis
        sp.ddd.ac, sp.ddd.wa,  
        
        # Overall expected Access antibiotic class
        pen.ddd, beta.enz.ddd, cep.first.ddd,amino.ddd,
        imida.ddd, tetra.ddd, amphe.ddd, linco.ddd,
        # Overall expected Watch antibiotic class
        cep.second.third.ddd, macro.ddd,
        beta.enz.pseudo.ddd, quino.ddd,
        carba.ddd, glyco.ddd
      ), 1), nsmall = 1))  # formatting into one decimal places
      
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

