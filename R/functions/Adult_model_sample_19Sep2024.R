

# The file contain a function for
# a decision tree model for adults (sample)
# based on the WHO AWaRe guidelines 
# created by Myo Maung Maung Swe on 19 September 2024
# The sample model only contains "Bacterial meningitis (CNS infection)" and ""

tree_adult <- function(params){
  with(
    as.list(params),
    {
      # Bacterial meningitis (CNS)------
      # First choice antibiotic
      ep1 <- p.bmen * p.first * 1/2       # Watch (P cefotaxime 2g 6h)
      ep2 <- p.bmen * p.first * 1/2       # Watch (P ceftriaxone 2g 12h)
      # Second choice antibiotic
      ep3 <- p.bmen * (1-p.first) * 1/4   # Access (P amoxicillin 2g 4h) 
      ep4 <- p.bmen * (1-p.first) * 1/4   # Access (P ampicillin 2g 4h)
      ep5 <- p.bmen * (1-p.first) * 1/4   # Access (P benzylpen 2.4g 4h)
      ep6 <- p.bmen * (1-p.first) * 1/4   # Access (P chloramphenicol 1g 6h)
      
      
      #Total probability (CNS)
      p.tot.cns <- (ep1+ep2+ep3+ep4+ep5+ep6)/p.bmen
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe's DDD/ WHO's DDD
      # Bacterial meningitis (10 days for unknown pathogens)
      d1 <- (6/4)                # P cefotaxime 4 g
      d2 <- (2/2)                # P ceftriaxone 2 g
      d3 <- (12/3)               # P amoxicillin 3 g
      d4 <- (12/6)               # P ampicillin 6 g
      d5 <- (14.4/3.6)           # P benzylpenicillin 3.6 g
      d6 <- (4/3)                # P chloramphenicol 3 g
      
      # Access 
      # Penicillin
      cns.pen.ddd   <- ((ep3*d3)+(ep4*d4)+(ep5*d5)) * pt.atb
      # Chloramphenicol 
      cns.chlor.ddd <- (ep6*d6) * pt.atb
      
      cns.ddd.ac     <- cns.pen.ddd + cns.chlor.ddd
      cns.ddd.ac.100 <- (cns.ddd.ac * 100)/pt.admt
      
      # Watch
      cns.ceph.sec.third.ddd <- ((ep1*d1)+(ep2*d2)) * pt.atb
      
      cns.ddd.wa     <- cns.ceph.sec.third.ddd
      cns.ddd.wa.100 <- (cns.ddd.wa * 100)/pt.admt
      
      # Upper UTI-----
      # Mild cases 
      ep7 <- p.uuti * (1-p.sev.uti)                  # Watch (O cipro 500 mg 12h)
      # Severe cases 
      # Low risk of ESBL
      ep8 <- p.uuti * p.sev.uti * (1-p.esbl) * 1/2   # Watch (P cefotaxime 1g 8h)
      ep9 <- p.uuti * p.sev.uti * (1-p.esbl) * 1/2   # Watch (P ceftriaxone 1g 24h)
      # High risk of ESBL
      ep10 <- p.uuti * p.sev.uti * p.esbl * 1/2       # Watch + Access (P cefotaxime 1g 8h + P amikacin 15mg/Kg 24h)
      ep11 <- p.uuti * p.sev.uti * p.esbl * 1/2       # Watch + Access (P ceftriaxone 1g 24h + P gentamycin 5mg/kg 24h)
      
      #Total probability
      p.tot.pye <- (ep7+ep8+ep9+ep10+ep11)/p.uuti
      
      # Define Daily Dose(DDD) 
      # Antibiotic use in DDD = AWaRe recommended DDD/ WHO's DDD
      # Upper UTI  (7 days)
      d7  <- (1/1)                  # O ciprofloxacin 1 g 
      d8  <- (3/4)                  # P cefotaxime 4 g 
      d9  <- (1/2)                  # P ceftriaxone 2 g 
      d10a <- (1/1)                 # P amikacin 1 g (15 mg/kg for 65 Kg ~ 1000 mg)
      d10w <- (3/4)                 # P cefotaxime 4 g
      d11a <- (0.32/0.24)           # P gentamycin  0.24 g (5mg/kg for 65 Kg ~ 320 mg)
      d11w <- (1/2)                 # P ceftriaxone 2 g 
      
      # Access
      pye.amino.ddd  <- ((ep10*d10a)+(ep11*d11a)) * pt.atb
      
      pye.ddd.ac     <-  pye.amino.ddd 
      pye.ddd.ac.100 <- (pye.amino.ddd * 100)/pt.admt
      
      # Watch 
      pye.ceph.sec.third.ddd  <- ((ep8*d8)+(ep9*d9)+(ep10*d10w)+(ep11*d11w)) * pt.atb
      pye.cipro.ddd           <- (ep7*d7) * pt.atb
      
      pye.ddd.wa     <- pye.ceph.sec.third.ddd + pye.cipro.ddd
      pye.ddd.wa.100 <- (pye.ddd.wa * 100)/pt.admt
      
      # Class of Access antibiotic (DDD)
      # Penicillins
      pen.ddd       <- (cns.pen.ddd)
      # Aminoglycosides
      amino.ddd     <- (pye.amino.ddd)
      # Amphenicols
      amphe.ddd     <- (cns.chlor.ddd)
      
      # Class of Watch antibiotic (DDD)
      # Second-Third generation cephalosporin
      cep.second.third.ddd      <- (cns.ceph.sec.third.ddd + pye.ceph.sec.third.ddd)
      # Fluroquinolones
      quino.ddd                 <- (pye.cipro.ddd)
      
      # Total Access antibiotic
      total.ddd.ac <- (pen.ddd + amino.ddd) 
      
      # Total Watch antibiotic
      total.ddd.wa <- (cep.second.third.ddd + quino.ddd)
      
      # Total antibiotics
      total.ddd.both <- total.ddd.ac + total.ddd.wa
      
      # Access:Watch (Relative quantity)
      prop.ac.wa <- total.ddd.ac/total.ddd.wa
      
      # Percentage of Access out of total AMU
      percent.ac <- (total.ddd.ac/total.ddd.both)*100
      
      # DDD (per 100 admitted patients)
      ddd.tot.100 <- (total.ddd.both/pt.admt)*100
      ddd.ac.100  <- (total.ddd.ac/pt.admt)*100
      ddd.wa.100  <- (total.ddd.wa/pt.admt)*100 
      
      
      # Labeling output
      names(p.tot.cns)   <- paste("p.tot.cns")
      names(cns.ddd.ac)  <- paste("Expected Access usage (Meningitis)")
      names(cns.ddd.wa)  <- paste("Expected Watch usage (Meningitis)")
      
      names(p.tot.pye)   <- paste("p.tot.pye")
      names(pye.ddd.ac)  <- paste("Expected Access usage (Upper UTI)")
      names(pye.ddd.wa)  <- paste("Expected Watch usage (Upper UTI)")
      
      names(total.ddd.both) <- paste("Total expected antibiotics usage (DDD)")
      names(total.ddd.ac)   <- paste("Total expected (Access) antibiotic usage (DDD)")
      names(total.ddd.wa)   <- paste("Total expected (Watch) antibiotic usage (DDD)")
      
      names(prop.ac.wa)     <- paste("prop.ac.wa")
      names(percent.ac)     <- paste("Expected proportion of Access antibiotics usage (%)")
      
      names(ddd.tot.100)   <- paste("Total expected antibiotics usage per 100 admitted patients (DDD)")
      names(ddd.ac.100)    <- paste("Total expected Access usage per 100 admitted patients (DDD)")
      names(ddd.wa.100)    <- paste("Total expected Watch usage per 100 admitted patients (DDD)")
      
      
      # Output---
      return(format(round(c(
        # Overall expected AMU
        total.ddd.both, total.ddd.ac, total.ddd.wa,
        # % of Access out of total AMU
        percent.ac, 
        # Usage per 100 admitted patients
        ddd.tot.100, ddd.ac.100, ddd.wa.100,
        # CNS (Bacterial meningitis)
        cns.ddd.ac, cns.ddd.wa, 
        # Upper UTI (Pyeonephritis)
        pye.ddd.ac, pye.ddd.wa
      ), 2), nsmall = 2)) # formatting into two decimal places
      
    }
  )
}



# Convert the named vector into a data frame
vector_to_table <- function(named_vector) {
  data.frame(
    Name = names(named_vector),  # Names of the vector as the first column
    Value = named_vector         # Values of the vector as the second column
  )
}


