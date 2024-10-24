#!!!
# Load input parameters into the fields
load_disease_inputs <- function(params, session) {
  
  # Sidebar item for "Type of Patients" 1
  updateNumericInput(session, "cap_cases", value = unname(params["Patients with community acquired pneumonia (CAP)"]))
  updateNumericInput(session, "bm_cases", value = unname(params["Patients with bacterial meningitis"]))
  updateNumericInput(session, "bji_cases", value = unname(params["Patients with bone and joint infections (BJ)"]))
  updateNumericInput(session, "uut_cases", value = unname(params["Patients with upper UTI"]))
  updateNumericInput(session, "sepsis_cases", value = unname(params["Patients with sepsis/ septic shock (SEPSIS)"]))
  updateNumericInput(session, "cdif_cases", value = unname(params["Patients with Clostridioides difficile infection (CDIF)"]))
  
  # Sidebar item for "Type of Patients" 2
  updateNumericInput(session, "hap_cases", value = unname(params["Patients with hospital acquired pneumonia (HAP) Non-VAP"]))
  updateNumericInput(session, "ia_cases", value = unname(params["Patients with intra-abdominal infection (IA)"]))
  updateNumericInput(session, "sst_cases", value = unname(params["Patients with skin and soft-tissue infection (SST)"]))
  updateNumericInput(session, "fn_cases", value = unname(params["Patients with febrile neutropenia (FN)"]))
  updateNumericInput(session, "sp_cases", value = unname(params["Patients for surgical prophylaxis (SP)"]))


  # Sidebar item for "Severity of Cases"
  updateSliderInput(session, "cap_severe", value = unname(params["proportion of severe cases in CAP patients"]))
  updateSliderInput(session, "abd_severe", value = unname(params["proportion of severe cases in patients with intra-abdominal infection"]))
  updateSliderInput(session, "uti_severe", value = unname(params["proportion of severe cases in patients with acute pyelonephritis (upper UTI)"]))
  updateSliderInput(session, "cdf_severe", value = unname(params["proportion of severe cases in patients with C. difficile infection"]))
  updateSliderInput(session, "sst_nf", value = unname(params["proportion of necrotizing fasciitis cases in patient with SST"]))
                    
  # Sidebar item for "Availability of First Choice Antibiotics"
  updateSliderInput(session, "p_first", value = unname(params["probability of first-choice antibiotics"]))
  
  # Sidebar item for "Prevalence of AMR"
  updateSliderInput(session, "esbl_prevalence", value = unname(params["prevalence of ESBL"]))
  updateSliderInput(session, "mrsa_prevalence", value = unname(params["prevalence of MRSA"]))
  updateSliderInput(session, "hap_mdr", value = unname(params["probability(risk) of multi-drug resistant infection in HAP patients"]))
  updateSliderInput(session, "strep_pyogenes", value = unname(params["prevalence of Strep pyogenes infection in necrotizing fasciitis"]))

  # Sidebar item for "Total Admitted Patients"
  updateNumericInput(session, "admitted_patients", value = unname(params["total admitted patients"]))
  updateNumericInput(session, "std_err", value = unname(params["std_err"]))

}