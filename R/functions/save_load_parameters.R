#!!!
# Load input parameters into the fields
load_disease_inputs <- function(params,choices_ac, session) {
  
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
  if(choices_ac == "adult"){
    updateSliderInput(session, "cap_severe_single_adult", value = unname(params["proportion of severe cases in CAP patients"]))
  }
  if(choices_ac == "child"){
    updateSliderInput(session, "cap_severe_single_child", value = unname(params["proportion of severe cases in CAP patients"]))
    updateSliderInput(session, "cap_no_resp", value = unname(params["proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr"]))
    updateSliderInput(session, "hiv_severe", value = unname(params["proportion(risk) of severe CAP patients with HIV infection"]))
  }
  
  updateSliderInput(session, "abd_severe", value = unname(params["proportion of severe cases in patients with intra-abdominal infection"]))
  updateSliderInput(session, "uti_severe", value = unname(params["proportion of severe cases in patients with acute pyelonephritis (upper UTI)"]))
  updateSliderInput(session, "cdf_severe", value = unname(params["proportion of severe cases in patients with C. difficile infection"]))
  updateSliderInput(session, "sst_nf", value = unname(params["proportion of necrotizing fasciitis cases in patient with SST"]))

  
  
  # Sidebar item for "Availability of First Choice Antibiotics"
  # Populate checkboxGroupInput dynamically with all antibiotics
    # Populate checkboxGroupInput dynamically with all antibiotics
    updateCheckboxGroupInput(
      session = session,
      inputId = "selected_antibiotics",
      selected =  unname(params[names(params) == "first-choice antibiotics"])  # Select all by default
    )
  
  
  # Sidebar item for "Prevalence of AMR"
  updateSliderInput(session, "esbl_prevalence", value = unname(params["prevalence of ESBL"]))
  updateSliderInput(session, "mrsa_prevalence", value = unname(params["prevalence of MRSA"]))
  updateSliderInput(session, "hap_mdr", value = unname(params["probability(risk) of multi-drug resistant infection in HAP patients"]))
  updateSliderInput(session, "strep_pyogenes", value = unname(params["prevalence of Strep pyogenes infection in necrotizing fasciitis"]))

  # Sidebar item for "Total Admitted Patients"
  updateNumericInput(session, "admitted_patients", value = unname(params["total admitted patients"]))

}

#!!!
# Load input parameters into the fields
load_disease_inputs_adult <- function(params,choices_ac, session) {
  
  # text item for "Type of Patients" 1
  updateNumericInput(session, "cap_cases_adult", value = unname(params["Patients with community acquired pneumonia (CAP)"]))
  updateNumericInput(session, "bm_cases_adult", value = unname(params["Patients with bacterial meningitis"]))
  updateNumericInput(session, "bji_cases_adult", value = unname(params["Patients with bone and joint infections (BJ)"]))
  updateNumericInput(session, "uut_cases_adult", value = unname(params["Patients with upper UTI"]))
  updateNumericInput(session, "sepsis_cases_adult", value = unname(params["Patients with sepsis/ septic shock (SEPSIS)"]))
  updateNumericInput(session, "cdif_cases_adult", value = unname(params["Patients with Clostridioides difficile infection (CDIF)"]))
  
  # text item for "Type of Patients" 2
  updateNumericInput(session, "hap_cases_adult", value = unname(params["Patients with hospital acquired pneumonia (HAP) Non-VAP"]))
  updateNumericInput(session, "ia_cases_adult", value = unname(params["Patients with intra-abdominal infection (IA)"]))
  updateNumericInput(session, "sst_cases_adult", value = unname(params["Patients with skin and soft-tissue infection (SST)"]))
  updateNumericInput(session, "fn_cases_adult", value = unname(params["Patients with febrile neutropenia (FN)"]))
  updateNumericInput(session, "sp_cases_adult", value = unname(params["Patients for surgical prophylaxis (SP)"]))
  
  
  # Sidebar item for "Severity of Cases"
  updateSliderInput(session, "cap_severe_adult", value = as.numeric(unname(params["proportion of severe cases in CAP patients"])))
  updateSliderInput(session, "abd_severe_adult", value = as.numeric(unname(params["proportion of severe cases in patients with intra-abdominal infection"])))
  updateSliderInput(session, "uti_severe_adult", value = as.numeric(unname(params["proportion of severe cases in patients with acute pyelonephritis (upper UTI)"])))
  updateSliderInput(session, "cdf_severe_adult", value = as.numeric(unname(params["proportion of severe cases in patients with C. difficile infection"])))
  updateSliderInput(session, "sst_nf_adult", value = as.numeric(unname(params["proportion of necrotizing fasciitis cases in patient with SST"])))

  # Sidebar item for "Availability of First Choice Antibiotics"
  # Populate checkboxGroupInput dynamically with all antibiotics
  # Populate checkboxGroupInput dynamically with all antibiotics
  updateCheckboxGroupInput(
    session = session,
    inputId = "selected_antibiotics_adult",
    selected =  unname(params[names(params) == "first-choice antibiotics"])  # Select all by default
  )
  
  
  # Sidebar item for "Prevalence of AMR"
  updateSliderInput(session, "esbl_prevalence_adult", value = as.numeric(unname(params["prevalence of ESBL"])))
  updateSliderInput(session, "mrsa_prevalence_adult", value = as.numeric(unname(params["prevalence of MRSA"])))
  updateSliderInput(session, "hap_mdr_adult", value = as.numeric(unname(params["probability(risk) of multi-drug resistant infection in HAP patients"])))
  updateSliderInput(session, "strep_pyogenes_adult", value = as.numeric(unname(params["prevalence of Strep pyogenes infection in necrotizing fasciitis"])))
  
  # text item for "Total Admitted Patients"
  updateNumericInput(session, "admitted_patients_adult", value = unname(params["total admitted patients"]))
  
}

# Load input parameters into the fields
load_disease_inputs_child <- function(params,choices_ac, session) {
  
  # Sidebar item for "Type of Patients" 1
  updateNumericInput(session, "cap_cases_child", value = unname(params["Patients with community acquired pneumonia (CAP)"]))
  updateNumericInput(session, "bm_cases_child", value = unname(params["Patients with bacterial meningitis"]))
  updateNumericInput(session, "bji_cases_child", value = unname(params["Patients with bone and joint infections (BJ)"]))
  updateNumericInput(session, "uut_cases_child", value = unname(params["Patients with upper UTI"]))
  updateNumericInput(session, "sepsis_cases_child", value = unname(params["Patients with sepsis/ septic shock (SEPSIS)"]))
  updateNumericInput(session, "cdif_cases_child", value = unname(params["Patients with Clostridioides difficile infection (CDIF)"]))
  
  # Sidebar item for "Type of Patients" 2
  updateNumericInput(session, "hap_cases_child", value = unname(params["Patients with hospital acquired pneumonia (HAP) Non-VAP"]))
  updateNumericInput(session, "ia_cases_child", value = unname(params["Patients with intra-abdominal infection (IA)"]))
  updateNumericInput(session, "sst_cases_child", value = unname(params["Patients with skin and soft-tissue infection (SST)"]))
  updateNumericInput(session, "fn_cases_child", value = unname(params["Patients with febrile neutropenia (FN)"]))
  updateNumericInput(session, "sp_cases_child", value = unname(params["Patients for surgical prophylaxis (SP)"]))
 
  # Sidebar item for "Severity of Cases"
  updateSliderInput(session, "cap_severe_child", value = as.numeric(unname(params["proportion of severe cases in CAP patients"])))
  updateSliderInput(session, "abd_severe_child", value = as.numeric(unname(params["proportion of severe cases in patients with intra-abdominal infection"])))
  updateSliderInput(session, "uti_severe_child", value = as.numeric(unname(params["proportion of severe cases in patients with acute pyelonephritis (upper UTI)"])))
  updateSliderInput(session, "cdf_severe_child", value = as.numeric(unname(params["proportion of severe cases in patients with C. difficile infection"])))
  updateSliderInput(session, "sst_nf_child", value = as.numeric(unname(params["proportion of necrotizing fasciitis cases in patient with SST"])))
  updateSliderInput(session, "cap_no_resp_child", value = as.numeric(unname(params["proportion of severe CAP patients with no clinical response to first-line treatment (Penicillin + Gentamicin) after 48 hr"])))
  updateSliderInput(session, "hiv_severe_child", value = as.numeric(unname(params["proportion(risk) of severe CAP patients with HIV infection"])))
  
  
  # Sidebar item for "Availability of First Choice Antibiotics"
  # Populate checkboxGroupInput dynamically with all antibiotics
  # Populate checkboxGroupInput dynamically with all antibiotics
  updateCheckboxGroupInput(
    session = session,
    inputId = "selected_antibiotics_child",
    selected =  unname(params[names(params) == "first-choice antibiotics"])  # Select all by default
  )
  
  
  # Sidebar item for "Prevalence of AMR"
  updateSliderInput(session, "esbl_prevalence_child", value = as.numeric(unname(params["prevalence of ESBL"])))
  updateSliderInput(session, "mrsa_prevalence_child", value = as.numeric(unname(params["prevalence of MRSA"])))
  updateSliderInput(session, "hap_mdr_child", value = as.numeric(unname(params["probability(risk) of multi-drug resistant infection in HAP patients"])))
  updateSliderInput(session, "strep_pyogenes_child", value = as.numeric(unname(params["prevalence of Strep pyogenes infection in necrotizing fasciitis"])))
  
  # Sidebar item for "Total Admitted Patients"
  updateNumericInput(session, "admitted_patients_child", value = unname(params["total admitted patients"]))
  
}