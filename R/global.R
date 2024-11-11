# load library
library(gtools)

source("R/functions/model.adult.shiny.v3.R")
source("R/functions/data.adult.shiny.v3.R")
source("R/functions/generate_input_dataframe.R")
source("R/functions/fluid_design.R")

first_choice_list <- data.frame("First-choice antibiotics (WHO AwaRe book)"=c("Amoxicillin",
                                               "Amoxicillin/ clavulanic-acid",
                                               "Phenoxymethyl penicillin",
                                               "Ampicillin",
                                               "Cloxacillin",
                                               "Benzyl penicillin",
                                               "Gentamycin Amikacin",
                                               "Sulphamethoxazole-Trimethoprim",
                                               "Clindamycin",
                                               "Metronidazole",
                                               "Doxycycline",
                                               "Chloramphenicol",
                                               "Cefalexin",
                                               "Cefazolin",
                                               "Cefuroxime",
                                               "Cefotaxime",
                                               "Ceftriaxone",
                                               "Meropenem",
                                               "Piperacillin + Tazobactam",
                                               "Clarithromycin",
                                               "Vancomycin",
                                               "Ciprofloxacin"),check.names = FALSE)

value_fin <- reactiveValues(finished=0, TotalAdmittedPatient=0)   # for checking whether the simulation has finished ; 0 <- not finished 1 <- finished