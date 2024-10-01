# Save input parameters for a single disease tab
save_disease_inputs <- function(input, i) {
  list(
    p_first_dist = input[[paste0("p_first_dist_", i)]],
    p_first_min = input[[paste0("p_first_min_", i)]],
    p_first_max = input[[paste0("p_first_max_", i)]],
    p_first_mean = input[[paste0("p_first_mean_", i)]],
    p_first_sd = input[[paste0("p_first_sd_", i)]],
    p_first_shape = input[[paste0("p_first_shape_", i)]],
    p_first_rate = input[[paste0("p_first_rate_", i)]],
    
    p_esbl_dist = input[[paste0("p_esbl_dist_", i)]],
    p_esbl_min = input[[paste0("p_esbl_min_", i)]],
    p_esbl_max = input[[paste0("p_esbl_max_", i)]],
    p_esbl_mean = input[[paste0("p_esbl_mean_", i)]],
    p_esbl_sd = input[[paste0("p_esbl_sd_", i)]],
    p_esbl_shape = input[[paste0("p_esbl_shape_", i)]],
    p_esbl_rate = input[[paste0("p_esbl_rate_", i)]],
    
    pt_admt_dist = input[[paste0("pt_admt_dist_", i)]],
    pt_admt_min = input[[paste0("pt_admt_min_", i)]],
    pt_admt_max = input[[paste0("pt_admt_max_", i)]],
    pt_admt_mean = input[[paste0("pt_admt_mean_", i)]],
    pt_admt_sd = input[[paste0("pt_admt_sd_", i)]],
    pt_admt_shape = input[[paste0("pt_admt_shape_", i)]],
    pt_admt_rate = input[[paste0("pt_admt_rate_", i)]],
    
    pt_bmen_dist = input[[paste0("pt_bmen_dist_", i)]],
    pt_bmen_min = input[[paste0("pt_bmen_min_", i)]],
    pt_bmen_max = input[[paste0("pt_bmen_max_", i)]],
    pt_bmen_mean = input[[paste0("pt_bmen_mean_", i)]],
    pt_bmen_sd = input[[paste0("pt_bmen_sd_", i)]],
    pt_bmen_shape = input[[paste0("pt_bmen_shape_", i)]],
    pt_bmen_rate = input[[paste0("pt_bmen_rate_", i)]],
    
    pt_uuti_dist = input[[paste0("pt_uuti_dist_", i)]],
    pt_uuti_min = input[[paste0("pt_uuti_min_", i)]],
    pt_uuti_max = input[[paste0("pt_uuti_max_", i)]],
    pt_uuti_mean = input[[paste0("pt_uuti_mean_", i)]],
    pt_uuti_sd = input[[paste0("pt_uuti_sd_", i)]],
    pt_uuti_shape = input[[paste0("pt_uuti_shape_", i)]],
    pt_uuti_rate = input[[paste0("pt_uuti_rate_", i)]],
    
    p_sev_uti_dist = input[[paste0("p_sev_uti_dist_", i)]],
    p_sev_uti_min = input[[paste0("p_sev_uti_min_", i)]],
    p_sev_uti_max = input[[paste0("p_sev_uti_max_", i)]],
    p_sev_uti_mean = input[[paste0("p_sev_uti_mean_", i)]],
    p_sev_uti_sd = input[[paste0("p_sev_uti_sd_", i)]],
    p_sev_uti_shape = input[[paste0("p_sev_uti_shape_", i)]],
    p_sev_uti_rate = input[[paste0("p_sev_uti_rate_", i)]]
  )
}

# Load input parameters into the fields
load_disease_inputs <- function(params, i, session) {
  updateRadioButtons(session, paste0("p_first_dist_", i), selected = params$p_first_dist)
  updateNumericInput(session, paste0("p_first_min_", i), value = params$p_first_min)
  updateNumericInput(session, paste0("p_first_max_", i), value = params$p_first_max)
  updateNumericInput(session, paste0("p_first_mean_", i), value = params$p_first_mean)
  updateNumericInput(session, paste0("p_first_sd_", i), value = params$p_first_sd)
  updateNumericInput(session, paste0("p_first_shape_", i), value = params$p_first_shape)
  updateNumericInput(session, paste0("p_first_rate_", i), value = params$p_first_rate)
  
  updateRadioButtons(session, paste0("p_esbl_dist_", i), selected = params$p_esbl_dist)
  updateNumericInput(session, paste0("p_esbl_min_", i), value = params$p_esbl_min)
  updateNumericInput(session, paste0("p_esbl_max_", i), value = params$p_esbl_max)
  updateNumericInput(session, paste0("p_esbl_mean_", i), value = params$p_esbl_mean)
  updateNumericInput(session, paste0("p_esbl_sd_", i), value = params$p_esbl_sd)
  updateNumericInput(session, paste0("p_esbl_shape_", i), value = params$p_esbl_shape)
  updateNumericInput(session, paste0("p_esbl_rate_", i), value = params$p_esbl_rate)
  
  updateRadioButtons(session, paste0("pt_admt_dist_", i), selected = params$pt_admt_dist)
  updateNumericInput(session, paste0("pt_admt_min_", i), value = params$pt_admt_min)
  updateNumericInput(session, paste0("pt_admt_max_", i), value = params$pt_admt_max)
  updateNumericInput(session, paste0("pt_admt_mean_", i), value = params$pt_admt_mean)
  updateNumericInput(session, paste0("pt_admt_sd_", i), value = params$pt_admt_sd)
  updateNumericInput(session, paste0("pt_admt_shape_", i), value = params$pt_admt_shape)
  updateNumericInput(session, paste0("pt_admt_rate_", i), value = params$pt_admt_rate)
  
  updateRadioButtons(session, paste0("pt_bmen_dist_", i), selected = params$pt_bmen_dist)
  updateNumericInput(session, paste0("pt_bmen_min_", i), value = params$pt_bmen_min)
  updateNumericInput(session, paste0("pt_bmen_max_", i), value = params$pt_bmen_max)
  updateNumericInput(session, paste0("pt_bmen_mean_", i), value = params$pt_bmen_mean)
  updateNumericInput(session, paste0("pt_bmen_sd_", i), value = params$pt_bmen_sd)
  updateNumericInput(session, paste0("pt_bmen_shape_", i), value = params$pt_bmen_shape)
  updateNumericInput(session, paste0("pt_bmen_rate_", i), value = params$pt_bmen_rate)
  
  updateRadioButtons(session, paste0("pt_uuti_dist_", i), selected = params$pt_uuti_dist)
  updateNumericInput(session, paste0("pt_uuti_min_", i), value = params$pt_uuti_min)
  updateNumericInput(session, paste0("pt_uuti_max_", i), value = params$pt_uuti_max)
  updateNumericInput(session, paste0("pt_uuti_mean_", i), value = params$pt_uuti_mean)
  updateNumericInput(session, paste0("pt_uuti_sd_", i), value = params$pt_uuti_sd)
  updateNumericInput(session, paste0("pt_uuti_shape_", i), value = params$pt_uuti_shape)
  updateNumericInput(session, paste0("pt_uuti_rate_", i), value = params$pt_uuti_rate)
  
  updateRadioButtons(session, paste0("p_sev_uti_dist_", i), selected = params$p_sev_uti_dist)
  updateNumericInput(session, paste0("p_sev_uti_min_", i), value = params$p_sev_uti_min)
  updateNumericInput(session, paste0("p_sev_uti_max_", i), value = params$p_sev_uti_max)
  updateNumericInput(session, paste0("p_sev_uti_mean_", i), value = params$p_sev_uti_mean)
  updateNumericInput(session, paste0("p_sev_uti_sd_", i), value = params$p_sev_uti_sd)
  updateNumericInput(session, paste0("p_sev_uti_shape_", i), value = params$p_sev_uti_shape)
  updateNumericInput(session, paste0("p_sev_uti_rate_", i), value = params$p_sev_uti_rate)
}