
library(dplyr)
library(gtsummary)

# Function to simulate model and return summary table
run_simulation_fn <- function(adult_cases, para_data, std_err) {
  
  # Create an empty dataframe to store model's output
  result_adult <- data.frame()
  
  # Run the model 1000 times
  for (i in 1:1000) {
    set.seed(1000 + i)
    
    # Generate input dataframe
    input_df <- generate_input_dataframe(adult_cases, para_data, std_err)
    
    # Simulate model A and store results
    temp <- as.data.frame(t(model_A(input_df)))  # Assuming model_A is defined elsewhere
    result_adult <- rbind(result_adult, temp)
  }
  
  # Convert all columns to numeric
  df_numeric <- result_adult %>%
    mutate_all(~ as.numeric(as.character(.)))
  
  # Generate summary table
  summary_table <- df_numeric %>%
    tbl_summary(
      by = NULL,
      statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      missing = "no"
    ) %>%
    modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
    modify_footnote(all_stat_cols() ~ "Median (IQR), 1000 iterations") %>%
    modify_caption("**Table: Expected antibiotic usage in hospital if the empirical prescription follows the WHO guidelines**")
  
  return(summary_table)
}
