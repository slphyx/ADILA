
# Helper function to create adult cases dataframe
create_adult_cases_df <- function() {
  data.frame(
    syndrome = c("CAP", "HAP", "CNS", "IA", "PYE", "SST", "BJ", "CDIF", "FN", "SEPSIS", "SP"),
    cases = c(50, 10, 15, 35, 35, 30, 30, 30, 10, 30, 60)
  )
}
