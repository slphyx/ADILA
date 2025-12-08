


#####################################
# scripts prepared on 28 November 2024
# by Myo Maung Maung Swe
#####################################

# Libraries 

if (!require("dplyr"))     install.packages("dplyr");     library(dplyr)
if (!require("tidyr"))     install.packages("tidyr");     library(tidyr)
if (!require("gtsummary")) install.packages("gtsummary"); library(gtsummary)
if (!require("gtools"))    install.packages("gtools");    library(gtools)
if (!require("ggplot2"))   install.packages("ggplot2");   library(ggplot2)
if (!require("viridis"))   install.packages("viridis");   library(viridis)
if (!require("hrbrthemes"))install.packages("hrbrthemes");library(hrbrthemes)


# Call the function
source("model.child.shiny.R")

# Create an empty dataframe to store model's output
result_child <- data.frame()

# Run the model 1000 times
for (i in 1:1000){
  set.seed(1000+i)
  source("data.child.shiny.R")
  temp <- as.data.frame(t(model_C(input.child)))
  result_child <- rbind(result_child, temp)
  
}


# Convert all columns to numeric
df_numeric <- result_child %>%
  mutate_all(~ as.numeric(as.character(.)))


# Summary tables----
# Table 1
summary_table_overall <- df_numeric[,1:8] %>%
  tbl_summary(
    by = NULL,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    missing = "no"
  ) %>%
  modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
  modify_caption("**Table 1: Overall expected empirical antibiotic usage in hospital**") 

summary_table_overall

#access and watch antibiotic usage in percentage of total use # Column `Description` doesn't exist.



summary_table_overall %>% ungroup() %>%
  select(Description, median_value, lower, upper) %>%
  distinct() %>%
  mutate(
    median_value = paste0(median_value, "%"),
    lower = paste0(lower, "%"),
    upper = paste0(upper, "%"),
    expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
  ) %>%
  select(Description, expected_use)


# expected use in percentage of total 
percent_table_syndrome <- percent_table_syndrome_long %>%
  group_by(Description) %>%
  mutate(
    median_value = round(median(`Expected use (% of total use)`, na.rm = TRUE), 1),
    lower = round(quantile(`Expected use (% of total use)`, 0.025, na.rm = TRUE), 1),
    upper = round(quantile(`Expected use (% of total use)`, 0.975, na.rm = TRUE), 1)
  ) %>%
  ungroup() %>%
  select(Description, median_value, lower, upper) %>%
  distinct() %>%
  mutate(
    median_value = paste0(median_value, "%"),
    lower = paste0(lower, "%"),
    upper = paste0(upper, "%"),
    expected_use = paste0(median_value, " (", lower, ", ", upper, ")")
  ) %>%
  select(Description, expected_use)

df <- summary_table_overall %>%
  mutate(
    expected_use = if_else(
      Description %in% c("Antibiotics 1 (%)", "Antibiotics 2 (%)"),
      paste0(median_value, "% (", lower, "%, ", upper, "%)"),
      NA_character_
    )
  )



tbl_percent <- df_percent %>%
  gt::gt() %>%
  gt::tab_header(title = "Table 2: Antibiotic Use Proportions")

# Table 2
summary_table_syndrome <- df_numeric[,9:(9+21)] %>%
  tbl_summary(
    by = NULL,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    missing = "no"
  ) %>%
  modify_header(label="**Description**", stat_0 = "**Expected usage**") %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy, CAP=community acquired pneumonia,
                  HAP = hospital acquired pneumonia, SST = skin and soft-tissue infection") %>%
  modify_caption("**Table 2: Expected empirical antibiotic usage by infection syndrome**") 


summary_table_syndrome

# Table 3
summary_table_class_access <- df_numeric[, 31:38] %>%
  tbl_summary(
    by = NULL,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    missing = "no"
  ) %>%
  modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DOT)**") %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
  modify_caption("**Table 3: Expected empirical Access antibiotic usage by antibiotic class**") 


summary_table_class_access

# Table 4
summary_table_class_watch <- df_numeric[, 39:ncol(df_numeric)] %>%
  tbl_summary(
    by = NULL,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    missing = "no"
  ) %>%
  modify_header(label="**Antibiotic class**", stat_0 = "**Expected usage (DOT)**") %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR), DOT = days of therapy") %>%
  modify_caption("**Table 4: Expected empirical Watch antibiotic usage by antibiotic class**") 


summary_table_class_watch

#Plotting----
# Change the dataformat for the plot
df_plot <- df_numeric %>%
  gather(key = "text", value= "value") %>%
  mutate(value = round(as.numeric(value),1))

# Distribution of "Access" and "Watch" antibiotic usage out of total

# Plot 1
# Access antibiotic
plot_access <- df_plot %>%
  filter(text %in% c("Access antibiotics(%)")) %>%
  ggplot( aes(x=value, fill=text)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 1) +
  scale_fill_manual(values = c("#009E73")) +
  theme_ipsum() +
  labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
       x = "Percentage of Overall Usage", 
       y = "Frequency") +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0,100)) +
  theme(legend.position = "none",
        axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 8),
        title  = element_text(size = 10))

plot_access

# Plot 2
# Watch antibiotic 
plot_watch <- df_plot %>%
  filter(text %in% c("Watch antibiotics(%)")) %>%
  ggplot( aes(x=value, fill=text)) +
  geom_histogram(color="#5d5e5f", alpha=0.6, position = 'identity', binwidth = 1) +
  scale_fill_manual(values = c("#F0E442")) +
  theme_ipsum() +
  labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
       x = "Percentage of Overall Usage", 
       y = "Frequency") +
  scale_x_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0,100)) +
  theme(legend.position = "none",
        axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 8),
        title  = element_text(size = 10))

plot_watch



# AWaRe group by antibiotic class

df_aware <- data.frame(
  text = c("Penicillins", "Beta-lactam antibiotics plus enzyme inhibitor",
           "First generation cephlosporins", "Aminoglycosides",
           "Nitroimidazoles", "Sulfonamides",
           "Amphenicols", "Lincosamides",
           "Second/ Third generation cephalosporins", "Macrolides",
           "Beta-lactam antibiotics plus enzyme inhibitor: Anti-pseudomonal", "Fluroquinolones",
           "Carbapenems", "Glycopeptides"),
  aware = c(rep("Access", time = 8), rep("Watch", time = 6))
)


# Antibiotic class
df_class <- df_numeric %>%
  select(Penicillins:last_col()) %>%
  gather(key = "text", value= "value") %>%
  mutate(value = round(as.numeric(value),1)) %>%
  left_join(df_aware, by = "text")

# Plot 3
# Access antibiotic by antibiotic class
plot_access_class <- df_class %>%
  filter(aware == "Access") %>%
  ggplot( aes(x=value, fill=text)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 1) +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum() +
  labs(title = "Distribution of Expected Empirical Access Antibiotic Usage",
       x = "Expected usage (DOT)", 
       y = "Frequency",
       fill = "Antibiotic class") +
  theme(legend.text = element_text(size = 8),
        axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 8),
        title  = element_text(size = 10))

plot_access_class

# Plot 4
# Watch antibiotic by antibiotic class
plot_watch_class <- df_class %>%
  filter(aware == "Watch") %>%
  ggplot( aes(x=value, fill=text)) +
  geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity', binwidth = 1) +
  scale_fill_viridis(discrete=TRUE) +
  theme_ipsum() +
  labs(title = "Distribution of Expected Empirical Watch Antibiotic Usage",
       x = "Expected usage (DOT)", 
       y = "Frequency",
       fill = "Antibiotic class") +
  theme(legend.text = element_text(size = 8),
        axis.text   = element_text(size = 8),
        axis.title  = element_text(size = 8),
        title  = element_text(size = 10))

plot_watch_class


df_main <- df_numeric %>%
  select(1:6)  # Replace with actual column names if needed

df_percent <- df_numeric %>%
  select(7:8)

# For the 6 numeric variables
tbl_main <- df_main %>%
  tbl_summary(
    by = NULL,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    missing = "no"
  )

# For the 2 percentage rows
tbl_percent <- df_percent %>%
  tbl_summary(
    by = NULL,
    statistic = all_continuous() ~ "{median} % ({p25}%, {p75}%)",
    missing = "no"
  )
