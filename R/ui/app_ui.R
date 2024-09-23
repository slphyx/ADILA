# SLPHYX@SHIFT-ENTER

# Define the UI
app_ui <- fluidPage(
  
  # Application title
  titlePanel("Antibiotic Choice and ESBL Model"),
  
  # Sidebar layout to accept parameters
  sidebarLayout(
    
    # Sidebar to accept inputs
    sidebarPanel(
      numericInput("p_first", "Probability of First Choice Antibiotic (p.first)", value = 0.90, min = 0, max = 1, step = 0.01),
      numericInput("p_esbl", "Prevalence of ESBL Infection (p.esbl)", value = 0.30, min = 0, max = 1, step = 0.01),
      numericInput("pt_admt", "Total Admitted Patients (pt.admt)", value = 150, min = 1, max = 1000, step = 1),
      numericInput("pt_bmen", "Patients with Bacterial Meningitis (pt.bmen)", value = 50, min = 1, max = 1000, step = 1),
      numericInput("pt_uuti", "Patients with Upper UTI (pt.uuti)", value = 50, min = 1, max = 1000, step = 1),
      numericInput("p_sev_uti", "Proportion of Severe Upper UTI (p.sev.uti)", value = 0.2, min = 0, max = 1, step = 0.01),
      
      # Button to submit inputs and update the table
      actionButton("submit", "Submit")
    ),
    
    # Main panel to display the table
    mainPanel(
      tableOutput("model_output")
    )
  )
)
