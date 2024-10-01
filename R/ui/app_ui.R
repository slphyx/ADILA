# SLPHYX@SHIFT-ENTER

# R/ui/app_ui.R

app_ui <- navbarPage(
  title = "Estimating expected antibiotics usage in hospitals according to the WHO AWaRe book guideline",
  
  # First tab: About the App
  tabPanel("About the App",
           fluidPage(
             h3("About the App"),
             p("This page will contain information about the app.")
           )),
  
  # Second tab: Run the Simulation
  tabPanel("Run the Simulation",
           fluidPage(
             titlePanel("Multi-Drug Antibiotic Choice Model"),
             
             # Upper part: Input Parameters
             fluidRow(
               column(12,  # Full-width container
                      h3("Parameter Inputs"),  # Title for parameter inputs
                      
                      # Buttons for saving and loading inputs
                      fluidRow(
                        column(3, actionButton("toggleInputs", "Show/Hide Inputs")),
                        column(3, downloadButton("save_params", "Save Inputs")),
                        column(3, fileInput("load_params", "Load Inputs", accept = c(".json")))
                      ),
                      
                      # Input panel can be hidden and will use full width of the page
                      conditionalPanel(
                        condition = "input.toggleInputs % 2 == 0",
                        wellPanel(
                          h4("Input Parameters"),
                          uiOutput("drug_tabs"),  # Dynamic UI output for tabs
                          br(),
                          actionButton("submit", "Run Simulation")  # Submit button
                        )
                      )
               )
             ),
             
             # Lower part: Simulation Results
             fluidRow(
               column(12, 
                      h3("Simulation Results"),  # Title for results
                      tableOutput("model_output")  # Display results
               )
             )
           )),
  
  # Third tab: Terminology Details
  tabPanel("Details about the Terminology",
           fluidPage(
             h3("Details about the Terminology"),
             p("This page will contain detailed explanations of terminology used in the app.")
           ))
)
