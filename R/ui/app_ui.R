# SLPHYX@SHIFT-ENTER

app_ui <- navbarPage(
  title = "Estimating expected antibiotics usage in hospitals according to the WHO AWaRe book guideline",
  
  # First tab: About the app (empty for now)
  tabPanel("About the App",
           fluidPage(
             h3("About the App"),
             p("This page will contain information about the app.")
           )),
  
  # Second tab: Run the simulation (show/hide input menu and full results page)
  tabPanel("Run the Simulation",
           fluidPage(
             titlePanel("Multi-Drug Antibiotic Choice Model with 10 Fixed Tabs"),
             
             # Toggle button to show/hide the input panel
             actionButton("toggleInputs", "Show/Hide Inputs"),
             
             # Create an area for input parameters that can be hidden
             conditionalPanel(
               condition = "input.toggleInputs % 2 == 0",  # Show when toggle is even
               fluidRow(
                 column(3, wellPanel(  # Input panel placed on the left
                   h4("Input Parameters"),
                   # 10 tabs for drug inputs
                   uiOutput("drug_tabs"),
                   br(),
                   # Submit button inside the input panel
                   actionButton("submit", "Run Simulation")
                 ))
               )
             ),
             
             # Main panel for showing the results
             fluidRow(
               column(12, 
                      h4("Simulation Results"),
                      tableOutput("model_output")  # Table to display the combined results
               )
             )
           )),
  
  # Third tab: Details about the terminology (empty for now)
  tabPanel("Details about the Terminology",
           fluidPage(
             h3("Details about the Terminology"),
             p("This page will contain detailed explanations of terminology used in the app.")
           ))
)
