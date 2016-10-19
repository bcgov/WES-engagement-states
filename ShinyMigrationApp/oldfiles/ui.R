library(DT)
library(shiny)
library(dplyr)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Engagement State"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Organization", "Organization ID", 
                  unique(as.character(CombinedStates1$ORGID15))),
      hr(),
      downloadButton('downloadFile','Download Report', class="dlButton"),
      helpText("Download pre-existing reports"),
      hr(),
      downloadButton('generateReport','Generate Report',class="dlButton"),
      helpText("Generate and download reports dynamically")
    ),
    
    # The main panel
    mainPanel(
      h3(textOutput("orgName")), # Dynamic header with the full name of the organization
      DT::dataTableOutput("EngagementTable")  # The main datatable
    )
    
  )
)
