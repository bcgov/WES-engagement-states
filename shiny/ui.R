fixedPage(
  title = "BC Work Environment Survey",
  
  tags$head(
    tags$link(href = "app.css", rel = "stylesheet")
  ),
  
  fixedRow(column(
    12,
    
    # Header
    img(src = "BCStats.png", id = "logo"),
    div(id = "titleSection",
        h1(strong("BC Work Environment Survey"))
    ),
    
    tabsetPanel(
      id = "main_nav",
      
      # Tab 1 - Engagement state
      tabPanel(
        "Engagement State",
        value = "tab_engagement",
        selectInput(
          "engagement_org", "Organization:",
          choices = org_names_list
        ),
        plotOutput("engagement_plot"),
        downloadButton("report", "Generate report"),
        DT::dataTableOutput("engagement_table"),
        downloadButton("engagement_data_download", "Export as CSV")
      ),
      
      # Tab 2 - Migration analysis
      tabPanel(
        "Migration Analysis",
        value = "tab_migration",
        selectInput(
          "migration_org", "Organization:",
          choices = org_names_list
        ),
        sankeyNetworkOutput("migration_plot"),
        DT::dataTableOutput("migration_table"),
        downloadButton("migration_data_download", "Export as CSV")
      ),
      
      # Tab 3 - Description
      tabPanel(
        "Methods",
        value = "tab_info",
        "[TODO]"
      )
    )
  ))
)