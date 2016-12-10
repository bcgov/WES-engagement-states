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
        h1(strong("BC Public Service", br(), "Work Environment Survey (WES)"))
    ),
    
    tabsetPanel(
      id = "main_nav",
      
      # Tab 1 - Engagement state
      tabPanel(
        "Engagement State",
        value = "tab_engagement",
        div(
          id = "tab_engagement",
          div(
            style = "display: inline-block; vertical-align: top;",
            selectInput(
              "engagement_org", "Organization:",
              choices = org_names_list,
              width = "400px"
            )
          ),
          div(
            style = "display: inline-block; vertical-align: top;",
            br(), downloadButton("btn_report", "Generate report")
          ),
          fixedRow(
            column(6, plotOutput("engagement_plot_13")),
            column(6, plotOutput("engagement_plot_15"))
          ),
          div(
            id = "engagement_plots_legend",
            lapply(seq(ENG_STATES_FULL), function(x) {
              span(
                span(
                  class = "legend_circle",
                  style = paste0("background: ", PLOT_COLS[x])
                ),
                ENG_STATES_FULL[x]
              )
            })
          ),
          DT::dataTableOutput("engagement_table"),
          downloadButton("engagement_data_download", "Export as CSV")
        )
      ),
      
      # Tab 2 - Migration analysis
      tabPanel(
        "Migration Flow",
        value = "tab_migration",
        div(
          id = "tab_migration",
          selectInput(
            "migration_org", "Organization:",
            choices = org_names_list,
            width = "400px"
          ),
          div(
            id = "migration_plot_area",
            div(id = "migration_plot_title_left", "2013"),
            div(id = "migration_plot_title_right", "2015"),
            sankeyNetworkOutput("migration_plot")
          ),
          DT::dataTableOutput("migration_table"),
          downloadButton("migration_data_download", "Export as CSV")
        )
      ),
      
      # Tab 3 - Description
      tabPanel(
        "Methods",
        value = "tab_info",
        div(
          id = "tab_info",
          source(file.path("ui-tab-about.R"), local = TRUE)$value
        )
      )
    )
  ))
)