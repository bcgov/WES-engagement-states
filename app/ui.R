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

          br(),
          tags$p("What characterizes these changes? Four types of migration describe the changes in the overall count and percentage of employees in each state from 2013 to 2015: "),
          br(),
          strong("1. Changes in continuing respondents: "),
          tags$p("perceptions of those who answered in both 2013 & 2015"),
          br(),
          strong("2. Changes in BC Public Service population: "),
          tags$p("employees hired into the BC Public Service since 2013 or departed from the BC Public Service after 2013"),
          br(),
          strong("3. Changes in organization population: "),
          tags$p("employees who transferred from another organization into the reference organization since 2013 or transferred out of the reference organization and into another organization after 2015"),
          br(),
          strong("4. Changes in sample: "),
          tags$p(" single-year respondents (answered in 2013 or 2015)."),
          
          DT::dataTableOutput("migration_table"),
          downloadButton("migration_data_download", "Export as CSV")
        )
      ),
      
      # Tab 3 - Methods
      tabPanel(
        "Methods",
        value = "tab_info",
        div(
          id = "tab_info",
          source(file.path("ui-tab-about.R"), local = TRUE)$value
        )
      ),
      
      # Tab 4 - Contact
      tabPanel(
        "Contact",
        value = "tab_contact",
        div(
          id = "tab_contact",
          source(file.path("ui-tab-contact.R"), local = TRUE)$value
        )
      )
    ),
    br(),
    tags$a(href="http://www2.gov.bc.ca/gov/content/home/copyright",
           "Copyright information")
  ))
)