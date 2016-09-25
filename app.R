library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(networkD3)

# Define the possible engagement states
STATES <- c("Engaged", "Dedicated", "Detached", "Disengaged", "Incomplete", "N/A")

# Constants to use in the code
PLOT_COLS <- c("#325A80", "#5091CD", "#FFFF05", "#D2BE32", "#FA1E1E", "#A40000")

# Initialize the data
eng_state_data <- read.csv("data/eng_states_data.csv")
eng_state_data$Engagement.State <- factor(
  eng_state_data$Engagement.State, 
  levels = c("Engaged", "Moderately Engaged",
             "Happily Detached", "Unhappily Dedicated",
             "Minimally Engaged", "Disengaged")
)
eng_state_data <- eng_state_data %>%
  group_by(ORGID) %>%
  mutate(Percent = round(Employees / sum(Employees) * 100)) %>%
  ungroup() %>%
  arrange(ORG, Engagement.State)

org_names_list <-
  c("All" = "all",
    setNames(unique(eng_state_data$ORGID) %>% as.character(),
             unique(eng_state_data$ORG) %>% as.character()
  ))

# Code for the engagement plot
engagement_plot <- function(data) {
  mean_satisfaction <- sum(data$Satisfaction * data$Percent / 100)
  mean_commitment <- sum(data$Commitment * data$Percent / 100)
  
  ggplot(
    data,
    aes(x = Commitment, y = Satisfaction,
        fill = Engagement.State)) +
    geom_vline(xintercept = mean_commitment, col = "#555555") +
    geom_hline(yintercept = mean_satisfaction, col = "#555555") +
    geom_point(aes(size = Percent), 
               shape = 21, colour = "black") +
    scale_size_area(max_size = 50, guide = FALSE) + 
    xlim(0, 120) + ylim(0, 120) +
    scale_fill_manual(values = PLOT_COLS) + 
    
    theme_bw(26) +
    guides(fill = guide_legend("Engagement State", override.aes = list(size = 4))) +
    theme(legend.key = element_blank(), panel.grid.minor = element_blank(),
          legend.key.height = unit(1.5, "line"))
}

# Shiny app
ui <- fixedPage(
  title = "BC Work Environment Survey",
  tags$head(
    tags$link(href = "app.css", rel = "stylesheet")
  ),
  fixedRow(column(
    12,
    img(src = "BCStats.png", id = "logo"),
    div(id = "titleSection",
        h1(strong("BC Work Environment Survey"))
    ),
    tabsetPanel(
      id = "main_nav",
      tabPanel(
        paste0("Engagement state "),
        value = "tab_engagement",
        selectInput(
          "engagement_org", "Organization:",
          choices = org_names_list
        ),
        plotOutput("engagement_plot", height = "500px"),
        DT::dataTableOutput("engagement_table")
      ),
      tabPanel(
        sprintf("Migration analysis"),
        value = "tab_migration",
        selectInput(
          "migration_org", "Organization:",
          choices = org_names_list
        ),
        sankeyNetworkOutput("migration_plot"),
        DT::dataTableOutput("migration_table")
      ),
      tabPanel(
        "Methods",
        value = "tab_info",
        "[TODO]"
      )
    )
  ))
)

server <- function(input, output, session) {
  filtered_data <- eventReactive(input$engagement_org, {
    if (input$engagement_org == "all") {
      eng_state_data
    } else {
      eng_state_data %>%
        filter(ORGID == input$engagement_org)
    }
  })
  
  filtered_data_agg <- eventReactive(filtered_data(), {
    data <- filtered_data() %>%
      dplyr::select_("Engagement.State", "Satisfaction", "Commitment",
                     "Employees", "Percent")
    
    if (input$engagement_org != "all") {
      data
    } else {
      data %>%
        group_by_("Engagement.State") %>%
        summarise(
          Satisfaction = weighted.mean(Satisfaction, Employees),
          Commitment = weighted.mean(Commitment, Employees),
          Employees = sum(Employees)
        ) %>%
        mutate(Percent = round(Employees / sum(Employees) * 100)) %>%
        arrange(Engagement.State)
    }
  })
  
  output$engagement_table <- DT::renderDataTable({
    data <- filtered_data() %>%
      dplyr::select(ORG, Engagement.State, Satisfaction, Commitment,
                    Employees, Percent)
    DT::datatable(
      data,
      rownames = FALSE,
      selection = 'none',
      class = 'stripe',
      options = list(
        searching = FALSE, paging = TRUE,
        scrollX = TRUE, scrollY = FALSE,
        dom = 'tlp',
        scrollCollapse = TRUE
      )
    )
  })
  
  output$engagement_plot <- renderPlot({
    engagement_plot(filtered_data_agg())
  })
  
  # ------- Tab 2: Migration analysis
  
  # Every time a new org is chosen, create MOCK migration data
  migration_data <- reactive({
    input$migration_org
    
    migration <- expand.grid(eng_past = STATES, eng_current = STATES,
                             KEEP.OUT.ATTRS = FALSE) %>%
      arrange(eng_past, eng_current) %>%
      mutate(num = round(runif(nrow(.), 10, 250)))
    migration
  }) 
  
  output$migration_table <- DT::renderDataTable({
    data <- migration_data()
    
    DT::datatable(
      data,
      rownames = FALSE,
      selection = 'none',
      class = 'stripe',
      options = list(
        searching = FALSE, paging = TRUE,
        scrollX = TRUE, scrollY = FALSE,
        dom = 'tlp',
        scrollCollapse = TRUE
      )
    )
  })
  
  output$migration_plot <- renderSankeyNetwork({
    # Transform migration data to a more useful format for sankey diagrams
    migration_sankey <- migration_data()
    migration_sankey$eng_past <- as.integer(migration_sankey$eng_past) - 1
    migration_sankey$eng_current <- as.integer(migration_sankey$eng_current) - 1 +
      length(STATES)
    states_df <- data.frame(state = c(STATES, STATES))
    
    sankeyNetwork(Links = migration_sankey, Nodes = states_df,
                  Source = 'eng_past', Target = 'eng_current', Value = 'num',
                  NodeID = 'state', fontSize = 22, fontFamily = "Arial",
                  nodeWidth = "20", nodePadding = "10", iterations = 0)
  })
}

shinyApp(ui = ui, server = server)
