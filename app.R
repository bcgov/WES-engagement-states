# TODO:
# - see why there's a console message when choosing a new org
# - make app much nicer
# - refactor code and directory structure
# - make plot interactive

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Constants to use in the code
YEAR_START <- 2013
YEAR_END <- 2015
PLOT_COLS <- c("#325A80", "#5091CD", "#FFFF05", "#D2BE32", "#FA1E1E", "#A40000")

# Initialize the data
eng_state_data <- read.csv("../WES-engagement-states/data/eng_states_data.csv")
eng_state_data$Engagement.State <- factor(
  eng_state_data$Engagement.State, 
  levels = c("Engaged", "Moderately Engaged",
             "Happily Detached", "Unhappily Dedicated",
             "Minimally Engaged", "Disengaged")
)
eng_state_data <- eng_state_data %>%
  group_by(ORGID) %>%
  mutate(Percent = round(Employees / sum(Employees) * 100)) %>%
  arrange(ORG, Engagement.State)

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
    geom_point(aes(size = Percent, stat = "identity"), 
               shape = 21, colour = "black") +
    scale_size_area(max_size = 50, guide = FALSE) + 
    xlim(0, 120) + ylim(0, 120) +
    scale_fill_manual(values = PLOT_COLS) + 
    
    theme_bw(20) +
    guides(fill = guide_legend("Engagement State", override.aes = list(size = 4))) +
    theme(legend.key = element_blank(), panel.grid.minor = element_blank(),
          legend.key.height = unit(1.5, "line"))
}

# Shiny app
ui <- fluidPage(
  h1("WES Engagement"),
  conditionalPanel(
    condition = "input.main_nav != 'tab_info'",
    selectInput("org_select", "Organization:",
                choices = c("All" = "all",
                  setNames(unique(eng_state_data$ORGID) %>% as.character(), unique(eng_state_data$ORG) %>% as.character())
                )
    )
  ),
  tabsetPanel(
    id = "main_nav",
    tabPanel(
      paste0("Engagement state ", YEAR_END),
      value = "tab_engagement",
      plotOutput("engagement_plot"),
      tableOutput("engagement_table")
    ),
    tabPanel(
      sprintf("Migration analysis %s-%s", YEAR_START, YEAR_END),
      value = "tab_migration",
      "[TODO]"
    ),
    tabPanel(
      "Methods",
      value = "tab_info",
      "[TODO]"
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- eventReactive(input$org_select, {
    if (input$org_select == "all") {
      eng_state_data
    } else {
      eng_state_data %>%
        filter(ORGID == input$org_select)
    }
  })
  
  filtered_data_agg <- eventReactive(filtered_data(), {
    data <- filtered_data() %>%
      dplyr::select_("Engagement.State", "Satisfaction", "Commitment",
                     "Employees", "Percent")
    
    if (input$org_select != "all") {
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

  output$engagement_table <- renderTable({
    filtered_data()
  })
  
  output$engagement_plot <- renderPlot({
    engagement_plot(filtered_data_agg())
  })
}

shinyApp(ui = ui, server = server)
