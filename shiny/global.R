library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(networkD3)

# Define the possible engagement states
STATES <- c("Engaged", "Dedicated", "Detached",
            "Disengaged", "Incomplete", "N/A")

# Constants to use in the code
PLOT_COLS <- c("#325A80", "#5091CD", "#FFFF05", "#D2BE32", "#FA1E1E", "#A40000")

# Read and set up the engagement data
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

# Extract a mapping with all organization ids -> names
org_names_list <-
  c("All" = "all",
    setNames(
      eng_state_data$ORGID %>% unique() %>% as.character(),
      eng_state_data$ORG %>% unique() %>% as.character()
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
    guides(fill = guide_legend("Engagement State",
                               override.aes = list(size = 4))) +
    theme(legend.key = element_blank(), panel.grid.minor = element_blank(),
          legend.key.height = unit(1.5, "line"))
}

# Aggregated engagement data from multiple organizations into one summary
engagement_agg <- function(data) {
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

# Create MOCK migration data
migration_data_mock <- function() {
  expand.grid(eng_past = STATES, eng_current = STATES,
              KEEP.OUT.ATTRS = FALSE) %>%
    arrange(eng_past, eng_current) %>%
    mutate(num = round(runif(nrow(.), 10, 250)))
}

# Code for migration Sankey plot
migration_plot <- function(data) {
  data$eng_past <- as.integer(data$eng_past) - 1
  data$eng_current <- as.integer(data$eng_current) - 1 + length(STATES)
  states_df <- data.frame(state = c(STATES, STATES))
  
  sankeyNetwork(Links = data, Nodes = states_df,
                Source = 'eng_past', Target = 'eng_current', Value = 'num',
                NodeID = 'state', fontSize = 22, fontFamily = "Arial",
                nodeWidth = "20", nodePadding = "10", iterations = 0)
}