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
