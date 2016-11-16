library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(networkD3)
library(ggiraph)

# -------------- Set up the migration data ---------------
migration_data_mock <- read.csv("data/migration_data.csv")

#--------------- Set up the engagement data ---------------
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
