library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(networkD3)
#library(ggiraph)
library(readr)

ENG_STATES_FULL <- c(
  "Disengaged", "Minimally Engaged", "Happily Detached",
  "Unhappily Dedicated", "Moderately Engaged", "Engaged"
)

STATES <- c("Engaged", "Dedicated", "Detached",
            "Disengaged", "Incomplete", "N/A")

# ---------------  Read and pre process the full data --------------

data_file <- "data/finalMockData.csv"
WES_data <- read_csv(
  data_file, 
  col_types = cols(COMMITMENT13 = col_number(), 
                   COMMITMENT15 = col_number(),
                   SAT13 = col_number(), 
                   SAT15 = col_number(),
                   ENGSTATE13 = col_factor(ENG_STATES_FULL),
                   ENGSTATE15 = col_factor(ENG_STATES_FULL))
)
WES_data$ORGID13 <- as.factor(WES_data$ORGID13)
WES_data$ORGID15 <- as.factor(WES_data$ORGID15)
WES_data$ORGANIZATION13 <- as.factor(WES_data$ORGANIZATION13)
WES_data$ORGANIZATION15 <- as.factor(WES_data$ORGANIZATION15)

# Extract a mapping with all organization ids -> names
org_names_list <-
  c("All" = "all",
    setNames(levels(WES_data$ORGID15), levels(WES_data$ORGANIZATION15)))

# ------------ Set up data for the engagement state bubble plot ---------

eng_state_data <- WES_data %>%
  dplyr::select(ORGID15, ORGANIZATION15, ENGSTATE15, COMMITMENT15, SAT15) %>%
  dplyr::filter(!is.na(ENGSTATE15)) %>%
  dplyr::group_by(ORGID15, ORGANIZATION15, ENGSTATE15) %>%
  dplyr::summarise(
    COMMITMENT15 = round(mean(COMMITMENT15), 1),
    SAT15 = round(mean(SAT15), 1),
    Employees = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ORGID15) %>%
  dplyr::mutate(Percent = round(Employees / sum(Employees) * 100)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(ORGANIZATION15, ENGSTATE15) %>%
  dplyr::rename(
    Org = ORGANIZATION15,
    Org.ID = ORGID15,
    Engagement.State = ENGSTATE15,
    Commitment = COMMITMENT15,
    Satisfaction = SAT15
  )


# -------------- Set up the migration data ---------------
migration_data_mock <- read.csv("data/migration_data.csv")
