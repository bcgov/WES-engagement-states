library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(networkD3)
#library(ggiraph)
library(readr)

# Consistent colour scheme to use in plots
PLOT_COLS <- c("#325A80", "#5091CD", "#FFFF05", "#D2BE32", "#FA1E1E", "#A40000")

ENG_STATES_FULL <- c(
  "Engaged", "Moderately Engaged", "Unhappily Dedicated",
  "Happily Detached", "Minimally Engaged", "Disengaged"
)

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
org_names_list <- unique(as.character(WES_data$ORGID15))
names(org_names_list) <- unique(as.character(WES_data$ORGANIZATION15))
org_names_list <- org_names_list[!is.na(org_names_list)]
org_names_list <- sort(org_names_list)
org_names_list <- c("All" = "all", org_names_list)

# ------------ Set up data for the engagement state bubble plot ---------

# TODO get rid of this duplicated code - it has to be abstracted away!
eng_state_data_13 <- WES_data %>%
  dplyr::select(ORGID13, ORGANIZATION13, ENGSTATE13, COMMITMENT13, SAT13) %>%
  dplyr::filter(!is.na(ENGSTATE13)) %>%
  dplyr::group_by(ORGID13, ORGANIZATION13, ENGSTATE13) %>%
  dplyr::summarise(
    COMMITMENT13 = round(mean(COMMITMENT13), 1),
    SAT13 = round(mean(SAT13), 1),
    Employees = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ORGID13) %>%
  dplyr::mutate(Percent = round(Employees / sum(Employees) * 100)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(ORGANIZATION13, ENGSTATE13) %>%
  dplyr::rename(
    Organization = ORGANIZATION13,
    Org.ID = ORGID13,
    Engagement.State = ENGSTATE13,
    Commitment = COMMITMENT13,
    Satisfaction = SAT13
  )
eng_state_data_15 <- WES_data %>%
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
    Organization = ORGANIZATION15,
    Org.ID = ORGID15,
    Engagement.State = ENGSTATE15,
    Commitment = COMMITMENT15,
    Satisfaction = SAT15
  )
eng_state_data <- suppressWarnings(
  dplyr::full_join(
    eng_state_data_13, eng_state_data_15,
    by = c("Org.ID", "Engagement.State"),
    suffix = c(".2013", ".2015")
  )) %>%
  dplyr::select(-Organization.2015) %>%
  dplyr::rename(Organization = Organization.2013)

# -------------- Set up the migration data ---------------

migration_data_full <- dplyr::select_(WES_data,
                                      "ORGID13", "ENGSTATE13",
                                      "ORGID15", "ENGSTATE15")
migration_data_full$ENGSTATE13 <- dplyr::recode_factor(
  migration_data_full$ENGSTATE13,
  "Moderately Engaged" = "Engaged",
  "Unhappily Dedicated" = "Dedicated",
  "Happily Detached" = "Detached",
  "Minimally Engaged" = "Disengaged"
)
migration_data_full$ENGSTATE15 <- dplyr::recode_factor(
  migration_data_full$ENGSTATE15,
  "Moderately Engaged" = "Engaged",
  "Unhappily Dedicated" = "Dedicated",
  "Happily Detached" = "Detached",
  "Minimally Engaged" = "Disengaged"
)
levels(migration_data_full$ENGSTATE13) <- c(
  levels(migration_data_full$ENGSTATE13), "Incomplete", "N/A")
levels(migration_data_full$ENGSTATE15) <- c(
  levels(migration_data_full$ENGSTATE15), "Incomplete", "N/A")
