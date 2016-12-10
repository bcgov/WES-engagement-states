# Consistent colour scheme to use in plots
PLOT_COLS <- c("#325A80", "#5091CD", "#FFFF05", "#D2BE32", "#FA1E1E", "#A40000")

# Transform the full data to a dataset useful for a migration sankey plot
get_migration_data <- function(org) {
  if (org == "all") {
    migration_data <- migration_data_full
    migration_data$ENGSTATE13[is.na(migration_data$ORGID13)] <- "N/A"
    migration_data$ENGSTATE15[is.na(migration_data$ORGID15)] <- "N/A"
  } else {
    migration_data <- dplyr::filter(migration_data_full,
                                    ORGID15 == org | ORGID13 == org)
    idx_na13 <- migration_data$ORGID13 != org | is.na(migration_data$ORGID13)
    idx_na15 <- migration_data$ORGID15 != org | is.na(migration_data$ORGID15)
    migration_data$ENGSTATE13[idx_na13] <- "N/A"
    migration_data$ENGSTATE15[idx_na15] <- "N/A"
  }

  migration_data$ENGSTATE13[is.na(migration_data$ENGSTATE13)] <- "Incomplete"
  migration_data$ENGSTATE15[is.na(migration_data$ENGSTATE15)] <- "Incomplete"
  
  migration_data <- migration_data %>%
    dplyr::group_by(ENGSTATE13, ENGSTATE15) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      Engagement.2013 = ENGSTATE13,
      Engagement.2015 = ENGSTATE15,
      Employees = n
    )  
  
  migration_data
}

# Code for the engagement plot
engagement_plot <- function(data, year) {
  cols_keep <- grep(sprintf("\\.%s$", year), colnames(data), value = TRUE)
  cols_keep <- c("Engagement.State", cols_keep)
  data <- dplyr::select_(data, .dots = cols_keep)  
  colnames(data) <- gsub(sprintf("(.*)\\.%s$", year), "\\1", colnames(data))
  
  mean_satisfaction <- sum(data$Satisfaction * data$Percent / 100)
  mean_commitment <- sum(data$Commitment * data$Percent / 100)

  ggplot(
    data,
    aes(x = Commitment, y = Satisfaction,
        fill = Engagement.State)) +
    geom_vline(xintercept = mean_commitment, col = "#555555") +
    geom_hline(yintercept = mean_satisfaction, col = "#555555") +
    geom_point(aes(size = Percent), shape = 21, colour = "black",
               show.legend = FALSE) +
    scale_size_area(max_size = 50, guide = FALSE) + 
    xlim(0, 120) + ylim(0, 120) +
    scale_fill_manual(values = PLOT_COLS) +
    theme_bw(26) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste0("20", year))
}

# Aggregate engagement data from multiple organizations into one summary
engagement_agg <- function(data) {
  data %>% group_by_("Engagement.State") %>%
    summarise(
      Satisfaction.13 = round(weighted.mean(Satisfaction.13, Employees.13), 1),
      Commitment.13 = round(weighted.mean(Commitment.13, Employees.13), 1),
      Employees.13 = sum(Employees.13),
      Satisfaction.15 = round(weighted.mean(Satisfaction.15, Employees.15), 1),
      Commitment.15 = round(weighted.mean(Commitment.15, Employees.15), 1),
      Employees.15 = sum(Employees.15)
    ) %>%
    mutate(Percent.13 = round(Employees.13 / sum(Employees.13) * 100)) %>%
    mutate(Percent.15 = round(Employees.15 / sum(Employees.15) * 100)) %>%
    arrange(Engagement.State)
}

# Code for migration Sankey plot
migration_plot <- function(data) {
  data <- as.data.frame(data)
  states_df <- data.frame(state = c(levels(data$Engagement.2013),
                                    levels(data$Engagement.2015)))
  
  data$Engagement.2013 <- as.integer(data$Engagement.2013) - 1
  data$Engagement.2015 <- as.integer(data$Engagement.2015) - 1 +
    length(unique(data$Engagement.2013))
  
  colours_js <- paste0('"', paste(PLOT_COLS, collapse = '","'), '"')
  colours_js_d3 <- sprintf('d3.scale.ordinal().range([%s])', colours_js)
  
  sankeyNetwork(Links = data, Nodes = states_df,
                Source = 'Engagement.2013', Target = 'Engagement.2015',
                Value = 'Employees',
                NodeID = 'state', fontSize = 22, fontFamily = "Arial",
                nodeWidth = "20", nodePadding = "10", iterations = 0,
                NodeGroup = 'state', colourScale = JS(colours_js_d3))
}