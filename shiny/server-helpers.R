# Transform the full data to a dataset useful for a migration sankey plot
get_migration_data <- function(org) {
  if (org == "all") {
    migration_data <- migration_data_full
    migration_data$ENGSTATE13[is.na(migration_data$ORGID13)] <- "Not in Population"
    migration_data$ENGSTATE15[is.na(migration_data$ORGID15)] <- "Not in Population"
  } else {
    migration_data <- dplyr::filter(migration_data_full,
                                    ORGID15 == org | ORGID13 == org)
    idx_na13 <- migration_data$ORGID13 != org | is.na(migration_data$ORGID13)
    idx_na15 <- migration_data$ORGID15 != org | is.na(migration_data$ORGID15)
    migration_data$ENGSTATE13[idx_na13] <- "Not in Population"
    migration_data$ENGSTATE15[idx_na15] <- "Not in Population"
  }

  migration_data$ENGSTATE13[is.na(migration_data$ENGSTATE13)] <- "Non Respondent"
  migration_data$ENGSTATE15[is.na(migration_data$ENGSTATE15)] <- "Non Respondent"
  
  migration_data <- migration_data %>%
    dplyr::group_by(ENGSTATE13, ENGSTATE15) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      Engagement.2013 = ENGSTATE13,
      Engagement.2015 = ENGSTATE15,
      Employees = n
    ) %>%
    dplyr::group_by(Engagement.2015) %>%
    dplyr::mutate(Percent.of.2015 = round(Employees / sum(Employees) * 100)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Engagement.2015)

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
    ggtitle(year)
}

# Aggregate engagement data from multiple organizations into one summary
engagement_agg <- function(data) {
  data %>% group_by_("Engagement.State") %>%
    summarise(
      Satisfaction.2013 = round(weighted.mean(Satisfaction.2013, Employees.2013), 1),
      Commitment.2013 = round(weighted.mean(Commitment.2013, Employees.2013), 1),
      Employees.2013 = sum(Employees.2013),
      Satisfaction.2015 = round(weighted.mean(Satisfaction.2015, Employees.2015), 1),
      Commitment.2015 = round(weighted.mean(Commitment.2015, Employees.2015), 1),
      Employees.2015 = sum(Employees.2015)
    ) %>%
    mutate(Percent.2013 = round(Employees.2013 / sum(Employees.2013) * 100)) %>%
    mutate(Percent.2015 = round(Employees.2015 / sum(Employees.2015) * 100)) %>%
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