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
        fill = Engagement.State, colour = Engagement.State)) +
    # cross-hairs
    geom_segment(x=mean_commitment,xend=mean_commitment,y=0,yend=mean_satisfaction, col = "#7F7F7F",linetype="dashed") +
    geom_segment(x=0,xend=mean_commitment,y=mean_satisfaction,yend=mean_satisfaction, col = "#7F7F7F",linetype="dashed") +
    geom_point(aes(size = Percent), shape = 21, colour = "black",
               show.legend = FALSE) +
    coord_fixed() +
    scale_size_continuous(limits = c(1,100), range = c(5,60), guide = FALSE) +
    # labels
    geom_text(aes(label=paste(data$Percent,"%",sep="")), fontface = "bold") +
    scale_colour_manual(values=FONT_COLS) +
    guides(colour=FALSE) +
    # axis
    scale_x_continuous(breaks=seq(0,100,10), limits=c(0,125), expand=c(0,0)) +
    scale_y_continuous(breaks=seq(0,100,10), limits=c(0,125), expand=c(0,0)) +
    # axis lines
    geom_segment(x = 0,xend = 100, y = 0, yend = 0, colour = "black") +
    geom_segment(x = 0,xend = 0, y = 0, yend = 100, colour = "black") +
    scale_fill_manual(values = PLOT_COLS) +
    theme_classic() +
    ggtitle(year) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.key = element_blank(),
          axis.line = element_blank(),
          axis.title = element_text(hjust = 0.4))
    
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
  colours_js_d3 <- sprintf('d3.scaleOrdinal().range([%s])', colours_js)
  
  sankeyNetwork(Links = data, Nodes = states_df,
                Source = 'Engagement.2013', Target = 'Engagement.2015',
                Value = 'Employees',
                NodeID = 'state', fontSize = 22, fontFamily = "Adobe-Garamond-Pro",
                nodeWidth = "20", nodePadding = "10", iterations = 0,
                NodeGroup = 'state', colourScale = JS(colours_js_d3))
}
