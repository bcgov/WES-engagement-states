# Constants to use in the code
PLOT_COLS <- c("#325A80", "#5091CD", "#FFFF05", "#D2BE32", "#FA1E1E", "#A40000")

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
    geom_point(aes(size = Percent), shape = 21, colour = "black") +
    scale_size_area(max_size = 50, guide = FALSE) + 
    xlim(0, 120) + ylim(0, 120) +
    scale_fill_manual(values = PLOT_COLS) +
    theme_bw(26) +
    guides(fill = guide_legend("Engagement State",
                               override.aes = list(size = 4))) +
    theme(legend.key = element_blank(), panel.grid.minor = element_blank(),
          legend.key.height = unit(1.5, "line"), legend.position = "right")
}

# Aggregate engagement data from multiple organizations into one summary
engagement_agg <- function(data) {
  data %>%
    group_by_("Engagement.State") %>%
    summarise(
      Satisfaction = round(weighted.mean(Satisfaction, Employees), 1),
      Commitment = round(weighted.mean(Commitment, Employees), 1),
      Employees = sum(Employees)
    ) %>%
    mutate(Percent = round(Employees / sum(Employees) * 100)) %>%
    arrange(Engagement.State)
}

# Code for migration Sankey plot
migration_plot <- function(data) {
  data <- as.data.frame(data)
  states_df <- data.frame(state = c(levels(data$engagement_past),
                                    levels(data$engagement_current)))
  
  data$engagement_past <- as.integer(data$engagement_past) - 1
  data$engagement_current <- as.integer(data$engagement_current) - 1 +
    length(unique(data$engagement_past))
  
  colours_js <- paste0('"', paste(PLOT_COLS, collapse = '","'), '"')
  colours_js_d3 <- sprintf('d3.scale.ordinal().range([%s])', colours_js)
  
  sankeyNetwork(Links = data, Nodes = states_df,
                Source = 'engagement_past', Target = 'engagement_current',
                Value = 'num',
                NodeID = 'state', fontSize = 22, fontFamily = "Arial",
                nodeWidth = "20", nodePadding = "10", iterations = 0,
                NodeGroup = 'state', colourScale = JS(colours_js_d3))
}