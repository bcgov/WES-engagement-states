function(input, output, session) {
  filtered_data <- eventReactive(input$engagement_org, {
    if (input$engagement_org == "all") {
      eng_state_data
    } else {
      eng_state_data %>%
        filter(ORGID == input$engagement_org)
    }
  })
  
  filtered_data_agg <- eventReactive(filtered_data(), {
    data <- filtered_data() %>%
      dplyr::select_("Engagement.State", "Satisfaction", "Commitment",
                     "Employees", "Percent")
    
    if (input$engagement_org != "all") {
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
  
  output$engagement_table <- DT::renderDataTable({
    data <- filtered_data() %>%
      dplyr::select(ORG, Engagement.State, Satisfaction, Commitment,
                    Employees, Percent)
    DT::datatable(
      data,
      rownames = FALSE,
      selection = 'none',
      class = 'stripe',
      options = list(
        searching = FALSE, paging = TRUE,
        scrollX = TRUE, scrollY = FALSE,
        dom = 'tlp',
        scrollCollapse = TRUE
      )
    )
  })
  
  output$engagement_plot <- renderPlot({
    engagement_plot(filtered_data_agg())
  })
  
  # ------- Tab 2: Migration analysis
  
  # Every time a new org is chosen, create MOCK migration data
  migration_data <- reactive({
    input$migration_org
    
    migration <- expand.grid(eng_past = STATES, eng_current = STATES,
                             KEEP.OUT.ATTRS = FALSE) %>%
      arrange(eng_past, eng_current) %>%
      mutate(num = round(runif(nrow(.), 10, 250)))
    migration
  }) 
  
  output$migration_table <- DT::renderDataTable({
    data <- migration_data()
    
    DT::datatable(
      data,
      rownames = FALSE,
      selection = 'none',
      class = 'stripe',
      options = list(
        searching = FALSE, paging = TRUE,
        scrollX = TRUE, scrollY = FALSE,
        dom = 'tlp',
        scrollCollapse = TRUE
      )
    )
  })
  
  output$migration_plot <- renderSankeyNetwork({
    # Transform migration data to a more useful format for sankey diagrams
    migration_sankey <- migration_data()
    migration_sankey$eng_past <- as.integer(migration_sankey$eng_past) - 1
    migration_sankey$eng_current <- as.integer(migration_sankey$eng_current) - 1 +
      length(STATES)
    states_df <- data.frame(state = c(STATES, STATES))
    
    sankeyNetwork(Links = migration_sankey, Nodes = states_df,
                  Source = 'eng_past', Target = 'eng_current', Value = 'num',
                  NodeID = 'state', fontSize = 22, fontFamily = "Arial",
                  nodeWidth = "20", nodePadding = "10", iterations = 0)
  })
}