function(input, output, session) {
  
  # -------- Tab 1 - Engagement state ----------
  
  # Get engagement data filtered by chosen organization
  engagement_data <- eventReactive(input$engagement_org, {
    if (input$engagement_org == "all") {
      eng_state_data
    } else {
      eng_state_data %>%
        filter(ORGID == input$engagement_org)
    }
  })
  
  # Get aggregated engagement data
  engagement_data_agg <- eventReactive(engagement_data(), {
    data <- engagement_data() %>%
      dplyr::select_("Engagement.State", "Satisfaction", "Commitment",
                     "Employees", "Percent")
    
    if (input$engagement_org != "all") {
      data
    } else {
      engagement_agg(data)
    }
  })
  
  output$engagement_table <- DT::renderDataTable({
    data <- engagement_data() %>%
      dplyr::select_("ORG", "Engagement.State", "Satisfaction", "Commitment",
                     "Employees", "Percent")
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
    engagement_plot(engagement_data_agg())
  })
  
  # -------- Tab 2 - Migration analysis ---------
  
  # Every time a new org is chosen, create MOCK migration data
  migration_data <- reactive({
    input$migration_org
    migration_data_mock()
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
    data <- migration_data()
    migration_plot(data)
  })
}