source("server-helpers.R")

function(input, output, session) {
  
  # -------- Tab 1 - Engagement state ----------
  
  # Get engagement data filtered by chosen organization
  engagement_data <- reactive({
    if (input$engagement_org == "all") {
      eng_state_data
    } else {
      dplyr::filter(eng_state_data, Org.ID == input$engagement_org)
    }
  })
  
  # Get aggregated engagement data
  engagement_data_agg <- eventReactive(engagement_data(), {
    data <- dplyr::select(engagement_data(), -Org.ID, -Organization)

    if (input$engagement_org != "all") {
      data
    } else {
      engagement_agg(data)
    }
  })
  
  # Knit and download a report based on the selected organization  
  output$btn_report <- downloadHandler(
    filename = "BC-WES-report.html",
    content = function(file) {
      tmpFile <- tempfile(tmpdir = ".", fileext = ".html")
      
      params <- list(Organization = input$engagement_org)
      
      rmarkdown::render("DynamicShinyReportTemplate.Rmd", output_file = tmpFile,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      file.rename(tmpFile, file)
    }
  )
  
  # The engagement data in the format that should be shown to the user visually
  engagement_data_view <- reactive({
    engagement_data() %>%
      dplyr::select(-c(Org.ID, Commitment.2013, Commitment.2015,
                       Satisfaction.2013, Satisfaction.2015))
  })
  
  output$engagement_table <- DT::renderDataTable({
    data <- engagement_data_view()
    
    if (input$engagement_org == "all") {
      dom <- "tlp"
    } else {
      dom <- "t"
      data <- dplyr::select(data, -Organization)
    }
    
    data$Employees.2013 <- format(data$Employees.2013, big.mark = ",")
    data$Employees.2015 <- format(data$Employees.2015, big.mark = ",")
    
    DT::datatable(
      data,
      rownames = FALSE,
      colnames = gsub("\\.", " ", colnames(data)),
      selection = 'none',
      class = 'stripe',
      options = list(
        searching = FALSE, paging = TRUE,
        scrollX = TRUE, scrollY = FALSE,
        dom = dom,
        scrollCollapse = TRUE
      )
    ) %>%
    formatString(columns = c("Percent.2013", "Percent.2015"), suffix = "%")
  })
  
  output$engagement_plot_13 <- renderPlot({
    p <- engagement_plot(engagement_data_agg(), 2013)
    p
    #ggiraph(code = print(p), width_svg = 15, height_svg = 10)
  })
  output$engagement_plot_15 <- renderPlot({
    p <- engagement_plot(engagement_data_agg(), 2015)
    p
    #ggiraph(code = print(p), width_svg = 15, height_svg = 10)
  })
  
  output$engagement_data_download <- downloadHandler(
    filename = function() {
      paste0("engagement_data_", input$engagement_org, ".csv")
    },
    content = function(file) {
      write.csv(engagement_data_view(), file, row.names = FALSE, quote = TRUE)
    }
  )
  
  # -------- Tab 2 - Migration analysis ---------
  
  # Every time a new org is chosen, create MOCK migration data
  migration_data <- reactive({
    get_migration_data(input$migration_org)
   }) 
  
  output$migration_table <- DT::renderDataTable({
    data <- migration_data()
    
    data$Employees <- format(data$Employees, big.mark = ",")
    
    DT::datatable(
      data,
      rownames = FALSE,
      colnames = gsub("\\.", " ", colnames(data)),
      selection = 'none',
      class = 'stripe',
      options = list(
        pageLength = nrow(data),
        searching = FALSE,
        scrollX = TRUE, scrollY = FALSE,
        dom = 't',
        scrollCollapse = TRUE
      )
    ) %>%
    formatString(columns = c("Percent.of.2015"), suffix = "%")
  })
  
  output$migration_plot <- renderSankeyNetwork({
    data <- migration_data()
    migration_plot(data)
  })

  output$migration_data_download <- downloadHandler(
    filename = function() {
      paste0("migration_data_", input$migration_org, ".csv")
    },
    content = function(file) {
      write.csv(migration_data(), file, row.names = FALSE, quote = TRUE)
    }
  )
  
  # -------- Tab 3 - Methods ---------
  
  engageMatrix <-matrix(c("Happily detached",
                          "Engaged", 
                          "Disengaged", 
                          "Unhappily dedicated"), ncol=2, byrow=TRUE)
  colnames(engageMatrix) <- c("Commitment < 60 points", "Commitment >= 60 points")
  rownames(engageMatrix) <- c("Satisfaction >= 60 points", "Satisfaction < 60 points")
  
  output$matrixTable <- DT::renderDataTable(engageMatrix,
                                            class = 'compact center',
                                            caption = htmltools::tags$caption(
                                              style = 'text-align: center;',
                                              'Engagement Matrix'
                                            ),
                                            options=list(
                                               dom='Bt', # only show buttons and table, hence Bt
                                               columnDefs = list(
                                                 list(targets={{1}},orderable = FALSE),
                                                 list(targets={{2}},orderable = FALSE),
                                                 list(targets={{1}},class = 'dt-center'),
                                                 list(targets={{2}},class = 'dt-center')
                                                                )
                                                        )
                                        )
}