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
    data <- dplyr::select(engagement_data(), -Org.ID, -Org)

    if (input$engagement_org != "all") {
      data
    } else {
      engagement_agg(data)
    }
  })
  
  # Knit and download a report based on the selected organization  
  output$report <- downloadHandler(
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
      dplyr::select(-Org.ID)
  })
  
  output$engagement_table <- DT::renderDataTable({
    data <- engagement_data_view()
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
  
  output$engagement_plot_13 <- renderPlot({
    p <- engagement_plot(engagement_data_agg(), 13)
    p
    #ggiraph(code = print(p), width_svg = 15, height_svg = 10)
  })
  output$engagement_plot_15 <- renderPlot({
    p <- engagement_plot(engagement_data_agg(), 15)
    p
    #ggiraph(code = print(p), width_svg = 15, height_svg = 10)
  })
  
  output$engagement_data_download <- downloadHandler(
    filename = function() {
      paste0("engagement_data_", input$engagement_org, ".csv")
    },
    content = function(file) {
      write.csv(engagement_data_view(), file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # -------- Tab 2 - Migration analysis ---------
  
  # Every time a new org is chosen, create MOCK migration data
  migration_data <- reactive({
    get_migration_data(input$migration_org)
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
    data <- migration_data()
    migration_plot(data)
  })

  output$migration_data_download <- downloadHandler(
    filename = function() {
      paste0("migration_data_", input$migration_org, ".csv")
    },
    content = function(file) {
      write.csv(migration_data(), file, row.names = FALSE, quote = FALSE)
    }
  )
}