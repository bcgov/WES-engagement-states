source("server-helpers.R")

function(input, output, session) {
  
  # -------- Tab 1 - Engagement state ----------
  
  # Get engagement data filtered by chosen organization
  engagement_data <- eventReactive(input$engagement_org, {
    if (input$engagement_org == "all") {
      eng_state_data
    } else {
      dplyr::filter(eng_state_data, Org.ID == input$engagement_org)
    }
  })
  
  # Get aggregated engagement data
  engagement_data_agg <- eventReactive(engagement_data(), {
    data <- dplyr::select_(
      engagement_data(),
      "Engagement.State", "Satisfaction", "Commitment",
      "Employees", "Percent")
    
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
      dplyr::select_("Org", "Engagement.State", "Satisfaction", "Commitment",
                     "Employees", "Percent")
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
  
  output$engagement_plot <- renderPlot({
    p <- engagement_plot(engagement_data_agg())
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
    org <- input$migration_org
    
    migration_data <- dplyr::filter(migration_data_full,
                                    ORGID15 == org | ORGID13 == org)
    idx_na13 <- migration_data$ORGID13 != org | is.na(migration_data$ORGID13)
    idx_na15 <- migration_data$ORGID15 != org | is.na(migration_data$ORGID15)
    migration_data$ENGSTATE13[idx_na13] <- "N/A"
    migration_data$ENGSTATE15[idx_na15] <- "N/A"
    migration_data$ENGSTATE13[is.na(migration_data$ENGSTATE13)] <- "Incomplete"
    migration_data$ENGSTATE15[is.na(migration_data$ENGSTATE15)] <- "Incomplete"
    
    migration_data <- migration_data %>%
      dplyr::group_by(ENGSTATE13, ENGSTATE15) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        engagement_past = ENGSTATE13,
        engagement_current = ENGSTATE15,
        num = n
      )
    migration_data
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