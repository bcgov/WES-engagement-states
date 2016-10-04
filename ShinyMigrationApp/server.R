library(shiny)
library(dplyr)
library(rmarkdown)
library(DT) # using the more advanced Datatable package

function(input, output) {
  
  output$EngagementTable <- DT::renderDataTable(CombinedStates1 %>% 
                                             filter(ORGID15==input$Organization),
                                             colnames = c('Organization ID','Engagement',
                                                          'Count 2013','Count 2015',
                                                          'Net Gain/Loss','% Change',
                                                          'Organization'), # headers
                                             rownames = FALSE,
                                             extensions = 'Buttons',
                                             class = 'cell-border stripe hover', #styling opts.
                                             options=list(
                                               dom='Bt ', # only show buttons and table, hence Bt
                                               buttons = 
                                                 list('copy', 'print', list(
                                                   extend = 'collection',
                                                   buttons = c('csv', 'excel', 'pdf'),
                                                   text = 'Download Data'
                                                 )),
                                               columnDefs = list(
                                                 list(targets={{0}},visible = FALSE),
                                                 list(targets={{6}},visible = FALSE)
                                                 ) # hide first and last columns
                                             ) 
  ) 
  
  output$downloadFile <- downloadHandler( # creating a download button to download already-existing organization specific reports
    filename = function(){
      paste("report",input$Organization,".html",sep='') # defining the filename for the file that users will download
      },
    content = function(file){
      file.copy(
        paste(
          "C:/RProjects/MigrationAnalysis/report",input$Organization,".html",sep=""), file) # filename to find on the server
      }
    )
  
  output$orgName <- renderText( # displaying reactive output
    {
      
      paste(CombinedStates1 %>% filter(ORGID15==input$Organization) %>% 
              select(ORGANIZATION15) %>% distinct()) # creating a defacto header for the table, with the organization name
      
    }
  )
  
  # Download dynamically generated reports  
  
  output$generateReport <- downloadHandler(
    # For PDF output, change this to "xxx.pdf"
    filename = function(){
      paste("dynamicReport",input$Organization,".html",sep='') # defining the filename for the file that users will download
    },
    content = function(file) {
      
      # Knit the document
      rmarkdown::render("C:/RProjects/MigrationAnalysis/DynamicShinyReportTemplate.Rmd",output_file = file)
      
    }
  )
  
  
}


