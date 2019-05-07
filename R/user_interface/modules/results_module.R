results_tabset <- function(input, selector) {
  
  tabs <- lapply(selector, function(id) {
    tabPanel(title = isolate({input[[NS(id)("layer")]]}),
             dataTableOutput(outputId = NS(id)("table"))
    )
  })
  
  tabs <- c(list(tabPanel(title = "Ground up", 
                          dataTableOutput(outputId = "gel_table"))), tabs)
  
  tagList(
    do.call(tabsetPanel, tabs),
    downloadButton(outputId = "download_btn")
  )
}

results_module_ui <- uiOutput(outputId = "tabset_ui")

results_module <- function(command_data, layers_data) {
  
  app_data <- reactiveValues(gel = NULL, layers = NULL)
  
  server_function <- function(input, output, session) {
    
    output$tabset_ui <- renderUI({
      
      req(command_data$run)
      
      isolate({
        
        system2(command_data$command[[1]], command_data$command[[2]])
        
        mapply(function(i, id) {
          output[[NS(id)("table")]] <- renderDataTable({
            req(app_data$layers[[i]])
            app_data$layers[[i]] %>%
              datatable(rownames = F) %>%
              formatPercentage(columns = 3:6, digits = 2)
          })
        }, 1:length(layers_data$selector[-1]), layers_data$selector[-1])
        
        app_data$gel <- sort_gel("con_gel.txt")
        
        app_data$layers <- read_metrics("con_metrics.txt")
        
        the_layers <- isolate({layers_data$selector[-1]})
        results_tabset(input, the_layers)
      })
    })
    
    output$gel_table <- renderDataTable({
      
      req(app_data$gel)
      
      app_data$gel %>%
        datatable(rownames = F) %>%
        formatPercentage(columns = 3, digits = 2) %>%
        formatCurrency(columns = 2)
    })
    
    output$download_btn <- downloadHandler(
      filename = function() {"metrics.xlsx"},
      content = function(file) {
        for(i in 1:length(app_data$layers))
          xlsx::write.xlsx(app_data$layers[[i]], file = file, 
                           sheetName = input[[NS(layers_data$selector[-1][i])("layer")]], append = T, row.names = F)
      }
    )
  }
  
  list(data = app_data, server = server_function)
}
