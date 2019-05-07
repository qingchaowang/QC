controls_module_ui <- tagList(
  fileInput(inputId = "ms_file", label = "Select Monitoring Sheet"),
  dateRangeInput(inputId = "dates", label = "Select date range", start = "2018-01-01", end = Sys.Date()),
  numericInput(inputId = "mc_size", label = "Select Monte Carlo size", value = 1e5),
  numericInput(inputId = "seed", label = "Select random seed (blank for random)", value = NULL),
  actionButton(inputId = "run_btn", label = "Run")           
)

controls_module <- function(layers) {
  
  app_data <- reactiveValues(command = NULL, run = NULL)
  
  server_function <- function(input, output, session) {
    
    observeEvent(input$run_btn, {
      
      tmp <- sapply(layers$selector[-1], function(id){
        paste(input[[NS(id)("layer")]], input[[NS(id)("att_point")]], input[[NS(id)("capacity")]])
      })
      
      tmp <- paste(tmp, collapse = " ")
      
      app_data$command <- list(
        command = "/Users/georgios.sermaidis/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/Tyche-tests",
        args =   paste(input$ms_file[1,]$datapath, input$dates[1], input$dates[2], input$mc_size, ifelse(is.na(input$seed), "$RANDOM", input$seed), "con_gel.txt", tmp)
      )
      
      app_data$run <- input$run_btn
    })
  }
  
  list(data = app_data, server = server_function)
}