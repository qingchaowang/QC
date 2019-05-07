unit_id <- function(id) {
  NS(id)("unit")
}

hashed_unit_id <- function(id) {
  paste0("#", unit_id(id))
}

layer_config_ui_unit <- function(id) {
  
  ns <- NS(id)
  
  div(id = unit_id(id),
    wellPanel(
      splitLayout(cellWidths = c("30%", "30%", "30%", "10%"), 
                  textInput(inputId = ns("layer"), label = "Layer name", placeholder = "Layer name"),
                  numericInput(inputId = ns("att_point"), label = "Attachment Point", value = 30e6),
                  numericInput(inputId = ns("capacity"), label = "Capacity", value = 30e6),
                  div(actionButton(inputId = ns("rm_btn"), label = "", icon = icon("times"), onclick = paste("Shiny.setInputValue('remove_id',", id, ")"), 
                                   title = "Delete layer"), align = "right")
      )
    ) 
  )
}

layer_module_ui <- tagList(
  
  actionButton(inputId = "add_input", label = "Add layer"),
  br(),br(),
  tags$div(id = "0-unit")
)

layer_module <- function() {
  
  app_data <- reactiveValues(selector = 0)
  
  layer_server <- function(input, output, session) {
    
    observeEvent(input$add_input, {
      
      id <- input$add_input
      h_unit_id <- hashed_unit_id(tail(app_data$selector, n=1))
      
      insertUI(selector = h_unit_id, where = "afterEnd",
               ui = layer_config_ui_unit(id)
      )
      
      app_data$selector <- c(app_data$selector, id)
    })
    
    observeEvent(input$remove_id, {
      removeUI(selector = hashed_unit_id(input$remove_id))
      app_data$selector <- app_data$selector[-which(app_data$selector == input$remove_id)]
    })
  }
  
  list(server = layer_server, data = app_data)
}


