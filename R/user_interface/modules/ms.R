library(shiny)
library(shinythemes)
library(DT)
library(shinycssloaders)

source("~/GitHub/R/user_interface/modules/layers_config.R")
source("~/GitHub/R/user_interface/modules/controls_module.R")
source("~/GitHub/R/user_interface/modules/results_module.R")

ui <- fluidPage(h1("Monitoring Sheet Analysis"),
                
                fluidRow(
                  column(width = 2,
                         wellPanel(controls_module_ui)
                  ),
                  
                  column(width = 4, 
                         wellPanel(layer_module_ui)
                  ),
                  
                  column(width = 6,
                         results_module_ui %>% withSpinner()
                  ))
)

server <- function(input, output, session) {
  
  layers <- layer_module()
  command <- controls_module(layers$data)
  results <- results_module(command$data, layers$data)
  
  layers$server(input, output, session)
  command$server(input, output, session)
  results$server(input, output, session)
}

sort_gel <- function(file_name) {
  
  x <- read.table("con_gel.txt")[,-3]
  total_el <- sum(x[,2])
  x$pct <- x[,2]/total_el
  x <- x[order(x[,2], decreasing = T),]
  colnames(x) <- c("Game", "Ground up EL", "% of Total")
  x
}

read_metrics <- function(file_name) {
  
  files <- system2("ls", stdout = TRUE)
  files <- files[grep(file_name, files)]  
  x <- lapply(files, read.table)
  
  y <- x[[1]]
  y[,-c(1:3)] <- 0
  for(i in 1:length(x)) y[,-c(1:3)] <- y[,-c(1:3)] + x[[i]][,-c(1:3)]
  y[,-c(1:3)] <- y[,-c(1:3)]/length(x)
  
  y[,1] <- as.Date(y[,1], "%Y-%b-%d")
  n_layers <- (ncol(y) - 2)/3
  
  out <- list()
  for(i in 1:n_layers) {
    
    result <- cbind(y[,1:2], y[,(3*i):(3*i+2)])
    colnames(result) <- c("Date", "Game", "AP", "EL", "EP")
    
    result$"dEL" <- c(result$EL[1], diff(result$EL))
    
    out[[i]] <- result[nrow(result):1, ]
    
  }
  
  out
}

shinyApp(ui, server)
