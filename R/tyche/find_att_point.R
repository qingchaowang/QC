read_data <- function(filename, start_date, game_pred, N) {
  library(magrittr)
  library(dplyr)
  my_data <- read.table(filename) %>%
    set_colnames(c("iter", "date", "game", "amount", "currency")) %>%
    mutate(date = as.Date(date, "%Y-%b-%d")) %>%
    filter(date >= start_date, game_pred(game))
  
  all_iter <- 0:(N-1)
  iter <- unique(my_data$iter)
  missing_iter <- all_iter[!(all_iter%in% iter)]
  iter <- c(iter, missing_iter)
  
  my_data <- tapply(my_data$amount, my_data$iter, sum)
  my_data <- c(my_data, rep(0, N - length(my_data)))
  
  my_data[order(iter)]
}

find_att_point <- function(ground_up_losses, capacities, target_el) {
  
  n_layers <- length(capacities)
  att_points <- c(0, cumsum(capacities))[-(n_layers + 1)]
  found_solution <- FALSE
  
  el_func <- function(att_point, capacity) {
    mean(pmin(capacity, pmax(0, ground_up_losses - att_point)))
  }

  while(!found_solution) {
    
    current_el <- mapply(el_func, att_points, capacities)/capacities
    found_solution <- all(current_el < target_el)
    if(!found_solution) att_points <- att_points + 100000
  }
  
  att_points
}

find_att_bounds <- function(ground_up_losses) {
  
  upper <- find_att_point(ground_up_losses, c(30, 30, 30)*1e6, c(15.6, 8.91, 4.74)/100)
  lower <- find_att_point(ground_up_losses, c(30, 30, 30)*1e6, c(17.5, 10.5, 5.75)/100)
  cbind(lower, upper)
}

find_prem <- function(ground_up_losses, retention, iROL, iEL) {
  
  layers <- list(
    list(att_point = retention, capacity = 30e6),
    list(att_point = retention + 30e6, capacity = 30e6),
    list(att_point = retention + 60e6, capacity = 30e6)
  )
  
  uEL <- sapply(layers, function(l) {
    mean(pmin(pmax(ground_up_losses - l$att_point, 0), l$capacity))/l$capacity
  })
  
  sum(iROL + 1.35*(uEL - iEL))*30e6*1.12
}

reset_pred <- function(x){x %in% c("glp", "emp", "ejp")}
# dot_95 <- read_data("~/Work/Projects/tyche_reset/results_com_95/all_losses.txt", "2019-1-1", reset_pred, 1e6)
# dot_102 <- read_data("~/Work/Projects/tyche_reset/results_com_102/all_losses.txt", "2019-1-1", reset_pred, 1e6)
# all_95 <- read_data("~/Work/Projects/tyche_reset/reset_losses/all_losses.txt", "2019-1-1", reset_pred, 1e6)
# all_102 <- read_data("~/Work/Projects/tyche_reset/results_all_102/all_losses.txt", "2019-1-1", reset_pred, 1e6)
# 
# dot_95_bounds <- find_att_bounds(dot_95)
# dot_102_bounds <- find_att_bounds(dot_102)
all_95 <- read_data("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/to_del/all_losses.txt", "2019-1-1", reset_pred, 1e6)
all_95_bounds <- find_att_bounds(all_95)
all_95_bounds
# all_102_bounds <- find_att_bounds(all_102)
# 
# all_pred <- function(x) {
#   rep(TRUE, length(x))
# }
# 
# dot_95 <- read_data("~/Work/Projects/tyche_reset/reset_dot_95/all_losses.txt", "2019-1-1", all_pred, 1e6)
# dot_102 <- read_data("~/Work/Projects/tyche_reset/reset_dot_102/all_losses.txt", "2019-1-1", all_pred, 1e6)
# all_95 <- read_data("~/Work/Projects/tyche_reset/reset_all_95/all_losses.txt", "2019-1-1", all_pred, 1e6)
# all_102 <- read_data("~/Work/Projects/tyche_reset/reset_all_102/all_losses.txt", "2019-1-1", all_pred, 1e6)
# 
# iROL <- c(24.25, 14.75, 9)/100
# iEL <- c(15.6, 8.91, 4.74)/100
# sims <- list(dot_95, dot_102, all_95, all_102)
# bounds <- list(dot_95_bounds, dot_102_bounds, all_95_bounds, all_102_bounds)
# mapply(function(s, b){
#   c(
#     find_prem(s, b[1,1], iROL, iEL),
#     find_prem(s, b[1,2], iROL, iEL)
#   )
# }, sims, bounds)


