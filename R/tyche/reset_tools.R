calc.losses <- function(capacity, x, att.p) {
  pmin(capacity, pmax(x - att.p, 0))
}

# read a tab from a monitoring sheet and output a list where each element is a draw
# keep in mind that a draw may contain many derivatives/partners
read_monitoring_sheet_unit <- function(filename, sheet_index, game_name, start_row, start_date, end_date) {
  
  require(readxl)
  
  file <- read_excel(filename, sheet_index, skip = start_row - 1)[,c("date", "off risk", "shared winners", "function", "primary winners", "secondary winners", "ML24 jackpot (USD)", "bets", "odds")]
  colnames(file) <- c("date", "off.risk", "shared.winners", "f", "primary.winners", "secondary.winners", "jackpot", "bets", "odds")
  file[,1] <- as.Date(file[,1]$date)
  file$type <- game_name
  inc_rows <- which((file[,1] >= start_date) & (file[,1] < end_date))
  file <- file[inc_rows,]
  out <- split(file, file$date)
  
  if (sum(out[[length(out)]]$bets) == 0) {
    ele_delete_count <- which(lapply(out, function(x) sum(x$bets)) == 0)
    out[-ele_delete_count]
  } else
    out
}

# create an ordered (by date) list of draws by combining individual init_data objects
combine_monitoring_sheet_data <- function(list_of_data) {
  
  dates <- lapply(list_of_data, function(x){as.Date(names(x))})
  dates <- do.call(c, dates)
  k <- order(dates)
  do.call(c, list_of_data)[k]
}

read_monitoring_sheet <- function(filename, starting_row, start_date, end_date) {
  
  game_names <- readxl::excel_sheets(filename)
  game_names <- game_names[-length(game_names)]
  n_games <- length(game_names)
  
  out <- list()
  for (i in 1:n_games) {
    out[[i]] <- read_monitoring_sheet_unit(filename, i, game_names[i], starting_row, start_date, end_date)
  }
  out <- combine_monitoring_sheet_data(out)
  out
}

ground_up_el_per_game <- function(ms_data, sim_data) {
  
  game_per_draw <- sapply(ms_data, function(x){x$type[1]})
  el_per_draw <- apply(sim_data, 2, mean)
  tapply(el_per_draw, game_per_draw, sum)
}

ils_el_per_draw <- function(sim_data, tower) {
  
  cum_losses <- apply(sim_data, 1, cumsum)
  
  
  tower_metrics <- list()
  
  for(i in 1:length(tower)) {
    
    capacity <- tower[[i]]$capacity
    att_point <- tower[[i]]$att_point
    
    tower_metrics[[i]] <- apply(cum_losses, 1, function(x){ 
      l <- calc.losses(capacity, x, att_point)
      c(mean(l > 0), mean(l)/capacity, mean(l==capacity), sd(l)/mean(l))
      
    })
  }
  
  t(do.call(rbind, tower_metrics))
}

ils_el_per_draw_last <- function(sim_data, tower) {
  
  cum_losses <- apply(sim_data, 1, sum)
  
  
  tower_metrics <- list()
  
  for(i in 1:length(tower)) {
    
    capacity <- tower[[i]]$capacity
    att_point <- tower[[i]]$att_point
    
    # tower_metrics[[i]] <- apply(cum_losses, 1, function(x){ 
    l <- calc.losses(capacity, cum_losses, att_point)
    tower_metrics[[i]] <- c(mean(l > 0), mean(l)/capacity, mean(l==capacity), sd(l)/mean(l))
    
    # })
  }
  
  do.call(rbind, tower_metrics)
}

extract_metrics_on_date <- function(x, dates) {
  
  n <- length(dates)
  out <- list()
  
  for(i in 1:n) {
    
    k <- which(as.Date(rownames(x)) <= as.Date(dates[i]))
    k <- k[length(k)]
    ret <- x[k,]
    ret <- matrix(ret, nrow = 3, byrow = T)
    dimnames(ret) <- list(c("C", "B", "A"), c("AP", "EL", "EP", "CoV"))
    out[[i]] <- ret  
  } 
  
  out
}


sim_payouts <- function(cl, data, mc_size, winners_sim_f, transform) {
  
  winners_f <- winners_sim_f
  # assumes multinomial only // will not work when there is shield
  out <- parSapply(cl, data, function(x) {
    
    n <- nrow(x)
    # winners <- matrix(rbinom(n*mc_size, x$bets, 1/x$odds), ncol = mc_size)
    winners <- winners_f(x, mc_size)
    # winners <- matrix(rpois(n*mc_size, x$bets/x$odds), ncol = mc_size)
    divisor <- winners 
    
    total <- apply(winners, 2, sum)
    total <- matrix(rep(total, each = n), ncol = mc_size)
    is_shared <- x$shared.winners == "Y"
    
    pwinners_sec <- colSums(subset(winners, x$off.risk == "Y"))
    pwinners_sec <- matrix(rep(pwinners_sec, each = n), ncol = mc_size)
    pure_pwinners <- x$primary.winners - sum(subset(x$secondary.winners, x$off.risk == "Y"))
    
    max_div <- pmax(divisor, pure_pwinners + pwinners_sec)
    sum_div <- divisor + pure_pwinners
    is_max <- x$f == "MAX"
    divisor <- is_max*max_div + (!is_max)*sum_div
    divisor <- is_shared*divisor + (!is_shared)*winners
    pays <- x$jackpot * ifelse(winners, winners/divisor, 0)
    transform(pays)
  })
  
  out
}

monitoring_sheet_sim_subset <- function(x, end_date) {
  
  dates <- as.Date(names(x))
  k <- which(dates < end_date)
  x[k]
}

poisson_winners_sim <- function(draw, mc_size) {
  
  n <- nrow(draw)
  matrix(rpois(n*mc_size, draw$bets/draw$odds), ncol = mc_size)
}

manual_winners_sim <- function(draw, mc_size) {
  
  n <- nrow(draw)
  matrix(rep(c(1,0), times = mc_size), ncol = mc_size)
}


EL_graph <- function(break_point_list, risk_metrics) {
  el <- 100 * risk_metrics[, 2]
  plot(el, type = "n", cex = 0.5, pch = 16, las = 1, xlab = "", ylab = "Conditional Expected Loss (\\%)", axes = F, lwd = 2, xaxs = "i", yaxs = "i")
  
  for (i in 1:(ncol(risk_metrics)/4)) {
    el <- 100 * risk_metrics[, 4*(i-1)+2]
    points(el, type = "l", cex = 0.5, pch = 16, col = i, lwd = 2)
  }
  
  abline(h = seq(0, 100, 1), col = "grey", lwd = 0.5)
  
  for (i in 1:length(break_point_list)) {
    abline(v = break_point_list[[i]], col = i, lty = 2)
  }
  
  axis(2, at = seq(0, 100, 0.5), cex.axis = 0.7, las = 1)
  axis(1, at = seq(0, 365, 365), col.tick = "white")
}

# reading the file everytime we want to extract a single value takes time
# now we read the file once and use the modelled_el_on_date function to extract values
read_modelled_el <- function(filename = "/Volumes/dfs/Hedging Management/001_Strategic/001_Tyche/006_Analysis/Hoplon III Insurance Ltd - Data File - November 2017.xlsx") {
  
  require(readxl)
  modelled_risk_metrics <- read_excel(filename, sheet = "KeyMetricsPerDay", skip = 11)
  modelled_risk_metrics[c("day", "el_ClassC", "el_ClassB", "el_ClassA")]
}

modelled_el_on_date <- function(x, date) {
  
  ret <- subset(x, as.Date(x$day) == as.Date(date))
  unlist(ret)[-1]
}

rol_actual <- function(conditional_el, el_i, rol_i, el_factor) {
  
  out <- rol_i + el_factor * (conditional_el - el_i)
  names(out) <- c("ROL_ClassC", "ROL_ClassB", "ROL_ClassA")
  out
}

adjustment_prem <- function(conditional_el, el_i, el_it, min_rol_multiple, rol_i, el_factor) {
  
  el_min <- el_i - rol_i * (1 - min_rol_multiple) / el_factor
  el_mint <- el_it * el_min / el_i
  out <- pmax(0, el_factor * (conditional_el - el_mint)) * 30e6
  names(out) <- c("AP_ClassC", "AP_ClassB", "AP_ClassA")
  out
}

min_el_t <- function(modelled_el, min_rol_multiple, rol_i, el_factor, date, last_date) {
  
  el_i <- modelled_el_on_date(modelled_el, last_date)
  el_it <- modelled_el_on_date(modelled_el, date)
  
  el_min <- el_i - rol_i * (1 - min_rol_multiple) / el_factor
  el_it * el_min / el_i
}

calc_premium_metrics <- function(metrics_per_day, modelled_el, date, last_date = "2018-12-31", rol_i = c(.2425, .1475, .09), el_factor = 1.35, min_rol_multiple = 0.75) {
  
  conditional_metrics <- extract_metrics_on_date(metrics_per_day, date)[[1]]
  el_i <- modelled_el_on_date(modelled_el, last_date)
  el_i_t <- modelled_el_on_date(modelled_el, date)
  list(
    conditional_layer_metrics = conditional_metrics,
    el_premium = sum(conditional_metrics[,2]*1.35)*30e6,
    actual_rol = rol_actual(conditional_metrics[,2], el_i, rol_i, el_factor),
    actual_prem = sum(rol_actual(conditional_metrics[,2], el_i, rol_i, el_factor))*30e6,
    adj_prem = sum(adjustment_prem(conditional_metrics[,2], el_i, el_i_t, min_rol_multiple, rol_i, el_factor)),
    min_el = min_el_t(modelled_el, min_rol_multiple, rol_i, el_factor, date, last_date)
  )
}