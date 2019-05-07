
format_date <- function(date_vector) {
  as.Date(date_vector, "%Y-%b-%d")
}

read_losses <- function(x) {
  losses <- read.table(x)
  losses[,2] <- format_date(losses[,2])
  losses
}

layer_losses <- function(x, capacity, att_point) {
  pmin(capacity, pmax(x - att_point, 0))
}

split_losses_unit <- function(x, mc_size, start_date, end_date) {
  
  iteration <- x[,1]
  inc_flag <- (x[,2] >= start_date) & (x[,2] < end_date)
  x[,3] <- inc_flag * x[,3]
  out <- tapply(x[,3], iteration, sum)
  out <- cbind(unique(iteration), out)
  missing_iteration <- setdiff(0:(mc_size - 1), unique(iteration))
  missing_losses <- cbind(missing_iteration, 0)
  out <- rbind(out, missing_losses)
  out[order(out[,1]), ]
}

split_losses <- function(x, mc_size, ...) {
  
  x <- read_losses(x)
  out <- NULL
  date_vector <- c(...)
  n_dates <- length(date_vector)
  
  for(i in 1:(n_dates - 1)) {
    losses_unit <- split_losses_unit(x, mc_size, date_vector[i], date_vector[i+1])[,2]
    out <- cbind(out, losses_unit)
  }
  dimnames(out) <- NULL
  out
}

tyche_ROL <- function(x, tower, beta) {

  ROL <- rep(NA, 3)
  
  for(i in 1:3) {
    layer <- tower[[i]]
    layer_losses <- pmin(layer$capacity, pmax(x - layer$att_point, 0))/layer$capacity
    ROL[i] <- iROL[i] + beta*(mean(layer_losses) - iEL[i])
  }
    
  ROL
}


tyche_layer_cost <- function(layer, beta, iROL, iEL, ipt = 0.12) {
  
  function(ground_up_losses) {
    
    my_losses <- layer_losses(ground_up_losses, layer$capacity, layer$att_point)
    avg_losses <- mean(my_losses)/layer$capacity
    layer$capacity*(iROL + beta*(avg_losses - iEL))*(1 + ipt)
  }
}


ins_layer_cost <- function(layer, beta, md, ipt = 0.12) {
  
  function(ground_up_losses) {
    
    my_losses <- layer_losses(ground_up_losses, layer$capacity, layer$att_point)
    avg_losses <- mean(my_losses)/layer$capacity
    (1 + ipt)*max(md, avg_losses)*layer$capacity
  }
}


avg_xs_draws <- function(lb, ub, x) {
  
  mean(apply(x, 2, function(y){
    sum( (y >= lb) & (y < ub) )
  }))
}



run <- function(folder, N, eur_to_usd, tower_cost, capacity, ...) {
  
  cwd <- getwd()
  setwd(folder)
  
  stakes <- scan("gls_bet_count.txt")
  stakes <- stakes + scan("ems_bet_count.txt")*2.5
  stakes <- stakes + scan("ejs_bet_count.txt")*2 
  stakes <- stakes + scan("pbs_bet_count.txt")*3.5
  stakes <- stakes + scan("mms_bet_count.txt")*3.5
  
  lower_payouts <- scan("gls_lower.txt")
  lower_payouts <- lower_payouts + scan("ems_lower.txt")
  lower_payouts <- lower_payouts + scan("ejs_lower.txt")
  lower_payouts <- lower_payouts + scan("pbs_lower.txt")/eur_to_usd
  lower_payouts <- lower_payouts + scan("mms_lower.txt")/eur_to_usd
  
  ems_hedged <- read.table("ems_hedged.txt")
  pbs_hedged <- read.table("pbs_hedged.txt")
  mms_hedged <- read.table("mms_hedged.txt")
  
  ticket_cost <- ems_hedged[,1]*2.5
  ticket_cost <- ticket_cost + pbs_hedged[,1]*2.3/eur_to_usd
  ticket_cost <- ticket_cost + mms_hedged[,1]*2.3/eur_to_usd
  
  ticket_income <- ems_hedged[,2]
  ticket_income <- ticket_income + pbs_hedged[,2]/eur_to_usd
  ticket_income <- ticket_income + mms_hedged[,2]/eur_to_usd
  
  pb_jackpot <- t(read.table("pbp_jackpot.txt"))
  mm_jackpot <- t(read.table("mmp_jackpot.txt"))
  
  pb_xs_draws <- avg_xs_draws(153e6, capacity, pb_jackpot*0.372)
  mm_xs_draws <- avg_xs_draws(153e6, capacity, mm_jackpot*0.372)
  
  gu_losses <- ground_up_losses(folder, N, eur_to_usd, ...)
  ins_cost <- tower_cost(gu_losses)/eur_to_usd
  
  setwd(cwd)
  
  list(pb_xs_draws, mm_xs_draws, net_hedging = mean(ticket_cost - ticket_income), smp = mean(stakes - lower_payouts),
       ils_cost = ins_cost, total = mean(stakes - lower_payouts - ticket_cost + ticket_income - sum(ins_cost)))
}

ground_up_losses <- function(folder, N, eur_to_usd, ...) {
  
  cwd <- getwd()
  setwd(folder)
  
  eur_losses_data <- read_losses("eur_ils_losses.txt")
  usd_losses_data <- read_losses("usd_ils_losses.txt")
  eur_losses <- split_losses(eur_losses_data, N, eur_to_usd, ...)
  usd_losses <- split_losses(usd_losses_data, N, 1, ...)
  total_losses <- eur_losses + usd_losses
  
  setwd(cwd)
  total_losses
}

build_tower <- function(...) {
  
  tower <- list(...)
  
  function(gu_losses) {
    
    sapply(tower, function(l) {
      l(gu_losses)
    })
  }
}

calc_init_ROL <- function(x, layer) {
  el <- mean(layer_losses(x, layer$capacity, layer$att_point))/layer$capacity
  rol <- 0.0229 + 1.4057*el
  c(el = el, rol = rol)
}

eur_to_usd <- 1.24
N <- 400000
gu_base <- ground_up_losses("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_base", N, eur_to_usd, "2018-1-1", "2020-1-1")
gu_50 <- ground_up_losses("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_50", N, eur_to_usd, "2018-1-1", "2020-1-1")
gu_75 <- ground_up_losses("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_75", N, eur_to_usd, "2018-1-1", "2020-1-1")
gu_100 <- ground_up_losses("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_100", N, eur_to_usd, "2018-1-1", "2020-1-1")

r_layer <- list(capacity = 30e6, att_point = 0)
b_layer <- list(capacity = 30e6, att_point = 30e6)
c_layer <- list(capacity = 30e6, att_point = 60e6)
a_layer <- list(capacity = 30e6, att_point = 90e6)
e1_layer <- list(capacity = 50e6, att_point = 120e6)
e2_layer <- list(capacity = 75e6, att_point = 120e6)
e3_layer <- list(capacity = 100e6, att_point = 120e6)
ins0_layer <- list(capacity = 33e6, att_point = 120e6)
ins1_layer <- list(capacity = 33e6, att_point = 170e6)
ins2_layer <- list(capacity = 33e6, att_point = 195e6)
ins3_layer <- list(capacity = 33e6, att_point = 220e6)
xs0_layer <- list(capacity = 400e6, att_point = 153e6)
xs1_layer <- list(capacity = 400e6, att_point = 203e6)
xs2_layer <- list(capacity = 400e6, att_point = 228e6)
xs3_layer <- list(capacity = 400e6, att_point = 253e6)

# needs re-evaluation of metrics for new layers
e1_m <- calc_init_ROL(gu_50, e1_layer)
e2_m <- calc_init_ROL(gu_75, e2_layer)
e3_m <- calc_init_ROL(gu_100, e3_layer)

r_layer_cost <- tyche_layer_cost(r_layer, 1, 0, 0, 0)
c_layer_cost <- tyche_layer_cost(c_layer, 1.35, 0.2425, 0.156)
b_layer_cost <- tyche_layer_cost(b_layer, 1.35, 0.1475, 0.0891)
a_layer_cost <- tyche_layer_cost(a_layer, 1.35, 0.09, 0.0474)
e1_layer_cost <- tyche_layer_cost(e1_layer, 1.35, e1_m[2], e1_m[1])
e2_layer_cost <- tyche_layer_cost(e2_layer, 1.35, e2_m[2], e2_m[1])
e3_layer_cost <- tyche_layer_cost(e3_layer, 1.35, e3_m[2], e3_m[1])
ins0_layer_cost <- ins_layer_cost(ins0_layer, 1.6, 0.02)
ins1_layer_cost <- ins_layer_cost(ins1_layer, 1.6, 0.02)
ins2_layer_cost <- ins_layer_cost(ins2_layer, 1.6, 0.02)
ins3_layer_cost <- ins_layer_cost(ins3_layer, 1.6, 0.02)
xs0_layer_cost <- tyche_layer_cost(xs0_layer, 1, 0, 0, 0)
xs1_layer_cost <- tyche_layer_cost(xs1_layer, 1, 0, 0, 0)
xs2_layer_cost <- tyche_layer_cost(xs2_layer, 1, 0, 0, 0)
xs3_layer_cost <- tyche_layer_cost(xs3_layer, 1, 0, 0, 0)

tower_base <- build_tower(r_layer_cost, c_layer_cost, b_layer_cost, a_layer_cost, ins0_layer_cost, xs0_layer_cost)
tower_50 <- build_tower(r_layer_cost, c_layer_cost, b_layer_cost, a_layer_cost, e1_layer_cost, ins1_layer_cost, xs1_layer_cost)
tower_75 <- build_tower(r_layer_cost, c_layer_cost, b_layer_cost, a_layer_cost, e2_layer_cost, ins2_layer_cost, xs2_layer_cost)
tower_100 <- build_tower(r_layer_cost, c_layer_cost, b_layer_cost, a_layer_cost, e3_layer_cost, ins3_layer_cost, xs3_layer_cost)



out_base <- run("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_base", N, eur_to_usd, tower_base, 153e6, "2018-1-1", "2020-1-1")
out_50 <- run("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_50", N, eur_to_usd, tower_50, 203e6, "2018-1-1", "2020-1-1")
out_75 <- run("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_75", N, eur_to_usd, tower_75, 228e6, "2018-1-1", "2020-1-1")
out_100 <- run("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_100", N, eur_to_usd, tower_100, 253e6, "2018-1-1", "2020-1-1")

# x <- as.matrix(read.table("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_4/sep_jackpot.txt"))
# usd_x <- x*eur_to_usd
# cap <- 200e6*eur_to_usd
# y <- marginal_cost(pmin(c(usd_x), cap), 622614630, ils3_tower)


export_table <- function(base, future) {
  
  matrix(c(future[[1]],
    future[[2]],
    base$net_hedging- future$net_hedging,
    sum(future$ils_cost) - sum(base$ils_cost),
    future$total - base$total))
}

read_layer_losses <- function(filename) {
  x <- read.table(filename)
  apply(x, 1, sum)
}


x <- read.table("~/GitHub/modelling_report/sims/ag_losses/output_95/gls_t24_ag_losses.txt") +
  read.table("~/GitHub/modelling_report/sims/ag_losses/output_95/ems_t24_ag_losses.txt") +
  read.table("~/GitHub/modelling_report/sims/ag_losses/output_95/ems_v24_ag_losses.txt") +
  read.table("~/GitHub/modelling_report/sims/ag_losses/output_95/ejs_t24_ag_losses.txt")

x <- x[,1]*1.1818
