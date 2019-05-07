# calculates the expected value of L(x + J/(k+1)) conditionally on x, where k is Poisson(tickets/odds)
# where L(y) = min(C, max(0, y - R)), C is the layer's capacity and R the layer's retention
# the method calculates the poisson expectation by truncating the summation up to a very high percentile
el_y_sum_poisson <- function(x, tickets, odds, jackpot, layer_f) {
  primary_winners <- qpois(1-1e-10, tickets/odds)
  l <- sapply(jackpot/(0:primary_winners + 1), function(k) k + x)
  l_layer <- apply(l, 2, layer_f$f)
  out <- l_layer %*% (dpois(0:primary_winners, tickets/odds))
  as.vector(out)
}

# calculates E(L(x + kJ)) conditionally on x, where k is Poisson(tickets/odds)
# where L(y) = min(C, max(0, y - R)), C is the layer's capacity and R the layer's retention
# the method calculates the poisson expectation by truncating the summation up to a very high percentile
# used for cases where each winner does not share with neither primary or secondary (each winner gets the whole jackpot)
el_y_fixed_payout <- function(x, tickets, bets, odds, jackpot, layer_f)
{
  library(magrittr)
  lambda <- bets/odds
  max_k <- max(qpois(0.99999, lambda), 10)
  probs <- dpois(0:max_k, lambda)
  probs %*% layer_f$f(matrix(rep(x, each = max_k + 1), nrow = max_k+1) + (0:max_k)*jackpot) %>% mean()
}

# This is the theoretical method calculating the exact expected loss
el_y_sum_poisson_anal <- function(x, tickets, odds, jackpot, layer_f) {
  
  retention <- layer_f$retention
  capacity <- layer_f$capacity
  
  prob <- 1/odds
  lambda <- tickets/odds
  lb <- floor(jackpot/(capacity + retention - x) - 1)
  ub <- floor(jackpot/(retention - x) - 1)
  
  out <- rep(0, length(x))
  
  case1 <- (x >= capacity + retention)
  case2 <- ((x < capacity + retention)&(x >= retention))
  case3 <- (x < retention)
  
  anal_sum2 <- (x - retention)*(1                 - ppois(lb, lambda)) + jackpot/lambda*(1                 - ppois(lb+1, lambda))
  anal_sum3 <- (x - retention)*(ppois(ub, lambda) - ppois(lb, lambda)) + jackpot/lambda*(ppois(ub+1, lambda) - ppois(lb+1, lambda))
  
  out[case1] <- capacity
  out[case2] <- (anal_sum2)[case2]
  out[case2] <- out[case2] + (ifelse(lb >= 0, capacity*ppois(lb, lambda), 0))[case2]
  out[case3] <- (ifelse(ub >= 0, anal_sum3, 0))[case3]
  out[case3] <- out[case3] + (ifelse(lb >= 0, capacity*ppois(lb, lambda), 0))[case3]
  out
}

el_y_full_jackpot <- function(x, tickets, bets, odds, jackpot, layer_f) {
  
  p <- 1/odds
  fx <- layer_f$f(x)
  apply(layer_f$f(matrix(jackpot, nrow = length(x), ncol = length(jackpot), byrow = T) + x), 2, mean)*p + (1-p)*mean(fx)
}

tyche_cost <- function(x, beta, ipt, layer_f, name, el_y_f = el_y_full_jackpot, tickets_f = NULL, bets_f = NULL) {
  
  f <- function(jackpot, odds, weekday) {
    tickets <- 0
    if(!is.null(tickets_f)) tickets <- tickets_f(jackpot, weekday)
    if(!is.null(bets_f)) bets <- bets_f(jackpot, weekday)
    fy <- el_y_f(x, tickets, bets, odds, jackpot, layer_f)
    fx <- layer_f$f(x)
    (1+ipt)*beta*(fy - mean(fx))/ifelse(is.null(bets_f), 1, bets)
  }
  
  list(f = f, name = name)
}

trad_ins_cost <- function(mult, ipt, layer_f, name = "") {
  
  f <- function(jackpot, odds, weekday) {
    (1+ipt)*mult*layer_f$f(jackpot)/odds
  }
  
  list(f = f, name = name)
}

trad_ins_cost_fixed <- function(mult, ipt, bets_f, layer_f, name = "")
{
  f <- function(jackpot, odds, weekday) {
    bets <- bets_f(jackpot, odds)
    (1+ipt)*mult*el_y_fixed_payout(0, 0, bets, odds, jackpot, layer_f)/bets
  }
  
  list(f = f, name = name)
}

fixed_cost <- function(num, name = "") {
  
  f <- function(jackpot, odds) {
    num
  }
  
  list(f = f, name = name)
}

read_losses <- function(folder) {
  x <- read.table(file.path(folder, "gls_t24_ag_losses.txt"))
  x <- x + read.table(file.path(folder, "ejs_t24_ag_losses.txt"))
  x <- x + read.table(file.path(folder, "ems_t24_ag_losses.txt"))
  x <- x + read.table(file.path(folder, "ems_v24_ag_losses.txt"))
  x
}

make_layer_f <- function(capacity, retention) {
  f <- function(x) {
    pmin(pmax(x - retention, 0), capacity)
  }
  
  list(f = f, capacity = capacity, retention = retention)
}

make_fixed_tower <- function(ils_retention, snr_att_poit, N)
{
  r_layer <- tyche_cost(x[,1], 1, 0, make_layer_f(ils_retention, 0), "retention", el_y_fixed_payout, NULL, tf(N))
  c_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(30e6, ils_retention), "c_layer", el_y_fixed_payout, NULL, tf(N))
  b_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(30e6, ils_retention + 30e6), "b_layer", el_y_fixed_payout, NULL, tf(N))
  a_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(30e6, ils_retention + 60e6), "a_layer", el_y_fixed_payout, NULL, tf(N))
  trad_layer <- trad_ins_cost_fixed(1.6, 0.12, tf(N), make_layer_f(33e6, snr_att_poit), "trad_layer")
  list(r_layer, c_layer, b_layer, a_layer, trad_layer)
}

fixed_entries <- function(N)
{
  function(jackpot, weeekday)
  {
    N
  }
}

marginal_cost <- function(jackpots, odds, layer_list, weekday = NULL) {
  
  result <- sapply(jackpots, function(jackpot) {
    
    sapply(layer_list, function(l) {
      l$f(jackpot, odds, weekday)
    })
  })
  
  result <- t(result)
  colnames(result) <- sapply(layer_list, function(l){l$name})
  total_cost <- apply(result, 1, sum)
  cbind(jackpot = jackpots, result, total_cost)
}

marginal_cost2 <- function(jackpots, odds, layer_list, weekday = NULL) {
  
  result <- sapply(layer_list, function(l) {
    l$f(jackpots, odds, weekday)
  })
  
  colnames(result) <- sapply(layer_list, function(l){l$name})
  total_cost <- apply(result, 1, sum)
  cbind(jackpot = jackpots, result, total_cost)
}


