# this function calculates the following double summation
# sum_x sum_y ( layer_f(payout_fn(x,y)) * p_prob(tickets,x) * s_prob(bets, x) )
# layer_f is a function that takes one argument (the loss) and returns the loss in the layer
# payout_fn is a function that takes two arguments (primary winners and secondary winners) and returns the ground-up loss
# p_prob/s_prob is a function of two arguments (number of entries, number of winners) and returns the corresponding pdf
# tickets is the PURE primary tickets (ie excluding any bets bought via a primary channel)
expected_loss <- function(N, layer_f, payout_fn, p_prob, s_prob, tickets, bets) {
  
  nx <- 0:N
  ny <- 1:N
  
  probs <- outer(nx, ny, function(x, y){
    p_prob(tickets, x) * s_prob(bets, y)
  })
  
  losses <- outer(nx, ny, function(x, y){
    layer_f(payout_fn(x, y))
  })
  
  sum(probs * losses)
}

# generic total ground-up payout function given jackpot and divisor function
make_payout_f <- function(jackpot, divisor_f) {
  
  function(x, y) {
    y*jackpot/divisor_f(x,y)
  }
}

# max payout function
max_payout_f <- function(jackpot) {
  make_payout_f(jackpot, pmax)
}

# sum payout function
sum_payout_f <- function(jackpot) {
  make_payout_f(jackpot, function(x,y){x+y})
}

# poisson density function
poisson_f <- function(odds) {
  function(n, x) {
    dpois(x, n/odds)
  }
}

# returns a function that calculates losses in a layer with given capacity and retention/attachement point
make_layer_f <- function(capacity, retention) {
  function(x) {
    pmin(capacity, pmax(x - retention, 0))
  }
}

# function for calculating the expected loss in a layer for a GLS 13th must go
gls_must_go <- function(N, ins_layer, jackpot1, jackpot2, tickets, bets) {
  
  el <- expected_loss(N, ins_layer, max_payout_f(jackpot1 + jackpot2), poisson_f(15537573/0.9), poisson_f(15537573), tickets, bets)  
  el*dpois(0, (tickets + bets)/139838160)
}

gls_must_go_sum <- function(N, ins_layer, jackpot1, jackpot2, tickets, bets) {
  
  el <- expected_loss(N, ins_layer, sum_payout_f(jackpot1 + jackpot2), poisson_f(15537573/0.9), poisson_f(15537573), tickets, bets)  
  el*dpois(0, (tickets + bets)/139838160)
}

# function for calculating the expected loss in a layer for a EJS spillover
ejs_spill <- function(N, ins_layer, jackpot1, jackpot2, tickets, bets) {
  
  el <- expected_loss(N, ins_layer, max_payout_f(jackpot2), poisson_f(5959012), poisson_f(5959012), tickets, bets)  
  el
}

# function for calculating the expected loss in a layer for a EJS spillover
ejs_spill_sum <- function(N, ins_layer, jackpot1, jackpot2, tickets, bets) {
  
  el <- expected_loss(N, ins_layer, sum_payout_f(jackpot2), poisson_f(5959012), poisson_f(5959012), tickets, bets)  
  el
}

# function for calculating the expected loss in a layer for a EMS spillover
ems_spill <- function(N, ins_layer, jackpot1, jackpot2, tickets, bets) {
  el <- expected_loss(N, ins_layer, sum_payout_f(jackpot2), poisson_f(6991908), poisson_f(6991908), tickets, bets)  
  el
}

# function for calculating the expected loss in a layer for a EMS 5th must go
ems_must_go <- function(N, ins_layer, jackpot1, jackpot2, tickets, bets) {
  el_so <- expected_loss(N, ins_layer, sum_payout_f(jackpot2), poisson_f(6991908), poisson_f(6991908), tickets, bets)  
  el_pd <- expected_loss(N, ins_layer, sum_payout_f(jackpot1 + jackpot2), poisson_f(6991908/0.9), poisson_f(6991908), tickets, bets)
  prob_pd <- dpois(0, (tickets + bets)/139838160)
  prob_pd*el_pd + (1-prob_pd)*el_so
}

# function for calculating the total expected cost in our special insurance tower
# fn is one of the gls_must_go, ems_spill,... functions
special_cost <- function(multiple, ipt, N, jackpot1, jackpot2, tickets, bets, fn) {
  
  fn <- match.fun(fn)
  ret_layer <- make_layer_f(5e6, 0)
  ins_layer <- make_layer_f(20e6, 5e6)
  xs_layer <- make_layer_f(400e6, 25e6)
  
  ret_cost <- fn(N, ret_layer, jackpot1, jackpot2, tickets, bets)
  xs_cost <- fn(N, xs_layer, jackpot1, jackpot2, tickets, bets)
  ins_cost <- fn(N, ins_layer, jackpot1, jackpot2, tickets, bets)*(1+ipt)*multiple
  total_cost <- ret_cost + ins_cost + xs_cost
  c(retention = ret_cost, insurance = ins_cost, xs_retention = xs_cost, total = total_cost)
}
