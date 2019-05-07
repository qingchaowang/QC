source("~/GitHub/R/special_draws/special_cost.R")

# Actual EJS 2nd Jan 2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 20449935.1, tickets = 47338185, bets = 580534, fn = ejs_spill)

# Actual EJS 9th Jan 2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 28164688.80, tickets = 50386168, bets = 650978, fn = ejs_spill)

# Actual EJS 25/5/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 9309431.20, tickets = 35775782, bets = 517626, fn = ejs_spill)

# Actual EJS 1/6/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 21956534.40, tickets = 39637773, bets = 529340, fn = ejs_spill)

# Actual EJS 8/6/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23365416.00, tickets = 41682699, bets = 573714, fn = ejs_spill)

# Actual EJS 15/6/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23887833.2, tickets = 42268189, bets = 594704, fn = ejs_spill)

# Actual EJS 22/6/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23907478.1, tickets = 42118940, bets = 620198, fn = ejs_spill)

#Actual EJS 29/6/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23865074.8, tickets = 42145418, bets = 597370, fn = ejs_spill)

#Actual EJS 6/7/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23997950.4, tickets = 42501202, bets = 616379, fn = ejs_spill)

#Actual GLS 6/9/2018
special_cost(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 27878873.7, jackpot2 = 1174251.6, tickets = 31875681, bets = 1131755, fn = gls_must_go)

ej_pars_max <- list(
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 20449935.1, tickets = 47338185, bets = 580534, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 28164688.80, tickets = 50386168, bets = 650978, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 9309431.20, tickets = 35775782, bets = 517626, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 21956534.40, tickets = 39637773, bets = 529340, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23365416.00, tickets = 41682699, bets = 573714, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23887833.2, tickets = 42268189, bets = 594704, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23907478.1, tickets = 42118940, bets = 620198, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23865074.8, tickets = 42145418, bets = 597370, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23997950.4, tickets = 42501202, bets = 616379, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 6172842.765, tickets = 36366433, bets = 482239, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 21700626.77, tickets = 38837833, bets = 449571, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 22587229.47, tickets = 40212913, bets = 550213, fn = ejs_spill),
  list(multiple = 1.7, ipt = 0.12, N = 50, jackpot1 = 0, jackpot2 = 23091369.52, tickets = 40955592, bets = 496632, fn = ejs_spill)
)





cost <- t(sapply(ej_pars_max, do.call, what = special_cost))
jackpot <- sapply(ej_pars_max, '[[', "jackpot2")
tickets <- sapply(ej_pars_max, '[[', "tickets")
bets <- sapply(ej_pars_max, '[[', "bets")



ej_pars_sum <- lapply(ej_pars_max, function(x){
  x$jackpot2 <- 0.2*x$jackpot2
  x$fn <- ejs_spill_sum
  x
})

max_res <- sapply(ej_pars_max, do.call, what = special_cost)
sum_res <- sapply(ej_pars_sum, do.call, what = special_cost)
