source("~/GitHub/R/rules/rules.R")

next_draw <- function(x, date, entries, winners) {
  
  pct_alloc <- c(0.36, 0.085, 0.03, 0.01, 0.009, 0.007, 0.006, 0.031, 0.03, 0.043, 0.078, 0.191)
  booster_alloc <- 0.12
  prize_fund <- entries
  
  jpad <- x$to_roll_over
  rojp <- x$to_roll_over
  
  if(x$jpad_booster > 20e6) {
    jpad[1] <- jpad[1] + x$jpad_booster - 20e6
    x$jpad_booster <- 20e6
  }
  
  jpad <- jpad + prize_fund*pct_alloc
  booster <- x$jpad_booster + booster_alloc*prize_fund
  if(jpad[1] < 10e6) {
    shortfall <- 10e6 - jpad[1]
    jpad[1] <- 10e6
    booster <- booster - shortfall
  }
  
  fund <- jpad
  
  if(jpad[1] > 90e6) {
    excess <- jpad[1] - 90e6
    jpad[2] <- jpad[2] + excess
    jpad[1] <- 90e6
  }
  
  if(jpad[2] > 90e6) {
    excess <- jpad[2] - 90e6
    jpad[2] <- 90e6
    ffw <- find_first_winner(winners, 3, 13)
    jpad[ffw] <-  jpad[ffw] + excess
  }

  prize <- round_down(10)(ifelse(winners > 0, jpad/winners, 0))
  prize <- pool_seniority(jpad, winners, prize, 1, 12, round_down(10))
  
  rounding <- sum(ifelse(winners, jpad - winners*prize, 0))
  booster <- booster + rounding
  
  to_roll_over <- ifelse(winners, 0, jpad)
  
  list(date = date, entries = entries, winners = winners, jpad = jpad, fund = fund, jpad_booster = booster, to_roll_over = to_roll_over, rojp = rojp, prize = prize)
}





