source("~/GitHub/R/rules/rules.R")

next_draw <- function(x, date, entries, winners) {
  
  pct_alloc_2to8 <- c(0.1, 0.05, 0.15, 0.05, 0.1, 0.1, 0.45)
  prize_fund <- entries*0.5
  
  counter <- x$counter
  rojp <- x$to_roll_over
  
  counter <- ifelse((x$counter == 13) | x$winners[-9], 1, x$counter + 1)
  counter[2] <- ifelse(((x$winners[1] | x$counter[1] == 13) & !x$winners[2])|x$winners[2], 1, x$counter[2] + 1)
  
  jpad <- rep(NA, 9)
  jpad[1] <- 0.128*prize_fund
  jpad[9] <- 5*winners[9]
  
  prize_fund <- prize_fund - jpad[1] - jpad[9]
  jpad[2:8] <- prize_fund*pct_alloc_2to8
  jpad[1:8] <- jpad[1:8] + x$to_roll_over
  
  fund <- jpad
  
  for(i in 1:8) {
    if(counter[i] == 13) jpad <- must_go(jpad, winners, i, 9)
  }
  
  if(winners[1] & !winners[2]) {
    jpad[1] <- sum(jpad[1:2])
    jpad[2] <- 0
  }
  
  prize <- round_down(10)(ifelse(winners > 0, jpad/winners, 0))
  prize <- pool_seniority(jpad, winners, prize, 1, 8, round_down(10))
  
  to_roll_over <- ifelse(winners[-9], 0, jpad[-9])
  to_roll_over <- round_down(10)(to_roll_over)
  
  list(date = date, entries = entries, winners = winners, jpad = jpad, fund = fund, counter = counter, to_roll_over = to_roll_over, rojp = rojp, prize = prize)
}





