prob <- function(x1, x2) {
  dhyper(x1, 5, 45, 5)*dhyper(x2, 2, 8, 2)
}

pool_seniority <- function(jpad, winners) {
  
  k <- which(winners > 0)
  winners <- winners[k]
  jpad <- jpad[k]
  
  quota <- 0.1*floor(10*jpad/winners)
  n <- length(quota)
  
  for(i in 1:(n-1)) {
    
    sum_jpad <- jpad[i]
    sum_winners <- winners[i]
    for(j in (i+1):n) {
      sum_jpad <- sum_jpad + jpad[j]
      sum_winners <- sum_winners + winners[j]
      
      if(quota[j] > quota[i]) {
        quota[i:j] <- 0.1*floor(10*sum_jpad/sum_winners)
      }
    }
  }
  
  out <- rep(0, 12)
  out[k] <- quota
  out
}

sim <- function(init_draw, n_draws, draw_checker, primary_vol, primary_winners, secondary_vol, secondary_winners, get_primary_draw, get_secondary_draw, secondary_delegate) {
  
  cur_date <- init_draw$date
  past_draw <- init_draw
  
  while(cur_date < end_date) {
    
    cur_date <- cur_date + 1
   
     if(draw_checker(cur_date)) {
      tickets <- primary_vol(cur_date, past_draw)
      pr_winners <- primary_winners(tickets)
      draw <- get_primary_draw(past_draw, tickets, pr_winners)
      # bets <- secondary_vol(cur_date, past_draw)
      # se_winners <- secondary_winners(bets)
      # secondary_draw <- get_secondary_draw(bets, se_winners, draw)
      
      # secondary_delegate$process_secondary_draw(secondary_draw)
      
      past_draw <- draw    
    }
  }
}



ej_primary_update <- function(jpad_alloc, booster_alloc) {
  
  function(past_draw, tickets, pr_winners)
  draw <- list()
  draw$tickets <- tickets
  draw$winners <- pr_winners
  draw$rojp <- ifelse(past_draw$winners > 0, 0, past_draw$fund)
  draw$rojp <- pmin(90e6, draw$rojp)
  draw$fund <- draw$rojp + jpad_alloc*tickets
  draw$booster <- past_draw$booster
  
  if(draw$fund[1] < 10e6) {
    draw$booster <- draw$booster - (10e6 - draw$fund[1])
  }
  
  if(draw$booster > 20e6) {
    draw$fund[1] <- draw$fund[1] + draw$booster - 20e6
    draw$booster <- 20e6
  }
  
  draw$booster <- past_draw$booster + booster_alloc*tickets
  
  draw$jpad <- draw$fund
  
  spill <- draw$fund[1] - 90e6
  if(spill > 0) {
    draw$jpad[1] <- 90e6
    draw$jpad[2] <- draw$jpad[2] + spill
  }

  draw$payout <- pool_seniority(draw$jpad, draw$winners)
  draw
}

ej_secondary_update <- function(cap_action, transform_winners) {
  
  function(bets, se_winners, primary_draw) {
    
    draw <- list()
    draw$bets <- bets
    draw$winners <- se_winners
    draw$jpad <- primary_draw$fund
    spill <- draw$fund - 90e6
    if(spill > 0) draw <- cap_action(draw, spill)
    tr_winners <- transform_winners(primary_draw$winners, draw$winners)
    draw$payout <- pool_seniority(draw$jpad, tr_winners)
    draw
  }
}

spill_action <- function(draw, spill) {
  
  draw$jpad[1] <- 90e6
  draw$jpad[2] <- draw$jpad[2] + spill
  draw
}

no_spill_action <- function(draw, spill) {
  
  draw
}

ej_draw_checker <- function(the_date) {
  
  wd <- weekdays(the_date)
  if(wd == "Friday") {
    T
  }
  else {
    F
  }
}

volume_curve <- function(coefs, std_dev) {

  function(the_date, past_draw) {
    rojp <- past_draw$rojp/1e6
    x <- coefs[1]
    if(rojp > 0.1) x <- x + coefs[2] + coefs[3]*rojp + coefs[4]*rojp^2
    x <- x*(1 + rnorm(1, 0, std_dev))
    x*1e6  
  }  
}

multinomial_winners <- function(probs) {
  
  probs <- c(probs, 1 - sum(probs))
  n <- length(probs)
  
  function(trials) {
    x <- rmultinom(1, trials, probs)
    x[-n]
  }
}


hist_vol <- function(the_data) {
  
  x <- split(the_data$entries, the_data$date)
  
  function(the_date, past_draw) {
    x[[as.character(the_date)]]
  }
}

require(xlsx)
hist_data <- read.xlsx("/Volumes/dfs/Hedging Management/003_Analysts/Data/xlsx/ejp_data.xlsm", 1)
hist_data <- hist_data[,c("date", "entries")]


jpad_alloc <- c(36, 8.5, 3, 1, 0.9, 0.7, 0.6, 3.1, 3, 4.3, 7.8, 19.1)/100
booster_alloc <- 12
probs <- c(prob(5,2), prob(5,1), prob(5,0), 
           prob(4,2), prob(4,1), prob(4,0),
           prob(3,2), prob(2,2), prob(3,1),
           prob(3,0), prob(1,2), prob(2,1)) 

init_draw <- list()
init_draw$tickets <- 10165350
init_draw$winners <- c(0, 1, 4, 33, 407, 732, 1573,	21391, 19134, 31555, 107045, 268020)
init_draw$rojp <- rep(0, 12)
init_draw$fund <- c(10000000.00000,	864054.75000,	304960.50000,	101653.50000,	91488.15000,	71157.45000,	60992.10000,	315125.85000,	304960.50000,	437110.05000,	792897.30000,	1941581.85000)
init_draw$booster <- 17112936.5000
init_draw$jpad <- init_draw$fund



pr_vol_gen <- hist_vol(hist_data)
pr_winners_gen <- multinomial_winners(probs)
se_vol_gen <- 
se_winners_gen <- multinomial_winners(probs)

