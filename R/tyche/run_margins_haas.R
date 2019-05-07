source("~/GitHub/R/tyche/zoe/ms_from_zoe.R")
source("~/GitHub/R/tyche/cost_per_bet.R")
source("~/GitHub/R/tyche/margin_automation.R")
source("~/GitHub/R/currencies/currencies.R")
source("~/GitHub/R/tools/tools.R")



run_me <- function() {
  
  require(xlsx)
  
  x <- scan("~/GitHub/R/sims/tyche_reset/reset_losses.txt")
  r_layer <- tyche_cost(x, 1, 0, make_layer_f(24.1e6, 0), "retention")
  c_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 24.1e6), "c_layer")
  b_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 54.1e6), "b_layer")
  a_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 84.1e6), "a_layer")
  trad_layer <- trad_ins_cost(1.7, 0.12, make_layer_f(30.5e6, 114.1e6), "trad_layer")
  tower <- list(r_layer, c_layer, b_layer, a_layer, trad_layer)
  setwd("/Users/georgios.sermaidis/Desktop/automated_margins_haas/")
  download.file(url = 'https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip', destfile = 'exchange_rate.zip')
  unzip("exchange_rate.zip")
  file.remove("exchange_rate.zip")
  
  total_lotto <- list(
    "Powerball" = margins_block(jackpot = usd(c(100, 250, 500)*1e6), lower_costs = usd(0.32), bet_price = eur(c(0.85, 1.06, 2.29)), odds = 292201338, tower = tower, jackpot_multiplier = 0.372),
    "Powerball-hedged" = margins_block(jackpot = usd(c(100, 250, 500)*1e6), lower_costs = usd(0.32), bet_price = eur(2.55), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_block(jackpot = usd(c(100,250,380)*1e6), lower_costs = usd(0.247), bet_price = eur(c(0.85, 1.16, 2.29)), odds = 302575350, tower = tower, jackpot_multiplier = 0.372),
    # "Megamillions-hedged" = margins_block(jackpot = usd(c(100,250,500)*1e6), lower_costs = usd(0.247), bet_price = eur(2.58), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "EuroMillions" = margins_block(jackpot = eur(c(25,50,70)*1e6), lower_costs = eur(0.572), bet_price = eur(c(1.28, 1.53, 1.79)), odds = 139838160, tower = tower, jackpot_multiplier = 0.8),
    "EuroMillions-hedged" = margins_block(jackpot = eur(c(25,50,70,190)*1e6), lower_costs = eur(0.572), bet_price = eur(2.67), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "full")),
    "EuroJackpot" = margins_block(jackpot = eur(c(20,40,60,75,90)*1e6), lower_costs = eur(0.52), bet_price = eur(c(1.45, 1.53, 1.62, 1.85, 2.11)), odds = 95344200, tower = tower),
    "German Lotto" = margins_block(jackpot = eur(c(5,10,15,20,37)*1e6), lower_costs = eur(0.436), bet_price = eur(c(0.64, 0.68, 0.72, 0.77, 0.81)), odds = 139838160, tower = tower),
    "Irish Lotto" = margins_block(jackpot = eur(c(5,10,14,20)*1e6), lower_costs = eur(0.3956), bet_price = eur(c(1.2, 1.6, 2.2, 2.8)), odds = 10737573, tower = tower),
    "Super6" = fixed_odds_block(class_costs = eur(0.56), bet_price = eur(0.85)),
    "GlucksSpirale" = fixed_odds_block(class_costs = eur(2.23), bet_price = eur(3.4)),
    "Spiel77" = uncovered_block(jackpot = eur(c(3,6,10)*1e6), lower_costs = eur(0.887), bet_price = eur(c(1.7, 1.79, 1.95)), odds = 10000000),
    "Cash4Life" = fixed_odds_block(class_costs = eur(0.74), bet_price = eur(1.06)),
    "Irish Lotto Plus" = fixed_odds_block(class_costs = eur(0.41), bet_price = eur(0.60)),
    "EuroMillions Plus" = fixed_odds_block(class_costs = eur(0.5418), bet_price = eur(0.70)),
    "OZ Lotto" = margins_block(jackpot = eur(c(10,20,30,40,50,60)*1e6), lower_costs = aud(0.4068), bet_price = eur(c(0.65,0.85,1.10,1.50,1.80,2.10)), odds = 45379620, tower = tower),
    "AUS Powerball" = margins_block(jackpot = eur(c(20,40,50,60)*1e6), lower_costs = aud(0.404), bet_price = eur(c(0.55,0.75,0.8,0.9)), odds = 134490400, tower = tower),
    "French Lotto" = margins_block(jackpot = eur(c(10,20,30)*1e6), lower_costs = eur(0.810352313), bet_price = eur(c(1.60,2.3,3)), odds = 19068840, tower = tower),
    "AUS Lotto MW" = margins_block(jackpot = eur(0.7e6), lower_costs = aud(0.198), bet_price = eur(0.3), odds = 8145060, tower = tower),
    # "AUS Lotto S" = margins_block(jackpot = eur(c(1,2,3,4)*1e6), lower_costs = aud(0.2574), bet_price = eur(0.6), odds = 8145060, tower = tower),
    "El Gordo (10%)" = fixed_odds_block(class_costs = eur(14), bet_price = eur(16)),
    # "Polish Lotto" = margins_block(jackpot = eur(c(4,9,20,30,40)*1e6), lower_costs = eur(), bet_price = eur(0.5, 0.8, 1), odds = 13983816, tower = tower),
    "Swedish Lotto" = margins_block(jackpot = eur(c(1,2,3)*1e6), lower_costs = sek(0.702), bet_price = eur(c(0.4, 0.5, 0.6)), odds = 6724520, tower = tower)
  )
  
  export_tenant(total_lotto, "total_lotto.xlsx")
}

tryCatch(run_me(), error = function(e){
  print(e)
  send_email(to = "georgios.sermaidis@mylotto24.co.uk", subject = "Automated margins error", body = "", attachment = "run_automated_margins.Rout")
  file.remove("run_automated_margins.Rout")
})


