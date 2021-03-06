source("~/GitHub/R/tyche/zoe/ms_from_zoe.R")
source("~/GitHub/R/tyche/cost_per_bet.R")
source("~/GitHub/R/tyche/margin_automation.R")
source("~/GitHub/R/currencies/currencies.R")
source("~/GitHub/R/tools/tools.R")



run_me <- function() {
  
  require(xlsx)
  
  x <- scan("~/GitHub/R/sims/tyche_reset/reset_losses.txt")
  r_layer <- tyche_cost(x, 1, 0, make_layer_f(24e6, 0), "retention")
  c_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 24e6), "c_layer")
  b_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 54e6), "b_layer")
  a_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 84e6), "a_layer")
  trad_layer <- trad_ins_cost(1.6, 0.12, make_layer_f(30.5e6, 114e6), "trad_layer")
  tower <- list(r_layer, c_layer, b_layer, a_layer, trad_layer)
  # setwd("/Volumes/dfs/Hedging Management/001_Strategic/011_Margins/003_automated_margins/")
  setwd("/Users/georgios.sermaidis/Desktop/automated_margins3/")
  download.file(url = 'https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip', destfile = 'exchange_rate.zip')
  unzip("exchange_rate.zip")
  file.remove("exchange_rate.zip")
  
  t24_com <- list(
    "German Lotto" = margins_scalar_block(jackpot = eur(seq(1,35)*1e6), lower_costs = eur(0.436), bet_price = eur(1), odds = 139838160, tower = tower),
    "EuroMillions" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572), bet_price = eur(3), odds = 139838160, tower = tower),
    "EuroMillions-hedged" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572), bet_price = eur(3), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroJackpot" = margins_scalar_block(jackpot = eur(seq(10,90)*1e6), lower_costs = eur(0.52), bet_price = eur(2), odds = 95344200, tower = tower),
    "Irish Lotto" = margins_scalar_block(jackpot = eur(seq(2,20)*1e6), lower_costs = eur(0.3956), bet_price = eur(2), odds = 10737573, tower = tower),
    "Powerball" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = eur(3.5), odds = 292201338, tower = tower, jackpot_multiplier = 0.372),
    "Powerball-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = eur(3.5), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = eur(3.5), odds = 302575350, tower = tower, jackpot_multiplier = 0.372),
    "Megamillions-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = eur(3.5), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "UK Lotto" = margins_scalar_block(jackpot = gbp(seq(1,25)*1e6), lower_costs = list(gbp(0.06), eur(0.56)), odds = 45057474, bet_price =  eur(2.5), tower = tower),
    "Cash4Life" = fixed_odds_block(class_costs = eur(0.7385), bet_price = eur(2.5)),
    "Keno2"= fixed_odds_block(class_costs = eur(keno2_cost()), bet_price = eur(1)),
    "Keno3"= fixed_odds_block(class_costs = eur(keno3_cost()), bet_price = eur(1)),
    "Keno4"= fixed_odds_block(class_costs = eur(keno4_cost()), bet_price = eur(1)),
    "Keno5"= fixed_odds_block(class_costs = eur(keno5_cost()), bet_price = eur(1)),
    "Keno6"= fixed_odds_block(class_costs = eur(keno6_cost()), bet_price = eur(1)),
    "Keno7"= fixed_odds_block(class_costs = eur(keno7_cost()), bet_price = eur(1)),
    "Keno8"= fixed_odds_block(class_costs = eur(keno8_cost()), bet_price = eur(1)),
    "Keno9"= fixed_odds_block(class_costs = eur(keno9_cost()), bet_price = eur(1)),
    "Keno10"= fixed_odds_block(class_costs = eur(keno10_cost()), bet_price = eur(1))
  )
  
  ml24_ie <- list(
    "German Lotto" = margins_scalar_block(jackpot = eur(seq(1,35)*1e6), lower_costs = eur(0.436), bet_price = eur(1.5), odds = 139838160, tower = tower),
    "EuroMillions" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572), bet_price = eur(2.5), odds = 139838160, tower = tower),
    "EuroMillions-hedged" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572), bet_price = eur(2.5), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroMillions Plus" = fixed_odds_block(class_costs = eur(0.54182), bet_price = eur(1)),
    "EuroJackpot" = margins_scalar_block(jackpot = eur(seq(10,90)*1e6), lower_costs = eur(0.52), bet_price = eur(2.5), odds = 95344200, tower = tower),
    "Irish Lotto" = margins_scalar_block(jackpot = eur(seq(2,20)*1e6), lower_costs = eur(0.3956), bet_price = eur(2), odds = 10737573, tower = tower),
    "Irish Lotto Plus" = fixed_odds_block(class_costs = eur(0.41), bet_price = eur(1)),
    "Powerball" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = eur(3.5), odds = 292201338, tower = tower, jackpot_multiplier = 0.372),
    "Powerball-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = eur(3.5), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = eur(3.5), odds = 302575350, tower = tower, jackpot_multiplier = 0.372),
    "Megamillions-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = eur(3.5), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "UK Lotto" = margins_scalar_block(jackpot = gbp(seq(1,25)*1e6), lower_costs = list(gbp(0.5215), eur(0.2242)), odds = 45057474, bet_price =  eur(2.3), tower = tower),
    "Cash4Life" = fixed_odds_block(class_costs = eur(0.7385), bet_price = eur(2)),
    "Super6" = fixed_odds_block(class_costs = eur(0.561), bet_price = eur(1.25)),
    "Spiel77" = uncovered_scalar_block(jackpot = eur(seq(1,10)*1e6), lower_costs = eur(0.8869), bet_price = eur(2.5), odds = 10e6)
  )
  
  ml24_uk <- list(
    "German Lotto" = margins_scalar_block(jackpot = eur(seq(1,35)*1e6), lower_costs = eur(0.436), bet_price = gbp(1), odds = 139838160, tower = tower),
    "EuroJackpot" = margins_scalar_block(jackpot = eur(seq(10,90)*1e6), lower_costs = eur(0.52), bet_price = gbp(2.2), odds = 95344200, tower = tower),
    "Irish Lotto" = margins_scalar_block(jackpot = eur(seq(2,20)*1e6), lower_costs = eur(0.3956), bet_price = gbp(2), odds = 10737573, tower = tower),
    "Irish Lotto Plus" = fixed_odds_block(class_costs = eur(0.41), bet_price = gbp(1)),
    "Powerball" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = gbp(3), odds = 292201338, tower = tower, jackpot_multiplier = 0.372),
    "Powerball-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = gbp(3), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = gbp(3), odds = 302575350, tower = tower, jackpot_multiplier = 0.372),
    "Megamillions-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = gbp(3), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "Cash4Life" = fixed_odds_block(class_costs = gbp(0.7385), bet_price = gbp(2)),
    "Keno2"= fixed_odds_block(class_costs = gbp(keno2_cost()), bet_price = gbp(1)),
    "Keno3"= fixed_odds_block(class_costs = gbp(keno3_cost()), bet_price = gbp(1)),
    "Keno4"= fixed_odds_block(class_costs = gbp(keno4_cost()), bet_price = gbp(1)),
    "Keno5"= fixed_odds_block(class_costs = gbp(keno5_cost()), bet_price = gbp(1)),
    "Keno6"= fixed_odds_block(class_costs = gbp(keno6_cost()), bet_price = gbp(1)),
    "Keno7"= fixed_odds_block(class_costs = gbp(keno7_cost()), bet_price = gbp(1)),
    "Keno8"= fixed_odds_block(class_costs = gbp(keno8_cost()), bet_price = gbp(1)),
    "Keno9"= fixed_odds_block(class_costs = gbp(keno9_cost()), bet_price = gbp(1)),
    "Keno10"= fixed_odds_block(class_costs = gbp(keno10_cost()), bet_price = gbp(1))
  )
  
  ml24_ro <- list(
    "Romanian Lotto" = margins_scalar_block(jackpot = ron(seq(5,50)*1e6), lower_costs = ron(1.7587), bet_price = ron(5), odds = 13983816, tower = tower),
    "Romanian Lotto -  75%" = margins_scalar_block(jackpot = ron(seq(5,50)*1e6), lower_costs = ron(1.7587*0.75), bet_price = ron(5*0.75), odds = 13983816, tower = tower, jackpot_multiplier = 0.75),
    "Romanian Lotto -  50%" = margins_scalar_block(jackpot = ron(seq(5,50)*1e6), lower_costs = ron(1.7587*0.5), bet_price = ron(5*0.5), odds = 13983816, tower = tower, jackpot_multiplier = 0.5),
    "Romanian Lotto -  25%" = margins_scalar_block(jackpot = ron(seq(5,50)*1e6), lower_costs = ron(1.7587*0.25), bet_price = ron(5*0.25), odds = 13983816, tower = tower, jackpot_multiplier = 0.25),
    "German Lotto" = margins_scalar_block(jackpot = eur(seq(1,35)*1e6), lower_costs = eur(0.436), bet_price = ron(5), odds = 139838160, tower = tower),
    "German Lotto - 75%" = margins_scalar_block(jackpot = eur(seq(1,35)*1e6), lower_costs = eur(0.436*0.75), bet_price = ron(5*0.75), odds = 139838160, tower = tower, jackpot_multiplier = 0.75),
    "German Lotto - 50%" = margins_scalar_block(jackpot = eur(seq(1,35)*1e6), lower_costs = eur(0.436/2), bet_price = ron(5/2), odds = 139838160, tower = tower, jackpot_multiplier = 0.5),
    "German Lotto - 25%" = margins_scalar_block(jackpot = eur(seq(1,35)*1e6), lower_costs = eur(0.436/4), bet_price = ron(5/4), odds = 139838160, tower = tower, jackpot_multiplier = 0.25),
    "EuroMillions" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572), bet_price = ron(14), odds = 139838160, tower = tower),
    "EuroMillions - 50%" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572/2), bet_price = ron(14/2), odds = 139838160, tower = tower, jackpot_multiplier = 0.5),
    "EuroMillions - 25%" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572/4), bet_price = ron(14/4), odds = 139838160, tower = tower, jackpot_multiplier = 0.25),
    "EuroMillions - 10%" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572/10), bet_price = ron(14/10), odds = 139838160, tower = tower, jackpot_multiplier = 0.1),
    "EuroMillions-hedged" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572), bet_price = ron(14), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroMillions-hedged - 50%" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572/2), bet_price = ron(14/2), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroMillions-hedged - 25%" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572/4), bet_price = ron(14/4), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroMillions-hedged - 10%" = margins_scalar_block(jackpot = eur(seq(17,190)*1e6), lower_costs = eur(0.572/10), bet_price = ron(14/10), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroJackpot" = margins_scalar_block(jackpot = eur(seq(10,90)*1e6), lower_costs = eur(0.52), bet_price = ron(12), odds = 95344200, tower = tower),
    "EuroJackpot - 50%" = margins_scalar_block(jackpot = eur(seq(10,90)*1e6), lower_costs = eur(0.52/2), bet_price = ron(12/2), odds = 95344200, tower = tower, jackpot_multiplier = 0.5),
    "EuroJackpot - 25%" = margins_scalar_block(jackpot = eur(seq(10,90)*1e6), lower_costs = eur(0.52/4), bet_price = ron(12/4), odds = 95344200, tower = tower, jackpot_multiplier = 0.25),
    "EuroJackpot - 10%" = margins_scalar_block(jackpot = eur(seq(10,90)*1e6), lower_costs = eur(0.52/10), bet_price = ron(12/10), odds = 95344200, tower = tower, jackpot_multiplier = 0.1),
    "Powerball" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = ron(16), odds = 292201338, tower = tower, jackpot_multiplier = 0.372),
    "Powerball - 25%" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32/4), bet_price = ron(16/4), odds = 292201338, tower = tower, jackpot_multiplier = 0.372*0.25),
    "Powerball - 10%" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32/10), bet_price = ron(16/10), odds = 292201338, tower = tower, jackpot_multiplier = 0.372*0.1),
    "Powerball - 5%" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32/20), bet_price = ron(16/20), odds = 292201338, tower = tower, jackpot_multiplier = 0.372/20),
    "Powerball-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32), bet_price = ron(16), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "Powerball-hedged - 25%" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32/4), bet_price = ron(16/4), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "Powerball-hedged - 10%" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32/10), bet_price = ron(16/10), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "Powerball-hedged - 5%" = margins_scalar_block(jackpot = usd(seq(40,410,10)*1e6), lower_costs = usd(0.32/20), bet_price = ron(16/20), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = ron(16), odds = 302575350, tower = tower, jackpot_multiplier = 0.372),
    "MegaMillions - 25%" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247/4), bet_price = ron(16/4), odds = 302575350, tower = tower, jackpot_multiplier = 0.372*0.25),
    "MegaMillions - 10%" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247/10), bet_price = ron(16/10), odds = 302575350, tower = tower, jackpot_multiplier = 0.372*0.1),
    "MegaMillions - 5%" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247/20), bet_price = ron(16/20), odds = 302575350, tower = tower, jackpot_multiplier = 0.372/20),
    "MegaMillions-hedged" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247), bet_price = ron(16), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions-hedged - 25%" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247/4), bet_price = ron(16/4), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions-hedged - 10%" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247/10), bet_price = ron(16/10), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions-hedged - 5%" = margins_scalar_block(jackpot = usd(seq(40,410,5)*1e6), lower_costs = usd(0.247/20), bet_price = ron(16/20), odds = 302575350, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "Cash4Life" = fixed_odds_block(class_costs = ron(3.6927), bet_price = ron(12)),
    "Cash4Life - 50%" = fixed_odds_block(class_costs = ron(3.6927*0.5), bet_price = ron(6)),
    "Cash4Life - 25%" = fixed_odds_block(class_costs = ron(3.6927*0.25), bet_price = ron(3)),
    "Cash4Life - 10%" = fixed_odds_block(class_costs = ron(3.6927*0.1), bet_price = ron(1.2))
  )
  
  ml24_za <- list(
    "UK Lotto" = margins_scalar_block(jackpot = gbp(seq(1,25)*1e6), lower_costs = list(gbp(0.06), zar(8.09)), bet_price = zar(35), odds = 45057474, tower = tower),
    "ZA lotto" = margins_scalar_block(jackpot = zar(seq(3,150)*1e6), lower_costs = zar(1.269), bet_price = zar(5), odds = 20358520, tower = tower),
    "ZA Lotto1_2" = margins_scalar_block(jackpot = zar(seq(3,150)*1e6), lower_costs = zar(0.855324), bet_price = zar(2.5), odds = 20358520, tower = tower),
    "ZA Powerball" = margins_scalar_block(jackpot = zar(seq(1,150)*1e6), lower_costs = zar(1.093), bet_price = zar(5), odds = 42375200, tower = tower),
    "ZA Powerball Plus" = margins_scalar_block(jackpot = zar(seq(1,150)*1e6),lower_costs = zar(0.546), bet_price = zar(2.5), odds = 42375200, tower = tower),
    "UK Super Lotto" = margins_scalar_block(jackpot = zar(1e9), lower_costs = zar(2.1181), bet_price = zar(8), odds = 2388046122, tower = tower)
  )
  
  
  export_tenant(t24_com, "t24_com.xlsx")
  export_tenant(ml24_ie, "ml24_ie.xlsx")
  export_tenant(ml24_uk, "ml24_uk.xlsx")
  export_tenant(ml24_ro, "ml24_ro.xlsx")
  export_tenant(ml24_za, "ml24_za.xlsx")
}

tryCatch(run_me(), error = function(e){
  print(e)
  send_email(to = "georgios.sermaidis@mylotto24.co.uk", subject = "Automated margins error", body = "", attachment = "run_automated_margins.Rout")
  file.remove("run_automated_margins.Rout")
})


