source("~/GitHub/R/tyche/zoe/ms_from_zoe.R")
source("~/GitHub/R/tyche/cost_per_bet.R")
source("~/GitHub/R/tyche/margin_automation.R")
source("~/GitHub/R/currencies/currencies.R")
source("~/GitHub/R/tools/tools.R")

bets_f <- function(N)
{
  function(jackpot, weekday)
  {
    N
  }
}

ils_tower <- function(N)
{
  N <- 1350
  x <- scan("~/GitHub/R/sims/tyche_reset/reset_losses.txt")
  r_layer <- tyche_cost(x, 1, 0, make_layer_f(24.1e6, 0), "retention", el_y_fixed_payout, NULL, bets_f(N))
  c_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 24.1e6), "c_layer", el_y_fixed_payout, NULL, bets_f(N))
  b_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 54.1e6), "b_layer", el_y_fixed_payout, NULL, bets_f(N))
  a_layer <- tyche_cost(x, 1.35, 0.12, make_layer_f(30e6, 84.1e6), "a_layer", el_y_fixed_payout, NULL, bets_f(N))
  trad_layer <- trad_ins_cost(1.7, 0.12, make_layer_f(30.5e6, 114.1e6), "trad_layer")
  list(r_layer, c_layer, b_layer, a_layer, trad_layer)  
}

trad_tower <- function(N, retention, capacity)
{
  r_layer <- trad_ins_cost(mult = 1, ipt = 0, layer_f = make_layer_f(retention, 0), name = "retention")
  trad_layer <- trad_ins_cost(mult = 1.8, ipt = 0.12, layer_f = make_layer_f(capacity, retention), name = "layer")
  list(r_layer, trad_layer)
}


run_me <- function(tower, suffix) {
  
  require(xlsx)

  setwd("/Users/georgios.sermaidis/Desktop/automated_margins_sa/")
  download.file(url = 'https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip', destfile = 'exchange_rate.zip')
  unzip("exchange_rate.zip")
  file.remove("exchange_rate.zip")
  
  sa_tenant_1.0 <- list(
    "EuroMillions" = margins_block(jackpot = eur(seq(20,190,10)*1e6), lower_costs = eur(0.572), bet_price = eur(3.50), odds = 139838160, tower = tower),
    "EuroMillions-hedged" = margins_block(jackpot = eur(seq(20,190,10)*1e6), lower_costs = eur(0.572), bet_price = eur(3.50), odds = 139838160, tower = tower, jackpot_multiplier = 0.2, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroJackpot" = margins_block(jackpot = eur(seq(10,90,10)*1e6), lower_costs = eur(0.52), bet_price = gbp(2.2), odds = 95344200, tower = tower),
    "Powerball" = margins_block(jackpot = usd(seq(40,500,10)*1e6), lower_costs = usd(0.32), bet_price = gbp(3.0), odds = 292201338, tower = tower, jackpot_multiplier = 0.372),
    "Powerball-hedged" = margins_block(jackpot = usd(seq(40,500,10)*1e6), lower_costs = usd(0.32), bet_price = gbp(3.0), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_block(jackpot = usd(seq(40,390,10)*1e6), lower_costs = usd(0.247), bet_price = gbp(3.0), odds = 302575350, tower = tower, jackpot_multiplier = 0.372),
    "BonoLoto" = margins_block(jackpot = eur(seq(0.5,7,0.5)*1e6), lower_costs = eur(0.2055), bet_price = gbp(1), odds = 13983816, tower = tower)
  )
  
  sa_tenant_0.5 <- list(
    "EuroMillions" = margins_block(jackpot = eur(seq(20,190,10)*1e6), lower_costs = eur(0.572/2), bet_price = eur(3.50/2), odds = 139838160, tower = tower, jackpot_multiplier = 0.5),
    "EuroMillions-hedged" = margins_block(jackpot = eur(seq(20,190,10)*1e6), lower_costs = eur(0.572/2), bet_price = eur(3.50/2), odds = 139838160, tower = tower, jackpot_multiplier = 0.2*0.5, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroJackpot" = margins_block(jackpot = eur(seq(10,90,10)*1e6), lower_costs = eur(0.52/2), bet_price = gbp(2.2/2), odds = 95344200, tower = tower, jackpot_multiplier = 0.5),
    "Powerball" = margins_block(jackpot = usd(seq(40,500,10)*1e6), lower_costs = usd(0.32/2), bet_price = gbp(3.0/2), odds = 292201338, tower = tower, jackpot_multiplier = 0.372*0.5),
    "Powerball-hedged" = margins_block(jackpot = usd(seq(40,500,10)*1e6), lower_costs = usd(0.32/2), bet_price = gbp(3.0/2), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_block(jackpot = usd(seq(40,390,10)*1e6), lower_costs = usd(0.247/2), bet_price = gbp(3.0/2), odds = 302575350, tower = tower, jackpot_multiplier = 0.372*0.5),
    "BonoLoto" = margins_block(jackpot = eur(seq(0.5,7,0.5)*1e6), lower_costs = eur(0.2055/2), bet_price = gbp(1/2), odds = 13983816, tower = tower, jackpot_multiplier = 0.5)
  )
  
  sa_tenant_0.25 <- list(
    "EuroMillions" = margins_block(jackpot = eur(seq(20,190,10)*1e6), lower_costs = eur(0.572/4), bet_price = eur(3.50/4), odds = 139838160, tower = tower, jackpot_multiplier = 0.25),
    "EuroMillions-hedged" = margins_block(jackpot = eur(seq(20,190,10)*1e6), lower_costs = eur(0.572/4), bet_price = eur(3.50/4), odds = 139838160, tower = tower, jackpot_multiplier = 0.2*0.25, ticket_buying = list(cost = eur(2.5), type = "partial")),
    "EuroJackpot" = margins_block(jackpot = eur(seq(10,90,10)*1e6), lower_costs = eur(0.52/4), bet_price = gbp(2.2/4), odds = 95344200, tower = tower, jackpot_multiplier = 0.25),
    "Powerball" = margins_block(jackpot = usd(seq(40,500,10)*1e6), lower_costs = usd(0.32/4), bet_price = gbp(3.0/4), odds = 292201338, tower = tower, jackpot_multiplier = 0.372*0.25),
    "Powerball-hedged" = margins_block(jackpot = usd(seq(40,500,10)*1e6), lower_costs = usd(0.32/4), bet_price = gbp(3.0/4), odds = 292201338, tower = tower, jackpot_multiplier = 0.372, ticket_buying = list(cost = usd(2.3), type = "full")),
    "MegaMillions" = margins_block(jackpot = usd(seq(40,390,10)*1e6), lower_costs = usd(0.247/4), bet_price = gbp(3.0/4), odds = 302575350, tower = tower, jackpot_multiplier = 0.372*0.25),
    "BonoLoto" = margins_block(jackpot = eur(seq(0.5,7,0.5)*1e6), lower_costs = eur(0.2055/4), bet_price = gbp(1/4), odds = 13983816, tower = tower, jackpot_multiplier = 0.25)
  )
  
  export_tenant(sa_tenant_1.0, paste0("sa_tenant_1.0_", suffix, ".xlsx"))
  export_tenant(sa_tenant_0.5, paste0("sa_tenant_0.5_", suffix, ".xlsx"))
  export_tenant(sa_tenant_0.25, paste0("sa_tenant_0.25_", suffix, ".xlsx"))
  
  list(full = sa_tenant_1.0, half = sa_tenant_0.5, quarter = sa_tenant_0.25)
}

ils <- run_me(ils_tower(N = 1350), suffix = "ils")
ti5 <- run_me(trad_tower(N = 1350, retention = 5e6, capacity = 139.6e6), suffix = "trad_ins_5")
ti25 <- run_me(trad_tower(N = 1350, retention = 25e6, capacity = 119.6e6), suffix = "trad_ins_25")

first <- function(x){x[1]}
last <- function(x){tail(x,1)}

starting_at <- list(list("EuroMillions", first), list("EuroJackpot", first), list("Powerball", first), list("MegaMillions", first), list("BonoLoto", first))
ending_at <- list(list("EuroMillions-hedged", last), list("EuroJackpot", last), list("Powerball-hedged", last), list("MegaMillions", last), list("BonoLoto", last))

extract <- function(x, what) {
  sapply(what, function(p){
  p[[2]](x[[p[[1]]]]$Margin)
  })}

full_start <- t(sapply(list(ils$full, ti5$full, ti25$full), extract, starting_at))
half_start <- t(sapply(list(ils$half, ti5$half, ti25$half), extract, starting_at))
quarter_start <- t(sapply(list(ils$quarter, ti5$quarter, ti25$quarter), extract, starting_at))
start <- rbind(full_start, half_start, quarter_start)

full_end <- t(sapply(list(ils$full, ti5$full, ti25$full), extract, ending_at))
half_end <- t(sapply(list(ils$half, ti5$half, ti25$half), extract, ending_at))
quarter_end <- t(sapply(list(ils$quarter, ti5$quarter, ti25$quarter), extract, ending_at))
end <- rbind(full_end, half_end, quarter_end)

# tryCatch(run_me(), error = function(e){
#   print(e)
#   send_email(to = "georgios.sermaidis@mylotto24.co.uk", subject = "Automated margins error", body = "", attachment = "run_automated_margins.Rout")
#   file.remove("run_automated_margins.Rout")
# })


