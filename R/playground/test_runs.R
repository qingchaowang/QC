setwd("/Users/qingchao.wang/Documents/github/R/tyche")
source("reset_tools.R")
require(parallel)

# gls <- read.xlsx("/Users/georgios.sermaidis/Work/Tyche/premium_calculation/mock_up_monitoring.xlsm", 1, startRow = 4)
# ems <- read.xlsx("/Users/georgios.sermaidis/Work/Tyche/premium_calculation/mock_up_monitoring.xlsm", 2, startRow = 4)
# ejs <- read.xlsx("/Users/georgios.sermaidis/Work/Tyche/premium_calculation/mock_up_monitoring.xlsm", 3, startRow = 4)

# gls <- read.xlsx("/Volumes/dfs/Hedging Management/003_Analysts/Tyche/premium_calculation/mock_up_update.xlsm", 1, startRow = 4)
# ems <- read.xlsx("/Volumes/dfs/Hedging Management/003_Analysts/Tyche/premium_calculation/mock_up_update.xlsm", 2, startRow = 4)
# ejs <- read.xlsx("/Volumes/dfs/Hedging Management/003_Analysts/Tyche/premium_calculation/mock_up_update.xlsm", 3, startRow = 4)

gls15 <- init_data("/Volumes/dfs/Hedging Management/003_Analysts/QC WANG/hist_2015.xlsx", 1, "gls")
ems15 <- init_data("/Volumes/dfs/Hedging Management/003_Analysts/QC WANG/hist_2015.xlsx", 2, "ems")
ejs15 <- init_data("/Volumes/dfs/Hedging Management/003_Analysts/QC WANG/hist_2015.xlsx", 3, "ejs")
data15 <- combine_data(gls15, ejs15, ems15)

cl <- makeCluster(rep("localhost", 6))

sim15 <- sim_payouts(cl, data15, 100000, identity)



actual_output15 <- sapply(data15, function(x){matrix(x$secondary.winners * x$jackpot)})
actual_output16 <- sapply(data16, function(x){matrix(x$secondary.winners * x$jackpot)})



test_f <- function(data, sim_data, cover) {
  
  for(i in 1:length(data)) {
    
    pays <- sim_data[[i]]
    
    for(j in 1:nrow(pays)) {
      
      cover$process(data[[i]]$bets[j], data[[i]]$odds[j], data[[i]]$jackpot[j])
      cover$claim(sim_data[[i]][j,])
    }
  }
}

test_f <- function(data, sim_data, gls_cover, ems_cover, ejs_cover) {
  
  for(i in 1:length(data)) {
    
    pays <- sim_data[[i]]
    
    for(j in 1:nrow(pays)) {
      
      cover <- NULL
      
      if(data[[i]]$type[1] == "gls") {
        cover <- gls_cover
      }
      
      if(data[[i]]$type[1] == "ems") {
        cover <- ems_cover
      }
      
      if(data[[i]]$type[1] == "ejs") {
        cover <- ejs_cover
      }
      
      cover$process(data[[i]]$bets[j], data[[i]]$odds[j], data[[i]]$jackpot[j])
      cover$claim(sim_data[[i]][j,])
    }
  }
}


N <- 100000

retention <- ins_layer(N, 15e6, F)
ti_retention <- ins_layer(N, 15e6, T, list(multiple = 1.7, ipt = 0))
jnr <- ins_layer(N, 25e6, F)
srb <- ins_layer(N, 25e6, F)
sra <- ins_layer(N, 25e6, F)
my_tower <- tower(retention, ti_retention, jnr, srb, sra)


gls_retention <- ins_layer(N, 15e6, F)
ems_retention <- ins_layer(N, 15e6, F)
ejs_retention <- ins_layer(N, 15e6, F)
ti_retention <- ins_layer(N, 15e6, T, list(multiple = 1.7, ipt = 0))
jnr <- ins_layer(N, 25e6, F)
srb <- ins_layer(N, 25e6, F)
sra <- ins_layer(N, 25e6, F)
gls_tower <- tower(gls_retention, ti_retention, jnr, srb, sra)
ems_tower <- tower(ems_retention, ti_retention, jnr, srb, sra)
ejs_tower <- tower(ejs_retention, ti_retention, jnr, srb, sra)


test_f(data15, sim15, gls_tower, ems_tower, ejs_tower)





gls_retention <- ins_layer(1, 15e6, F)
ems_retention <- ins_layer(1, 15e6, F)
ejs_retention <- ins_layer(1, 15e6, F)
ti_retention2 <- ins_layer(1, 15e6, T, list(multiple = 1.7, ipt = 0))
jnr2 <- ins_layer(1, 25e6)
srb2 <- ins_layer(1, 25e6)
sra2 <- ins_layer(1, 25e6)
gls_tower <- tower(gls_retention, ti_retention2, jnr2, srb2, sra2)
ems_tower <- tower(ems_retention, ti_retention2, jnr2, srb2, sra2)
ejs_tower <- tower(ejs_retention, ti_retention2, jnr2, srb2, sra2)


for(i in 1:length(actual_output16)) {
  
  pays <- actual_output16[[i]]
  
  for(j in 1:nrow(pays)) {
    
    if(data16[[i]]$type[1] == "gls") {
      gls_tower$process(data16[[i]]$bets[j], data16[[i]]$odds[j], data16[[i]]$jackpot[j])
      gls_tower$claim(pays[j,])  
    }
    
    if(data16[[i]]$type[1] == "ems") {
      ems_tower$process(data16[[i]]$bets[j], data16[[i]]$odds[j], data16[[i]]$jackpot[j])
      ems_tower$claim(pays[j,])  
    }
    
    if(data16[[i]]$type[1] == "ejs") {
      ejs_tower$process(data16[[i]]$bets[j], data16[[i]]$odds[j], data16[[i]]$jackpot[j])
      ejs_tower$claim(pays[j,])  
    }
  }
}












for(i in 1:length(del)) {
  
  pays <- del[[i]]
  
  for(j in 1:nrow(pays)) my_tower$claim(pays[j,])
}

for(i in 1:length(del)) {
  
  pays <- del[[i]]
  
  for(j in 1:nrow(pays)) {
    if( data15[[i]]$type[1] == "gls") {
      
      gls_tower$process(data15[[i]]$bets, data15[[i]]$odds, data15[[i]]$jackpot)
      gls_tower$claim(pays[j,])
      }
    
    if( data15[[i]]$type[1] == "ems") {
      
      ems_tower$process(data15[[i]]$bets, data15[[i]]$odds, data15[[i]]$jackpot)
      ems_tower$claim(pays[j,])
    }
    
    if( data15[[i]]$type[1] == "ejs") {
      
      ejs_tower$process(data15[[i]]$bets, data15[[i]]$odds, data15[[i]]$jackpot)
      ejs_tower$claim(pays[j,])
    }
  }
}


clusterExport(cl, list("calc_total_winners_unit"))


gls_losses <- sim_losses_for_reset(cl, 1000000, gls)
ems_losses <- sim_losses_for_reset(cl, 1000000, ems)
ejs_losses <- sim_losses_for_reset(cl, 1000000, ejs)
losses <- cbind(gls_losses, ems_losses, ejs_losses)
qs <- c(0.1, 0.5, 10, 20, 25, 50, 75, 80, seq(90, 99, 1), seq(99.1, 99.9, 0.1), 99.95, 99.99, 99.999, 99.9999, 100)/100
result <- apply(losses, 2, descr, qs)
write.csv(result, "result.csv")
