source("cost_per_bet.R")

x <- read_losses("~/GitHub/R/sims/output_110")
x <- x*1.1818

r_layer <- tyche_cost(x[,1], 1, 0, make_layer_f(30e6, 0), "retention")
c_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(30e6, 30e6), "c_layer")
b_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(30e6, 60e6), "b_layer")
a_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(30e6, 90e6), "a_layer")

# e1_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(50e6, 120e6), "e1_layer")
# e2_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(25e6, 170e6), "e2_layer")
# e3_layer <- tyche_cost(x[,1], 1.35, 0.12, make_layer_f(25e6, 195e6), "e3_layer")
# 
# o_layer <- trad_ins_cost(1.6, 0.12, make_layer_f(33e6, 120e6), "s_layer")
# o1_layer <- trad_ins_cost(1.6, 0.12, make_layer_f(33e6, 170e6), "s_layer")
# o2_layer <- trad_ins_cost(1.6, 0.12, make_layer_f(33e6, 195e6), "s_layer")
# o3_layer <- trad_ins_cost(1.6, 0.12, make_layer_f(33e6, 220e6), "s_layer")

ils_tower <- list(r_layer, c_layer, b_layer, a_layer)
# ils1_tower <- list(r_layer, c_layer, b_layer, a_layer, e1_layer, o1_layer)
# ils2_tower <- list(r_layer, c_layer, b_layer, a_layer, e1_layer, e2_layer, o2_layer)
# ils3_tower <- list(r_layer, c_layer, b_layer, a_layer, e1_layer, e2_layer, e3_layer, o3_layer)

require(xlsx)

gls_cost <- marginal_cost(seq(1,500,1)*1e5, 139838160, ils_tower)
write.xlsx(gls_cost, file = "margins.xlsx", row.names = F, sheetName = "GLS", append = F)

ejs_cost <- marginal_cost(seq(1,1350,1)*1e5, 95344200, ils_tower)
write.xlsx(ejs_cost, file = "margins.xlsx", row.names = F, sheetName = "EJS", append = T)

ems_cost <- marginal_cost(seq(1,1530,1)*1e5, 139838160, ils_tower)
write.xlsx(ems_cost, file = "margins.xlsx", row.names = F, sheetName = "EMS", append = T)

irls_cost <- marginal_cost(seq(1,300,1)*1e5, 10737573, ils_tower)
write.xlsx(irls_cost, file = "margins.xlsx", row.names = F, sheetName = "IRLS", append = T)

pbs_cost <- marginal_cost(seq(1,1530,1)*1e5, 292201338, ils_tower)
write.xlsx(pbs_cost, file = "margins.xlsx", row.names = F, sheetName = "PBS", append = T)

mms_cost <- marginal_cost(seq(1,1530,1)*1e5, 302575350, ils_tower)
write.xlsx(mms_cost, file = "margins.xlsx", row.names = F, sheetName = "MMS", append = T)

ses_cost <- marginal_cost(seq(1,153,1)*1e6, 622614630, ils1_tower)
write.csv(ses_cost, "ses.csv")

ses1_cost <- marginal_cost(seq(1,203,1)*1e6, 622614630, ils1_tower)
write.csv(ses1_cost, "ses1.csv")

ses2_cost <- marginal_cost(seq(1,228,1)*1e6, 622614630, ils2_tower)
write.csv(ses2_cost, "ses2.csv")

ses3_cost <- marginal_cost(seq(1,253,1)*1e6, 622614630, ils3_tower)
write.csv(ses3_cost, "ses5.csv")

uk_lotto <- marginal_cost(seq(1e6,55e6,1e5), 45057474, ils_tower)

sa_lotto <- marginal_cost(seq(1e5,15e6,1e5), 20358520, ils_tower)

# Lower class cost
prob <- dhyper(1:6, 6, 84, 6)
lower_class_cost <- c(one_number = 1 * prob[1], two_number = 5 * prob[2], three_number = 0.6 * .128, 
                      four_number = 0.6 * .042, five_number = 0.6 * .042)
sum(lower_class_cost)


swedish_ticket_f <- function(jackpot, weekday){
  if (weekday == 6)
    return (6476879)
  if (weekday == 3)
    return (2618139)
}

sls_cost_Wed <- marginal_cost(cl, seq(1, 40, 0.1) * 1e6 * .12, 6724520, ils_tower, weekday = 3)
sls_cost_Sat <- marginal_cost(cl, seq(1, 40, 0.1) * 1e6 * .12, 6724520, ils_tower, weekday = 6)


