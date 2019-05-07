source("~/GitHub/R/tyche/reset_tools.R")

ms_data <- read_monitoring_sheet(filename = "~/Desktop/Monitoring_Sheet_ML24_22062018.xlsx",
                                 starting_row = 4, 
                                 start_date = "2018-1-1",
                                 end_date = "2018-6-22")

require(parallel)
cl <- makeCluster(rep("localhost", 6))
sim <- sim_payouts(cl, ms_data, 4e6, poisson_winners_sim, function(x){apply(x, 2, sum)})
rm(cl)
gc()
modelled_el <- read_modelled_el()
tower <- list(list(capacity = 30e6, att_point = 30e6),  list(capacity = 30e6, att_point = 60e6), list(capacity = 30e6, att_point = 90e6))
ils_el18 <- ils_el_per_draw_last(sim, tower)


del1 <- calc_premium_metrics(ils_el18, modelled_el, "2018-1-31")
del2 <- calc_premium_metrics(ils_el18, modelled_el, "2018-2-28")
del3 <- calc_premium_metrics(ils_el18, modelled_el, "2018-3-31")
del4 <- calc_premium_metrics(ils_el18, modelled_el, "2018-4-30")
del5 <- calc_premium_metrics(ils_el18, modelled_el, "2018-5-31")
del6 <- calc_premium_metrics(ils_el18, modelled_el, "2018-6-22")
# EL_graph(list(c(5,12,19,26,33,40)), ils_el18)
# dev.off()