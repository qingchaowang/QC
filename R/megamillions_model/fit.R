source("~/GitHub/R/tools/tools.R")
library(readxl)
all_data <- read_xlsx("~/Desktop/xlsx/historical/mmp_data.xlsx")
all_data$rojp_1 <- all_data$rojp_1/1e6
my_data <- all_data
plot(entries ~ rojp_1, my_data, col = as.factor(day))
m2 <- irls(lm(entries ~ I(rojp_1 > 0) + rojp_1 + I(rojp_1^2), my_data), wfunc = function(x){1/x$fit^2}, 0.0000001)
m3 <- irls(lm(entries ~ I(rojp_1 > 0) + rojp_1 + I(rojp_1^2) + I(rojp_1^3), my_data), wfunc = function(x){1/x$fit^2}, 0.0000001)

ROJP <- seq(40, 500, 10)
lines(ROJP, predict(m2, newdata = data.frame(rojp_1 = ROJP)), col = 2)
lines(ROJP, predict(m3, newdata = data.frame(rojp_1 = ROJP)), col = 3)

plot(jj_entries ~ rojp_1, my_data, col = as.factor(day))
plot(jj_entries ~ rojp_1, my_data, col = c(rep(1,20), rep(2,12)))
jj2 <- irls(lm(jj_entries ~ I(rojp_1 > 0) + rojp_1, my_data[-c(1:20),]), wfunc = function(x){1/x$fit^2}, 0.0000001)
lines(ROJP, predict(jj2, newdata = data.frame(rojp_1 = ROJP)), col = 2)


source("~/GitHub/R/tools/tools.R")
library(readxl)
all_data <- read_xlsx("~/Desktop/xlsx/historical/t24com_mm_data.xlsx")
all_data$rojp_1 <- all_data$rojp_1/1e6
all_data$day <- factor(all_data$day, levels = c("Tue", "Fri"))
my_data <- all_data[-c(1:2),]
plot(entries ~ rojp_1, my_data, col = day)
m2 <- irls(lm(entries ~ I(rojp_1 > 0) + rojp_1, my_data), wfunc = function(x){1/x$fit^2}, 0.0000001)
m3 <- glm(entries ~ I(rojp_1 > 0) + rojp_1, my_data, family = poisson(link = "identity"))

# plot(entries ~ rojp_1, my_data, col = day, xlim = c(0, 1000), ylim = c(0, 1))
plot(entries ~ rojp_1, my_data, col = day)
ROJP <- seq(40, 1000, 10)
lines(ROJP, predict(m2, newdata = data.frame(rojp_1 = ROJP)), col = 1)

