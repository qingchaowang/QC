source("~/GitHub/R/tools/tools.R")
library(readxl)
all_data <- read_xlsx("~/Desktop/xlsx/historical/pbp_data.xlsx")
all_data$day <- factor(all_data$day, levels = c("Wed", "Sat"))
all_data$rojp_1 <- all_data$rojp_1/1e6
my_data <- all_data[-c(18:20),]
plot(entries ~ rojp_1, my_data, col = as.factor(day))
m1 <- irls(lm(entries ~ I(rojp_1 > 0) + rojp_1 + I(rojp_1^2) + I(rojp_1^3), subset(my_data, day == "Wed")), wfunc = function(x){1/x$fit^2}, 0.0000001)
m2 <- irls(lm(entries ~ I(rojp_1 > 0) + rojp_1 + I(rojp_1^2) + I(rojp_1^3), subset(my_data, day == "Sat")), wfunc = function(x){1/x$fit^2}, 0.0000001)
m3 <- irls(lm(entries ~ day*(I(rojp_1 > 0) + rojp_1 + I(rojp_1^2) + I(rojp_1^3)), my_data), wfunc = function(x){1/x$fit^2}, 0.0000001)

plot(entries ~ rojp_1, all_data, col = day)
ROJP <- seq(40, 20000, 10)
lines(ROJP, predict(m1, newdata = data.frame(rojp_1 = ROJP)), col = 1)
lines(ROJP, predict(m2, newdata = data.frame(rojp_1 = ROJP)), col = 2)


source("~/GitHub/R/tools/tools.R")
library(readxl)
all_data <- read_xlsx("~/Desktop/xlsx/historical/t24com_pb_data.xlsx")
all_data$rojp_1 <- all_data$rojp_1/1e6
all_data$day <- factor(all_data$day, levels = c("Wed", "Sat"))
my_data <- all_data[-c(1:5,24),]
plot(entries ~ rojp_1, my_data, col = day)
m1 <- glm(entries ~ I(rojp_1 > 0) + rojp_1, subset(my_data, day == "Wed"), family = poisson(link = "identity"))
m2 <- glm(entries ~ I(rojp_1 > 0) + rojp_1 + I(rojp_1^2), subset(my_data, day == "Sat"), family = poisson(link = "identity"))


# plot(entries ~ rojp_1, my_data, col = day)
plot(entries ~ rojp_1, my_data, col = day, xlim = c(0, 25000), ylim = c(0, 3000000))
ROJP <- seq(40, 20000, 10)
# lines(ROJP, predict(m1, newdata = data.frame(rojp_1 = ROJP)), col = 1)
lines(ROJP, predict(m2, newdata = data.frame(rojp_1 = ROJP)), col = 2)
lines(ROJP, predict(m3, newdata = data.frame(rojp_1 = ROJP)), col = 3)

ROJP <- as.numeric(pb_rollover[k[1],])
predict(m3, newdata = data.frame(rojp_1 = ROJP, day = rep(c("Wed", "Sat"), length.out = 104)))
