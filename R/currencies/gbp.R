source("~/GitHub/R/currencies/tools.R")

GBP <- setClass("GBP", slots = c(amount = "numeric"))

gbp <- function(amount) {
  GBP(amount = amount)
}

set_currency_methods("GBP")