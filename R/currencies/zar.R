source("~/GitHub/R/currencies/tools.R")

ZAR <- setClass("ZAR", slots = c(amount = "numeric"))

zar <- function(amount) {
  ZAR(amount = amount)
}

set_currency_methods("ZAR")