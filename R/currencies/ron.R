source("~/GitHub/R/currencies/tools.R")

RON <- setClass("RON", slots = c(amount = "numeric"))

ron <- function(amount) {
  RON(amount = amount)
}

set_currency_methods("RON")