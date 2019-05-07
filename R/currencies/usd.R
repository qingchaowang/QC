source("~/GitHub/R/currencies/tools.R")

USD <- setClass("USD", slots = c(amount = "numeric"))

usd <- function(amount) {
  USD(amount = amount)
}

set_currency_methods("USD")