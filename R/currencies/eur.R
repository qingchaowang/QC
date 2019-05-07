source("~/GitHub/R/currencies/tools.R")

EUR <- setClass("EUR", slots = c(amount = "numeric"))

eur <- function(amount) {
  EUR(amount = amount)
}

set_currency_methods("EUR")
