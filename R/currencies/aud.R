source("~/GitHub/R/currencies/tools.R")

AUD <- setClass("AUD", slots = c(amount = "numeric"))

aud <- function(amount) {
  AUD(amount = amount)
}

set_currency_methods("AUD")
