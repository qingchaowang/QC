source("~/GitHub/R/currencies/tools.R")

SEK <- setClass("SEK", slots = c(amount = "numeric"))

sek <- function(amount) {
  SEK(amount = amount)
}

set_currency_methods("SEK")
