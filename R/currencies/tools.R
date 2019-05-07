set_currency_methods <- function(tag) {
  
  currency_plus <- "setMethod(\"+\", c(\"TAG\", \"TAG\"), function(e1, e2) {
  TAG(amount = e1@amount + e2@amount)
})"
  
  currency_minus <- "setMethod(\"-\", c(\"TAG\", \"TAG\"), function(e1, e2) {
  TAG(amount = e1@amount - e2@amount)
  })"
  
  currency_mult_num1 <- "setMethod(\"*\", c(\"TAG\", \"numeric\"), function(e1, e2) {
  TAG(amount = e1@amount*e2)
  })"
  
  currency_mult_num2 <- "setMethod(\"*\", c(\"numeric\", \"TAG\"), function(e1, e2) {
  e2*e1
  })"
  
  currency_div <- "setMethod(\"/\", c(\"TAG\", \"TAG\"), function(e1, e2) {
  e1@amount/e2@amount
})"
  
  currency_div_num <- "setMethod(\"/\", c(\"TAG\", \"numeric\"), function(e1, e2) {
  TAG(amount = e1@amount/e2)
})"
  
  currency_sum <- "setMethod('sum', 'TAG', function(x, ...) {
  TAG(amount = sum(x@amount,...))
})"
  
  eval(parse(text = gsub("TAG", tag, currency_plus)))
  eval(parse(text = gsub("TAG", tag, currency_minus)))
  eval(parse(text = gsub("TAG", tag, currency_mult_num1)))
  eval(parse(text = gsub("TAG", tag, currency_mult_num2)))
  eval(parse(text = gsub("TAG", tag, currency_div)))
  eval(parse(text = gsub("TAG", tag, currency_div_num)))
  eval(parse(text = gsub("TAG", tag, currency_sum)))
}

as_currency <- function(x, tag) {
  eval(parse(text = paste0(tag, "(amount = x)")))
}

  