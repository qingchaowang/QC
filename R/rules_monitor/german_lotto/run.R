source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, hessen_lotto_rules_page, "German Lotto", function(b){
  b$findElement("xpath", "//*[text()[contains(., 'LOTTO 6aus49\n')]]")
}), error = on_rule_error("German Lotto"))
browser$close()





