source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, hessen_lotto_rules_page, "Spiel 77", function(b){
  b$findElement("xpath", "//*[text()[contains(., 'Spiel 77')]]")
}), error = on_rule_error("Spiel 77"))

