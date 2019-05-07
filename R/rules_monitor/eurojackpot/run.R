source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, hessen_eurojackpot_rules_page, "EuroJackpot", function(b){
  b$findElement("xpath", "//*[text()[contains(., 'Teilnahmebedingungen Eurojackpot\n')]]")
}), error = on_rule_error("EuroJackpot"))
browser$close()
