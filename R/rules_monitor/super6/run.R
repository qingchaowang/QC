source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, hessen_eurojackpot_rules_page, "SUPER 6", function(b){
  b$findElement("xpath", "//*[text()[contains(., 'SUPER 6')]]")
}), error = on_rule_error("Super 6"))

