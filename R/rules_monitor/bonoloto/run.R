source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, bonoloto_rules_page, "Bonoloto", function(b){
  b$findElement("xpath", "//*[contains(@href, 'pdf')]")
}), error = on_rule_error("Bonoloto"))

