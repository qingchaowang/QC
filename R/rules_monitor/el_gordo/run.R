source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, el_gordo_rules_page, "El Gordo", function(b){
  b$findElement("xpath", "//*[contains(@href, 'pdf')]")
}), error = on_rule_error("El Gordo"))

