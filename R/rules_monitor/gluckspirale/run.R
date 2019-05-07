source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, hessen_lotto_rules_page, "GlucksSpirale", function(b){
  b$findElement("xpath", "//*[text()[contains(., 'GlücksSpirale')]]")
}), error = on_rule_error("GlucksSpirale"))
browser$close()
