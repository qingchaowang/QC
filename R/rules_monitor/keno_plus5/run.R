source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, hessen_keno_rules_page, "Keno Plus5", function(b){
  b$findElement("xpath", "//a[contains(., 'Teilnahmebedingungen plus5\n')]")
}), error = on_rule_error("Keno Plus5"))
browser$close()
