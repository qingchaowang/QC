source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, hessen_keno_rules_page, "Keno", function(b){
  b$findElement("xpath", "//a[contains(., 'Teilnahmebedingungen KENO\n')]")
}), error = on_rule_error("Keno"))
browser$close()





