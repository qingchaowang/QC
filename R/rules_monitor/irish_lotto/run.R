source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, lottery_ie_rules_page, "Irish Lotto", function(b){
  b$findElement("xpath", "//a[contains(., 'Game Rules for Lotto')]")
}), error = on_rule_error("Irish Lotto"))
browser$close()





