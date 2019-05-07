source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, "https://www.coloradolottery.com/en/about/rules/", "Powerball", function(b){
  b$findElement("xpath", "//a[contains(@href,'.pdf') and contains(text(), 'Powerball') and not(contains(text(), 'with'))]")
}), error = on_rule_error("Powerball"))

