source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, "https://www.nationallottery.co.za/about/how-to-lotto", "SA Lotto", function(b){
  b$findElement("xpath", "//button[text()='Rules and regulations']/..")
}, function(e){e$getElementAttribute("data-href")[[1]]}), error = on_rule_error("SA Lotto"))
browser$close()
