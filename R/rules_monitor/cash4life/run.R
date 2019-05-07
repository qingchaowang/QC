source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, "https://nylottery.ny.gov/cash4life", "Cash4Life", function(b){
  b$findElement("partial link text", "PRINTABLE PDF")
}), error = on_rule_error("Cash4Life"))

