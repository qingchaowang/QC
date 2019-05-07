source("../tools.R")

driver <- RSelenium::rsDriver(browser = "chrome")
browser <- driver$client
tryCatch(pdf_rule_change(browser, "https://www.lottery.ie/games/game-info/how-to-play-lotto-plus-game", "Irish Lotto Plus", function(b){
  b$findElement("partial link text", "Lotto Plus Game Rules booklet")
}), error = on_rule_error("Irish Lotto Plus"))

