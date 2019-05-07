source("~/GitHub/R/rules/rules.R")
source("~/GitHub/R/data_extractors/sa_games/draw_extraction.R")

library(RSelenium)
library(magrittr)

driver <- rsDriver(browser = "chrome")
browser <- driver$client

update_data("extracted_data_lotto.csv", browser, "https://www.nationallottery.co.za/results/lotto", "LOTTO DRAW", sa_lotto_date_extractor, "tableWrap gameTable2", "tableWrap lottoTable3")
update_data("extracted_data_lotto1.csv", browser, "https://www.nationallottery.co.za/results/lotto-plus-1-results", "LOTTO PLUS 1 DRAW", sa_lotto_date_extractor, "tableWrap gameTable2", "tableWrap lottoTable3")
update_data("extracted_data_lotto2.csv", browser, "https://www.nationallottery.co.za/results/lotto-plus-2-results", "LOTTO PLUS 2 DRAW", sa_lotto_date_extractor, "tableWrap gameTable2", "tableWrap lottoTable3")
update_data("extracted_data_powerball.csv", browser, "https://www.nationallottery.co.za/results/powerball", "POWERBALL DRAW", sa_powerball_date_extractor, "tableWrap gameTable2", "tableWrap gameTable3")
update_data("extracted_data_powerball_plus.csv", browser, "https://www.nationallottery.co.za/results/powerball-plus", "POWERBALL PLUS DRAW", sa_powerball_date_extractor, "tableWrap gameTable2", "tableWrap gameTable3")
browser$close()
