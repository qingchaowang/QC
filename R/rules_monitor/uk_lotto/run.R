source("../tools.R")

tryCatch(camelot_check_rule_change("https://www.national-lottery.co.uk/games/lotto/game-procedures", "Edition 18 effective 18th November 2018", "UK Lotto"),
         error = on_rule_error("UK Lotto"))

