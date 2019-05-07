source("../tools.R")

tryCatch(camelot_check_rule_change("https://www.national-lottery.co.uk/games/euromillions/game-procedures", "Edition 20 effective 18th November 2018", "EuroMillions"), 
         error = on_rule_error("EuroMillions"))
