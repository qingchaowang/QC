source("~/GitHub/R/data_extractors/eurojackpot/crawler.R")
source("~/GitHub/R/eurojackpot/eurojackpot.R")


data_crawler <- crawler()
hist_data <- historical_data("extracted_data.csv")

z <- data_crawler$process(hist_data$most_recent_date, hist_data$call)

hist_data$append(z)
