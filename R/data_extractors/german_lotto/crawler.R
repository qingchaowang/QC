crawler <- function() {
  
  url_string <- "https://www.lotto-hessen.de/static/gamebroker_5/default/download_files/lotto2019.zip"
  system(command = paste("curl ", url_string, " -o glp_data.zip"))
  system(command = "tar xzvf glp_data.zip")
  system(command = "rm glp_data.zip")

  x <- read.table("lotto.txt", skip = 1, fill = T)
  remove_na <- mean(is.na(x[nrow(x),]))
  if(remove_na > 0) x <- x[-nrow(x),]
  x_dates <- paste(x[,4], x[,3], x[,2], sep = "/")
  x_dates <- as.Date(x_dates, "%Y/%m/%d")
  x <- x[,12:30]
  y <- do.call(data.frame, lapply(x, function(e){
    e <- gsub("Jackpot", 0, e)
    e <- gsub(",", ".", e)
    as.numeric(e)
  }))
  colnames(y) <- c("stakes", paste(c("prize_", "winners_"), rep(1:9, each = 2), sep = ""))
  y$date <- x_dates
  y$entries <- y$stakes
  
  system(command = "rm lotto.txt")
  
  process <- function(after_date, listener) {
    
    out <- list()
    
    n <- nrow(y)
    i <- which(y$date > after_date)[1]
    
    j <- 1
    
    while(i <= n) {
      
      out[[j]] <- listener(y[i,])
      i <- i + 1
      j <- j + 1
    }
    
    out
  }
  
  list(process = process)
}
