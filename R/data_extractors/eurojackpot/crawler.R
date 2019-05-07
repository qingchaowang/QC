crawler <- function() {
  
  url_string <- "https://www.lotto-hessen.de/static/gamebroker_5/default/download_files/eurojackpot2019.zip"
  system(command = paste("curl ", url_string, " -o ejp_data.zip"))
  system(command = "tar xzvf ejp_data.zip")
  system(command = "rm ejp_data.zip")
  
  x <- read.table("eurojackpot.txt", skip = 1)
  x_dates <- paste(x[,4], x[,3], x[,2], sep = "/")
  x_dates <- as.Date(x_dates, "%Y/%m/%d")
  
  y <- x[,seq(12,36,2)]
  y <- do.call(data.frame, lapply(y, function(x){
    as.numeric(sub("Jackpot", 0, x))})
  )
  y <- data.frame(x_dates, y)
  prizes <- x[, seq(13,35,2)]
  prizes <- do.call(data.frame, lapply(prizes, function(x){
    as.numeric(gsub(",", ".", x))
  }))
  y <- cbind(y, prizes)
  colnames(y) <- c("date", "stakes", paste("winners", 1:12, sep = "_"), paste("prize", 1:12, sep = "_"))
  y$entries <- y$stakes/2
  
  system(command = "rm eurojackpot.txt")
  
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


