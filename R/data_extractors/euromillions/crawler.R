pdf_url <- function(draw_url, draw_date) {
  
  prefix <- "https://www.loteriasyapuestas.es/"
  con <- url(draw_url)
  conLines <- readLines(con)
  pattern <- paste(".*pdf.*Nota de Prensa del sorteo del ", format(draw_date, "%d/%m/%Y"), sep = "")
  k <- grep(pattern = pattern, conLines, ignore.case = T)
  if(length(k) > 1) {
    l <- grep(paste(pattern, " Euromillones*", sep = ""), conLines[k], ignore.case = T)
    k <- k[l]
  }
  pdf_line <- sub(".*(f/loterias/.*pdf).*", "\\1", conLines[k])
  pdf_line <- gsub(" ", "%20", pdf_line)
  close(con)
  paste(prefix, pdf_line, sep = "")
}

is_last_draw <- function(url_string) {
  con <- url(url_string)
  res <- length(grep(pattern = "view next", readLines(con))) == 0
  close(con)
  res
}

update_data_file <- function() {
  
  require(rvest)
  
  y <- read.csv(file = "extracted_data.csv")
  y[,1] <- as.character(y[,1])
  y[,2] <- as.character(y[,2])
  latest_draw_url <- tail(y[,1], n = 1)
  latest_draw_date <- as.Date(y[,2], "%Y-%b-%d")
  latest_draw_date <- latest_draw_date[length(latest_draw_date)]
  my_session <- html_session(latest_draw_url)
  
  while(!is_last_draw(my_session$url)) {
    
    my_session <- follow_link(my_session, "view next prize draw")
    n_draw_date <- (my_session %>% html_nodes(".contenidoRegion .cabeceraRegion .tituloRegion h2") %>% html_text())
    n_draw_date <- tail(n_draw_date, n = 1)
    n_draw_date <- sub(".*, +(.*)", "\\1", n_draw_date)
    n_draw_date <- as.Date(n_draw_date, "%d %B %Y")
    n_draw_url <- my_session$url
    tab <- htmltab::htmltab(n_draw_url, 1)
    winners <- as.numeric(gsub(",", "", tab[,4]))
    prizes <- as.numeric(gsub("[^0-9\\.]+", "", tab$Prizes))
    
    download.file(url = pdf_url(n_draw_url, n_draw_date), destfile = "draw.pdf")
    text <- pdftools::pdf_text("draw.pdf")
    stakes <- sub(".*sorteo ascendió a\\s+(.*)\\s+.-euros.*", "\\1", text)
    stakes <- gsub("\\.", "", stakes)
    stakes <- as.numeric(gsub(",", ".", stakes))
    
    y <- rbind(y, c(n_draw_url, format(n_draw_date, "%Y-%b-%d"), stakes, winners, prizes))
    
    system(command = "rm draw.pdf")
    write.csv(x = y, file = "extracted_data.csv", row.names = F, quote = F)
  }
}


