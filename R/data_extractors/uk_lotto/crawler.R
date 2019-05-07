extract_free_tickets <- function(x) {
  match2_k <- grep("2 match", x) 
  match2_line <- x[match2_k]
  string <- sub("2 match.*?0(.*)&.*", "\\1", match2_line)
  string <- gsub(",", "", string)
  as.numeric(string)
}

extract_date <- function(url_lines) {
  
  x <- paste(url_lines, collapse = " ")
  extracted <- sub(".*section_header\">.*<h1>[A-Za-z]*\\s.*?(.+)</h1.*", "\\1", x)
  extracted <- as.Date(extracted, "%d %b %Y")
  format(extracted, "%Y-%b-%d")
}

mersey_extract_sales <- function(url_lines) {
  
  Sys.setlocale('LC_ALL','C') 
  cur_locale <- Sys.getlocale()
  sales_k <- grep("Ticket sales", url_lines)
  sales_line <- url_lines[sales_k[1]]
  string <- sub(".*;([0-9,]+).*", "\\1", sales_line)
  string <- gsub(",", "", string)
  as.numeric(string)
}

official_extract_table <- function(url_string) {
  
  x <- htmltab::htmltab(url_string, 1)
  winners <- x[1:6,2]
  prize <- as.numeric(gsub("[£,]", "", x[1:5, 3]))
  fund <- as.numeric(gsub("[£,]", "", x[1:5, 4]))
  if(prize[1] == 0) prize[1] <- fund[1]
  result <- c(winners, prize)
  result <- as.numeric(gsub(",", "", result))
  names(result) <- c(paste("winners", 1:6, sep = "_"), paste("prize", 1:5, sep = "_"))
  result
}

mersey_url <- function(draw_number) {
  paste("http://lottery.merseyworld.com/archive/Lott", draw_number, ".html", sep = "")
}

uk_lotto_url <- function(draw_number) {
  paste("https://www.national-lottery.co.uk/results/lotto/draw-history/prize-breakdown/", draw_number, sep = "")
}

extract_draw_result <- function(draw_numbers) {
  
  mersey_urls <- mersey_url(draw_numbers)
  uk_lotto_urls <- uk_lotto_url(draw_numbers)
  
  sales <- sapply(mersey_urls, function(m_url){
    con_url <- url(m_url)
    result <- mersey_extract_sales(readLines(con_url))
    close(con_url)
    result
  })
  
  sales <- unname(sales)
  
  dates <- sapply(uk_lotto_urls, function(p_url){
    con_url <- url(p_url)
    result <- extract_date(readLines(con_url))
    close(con_url)
    result
  })
  
  official_tables <- lapply(uk_lotto_urls, official_extract_table)
  official_tables <- do.call(rbind, official_tables)
  
  result <- cbind(date = dates, draw_number = draw_numbers, sales, official_tables)
  result
}

is_draw_ready <- function(draw_number) {
  
  url_string <- uk_lotto_url(draw_number)
  draw_url <- url(url_string)
  result <- grep("Page not found", readLines(draw_url))
  close(draw_url)
  length(result)==0
}

update_data_file <- function() {
  
  x <- read.csv("extracted_data.csv")
  last_draw_number <- x[,2]
  last_draw_number <- last_draw_number[length(last_draw_number)]
  next_draw_number <- last_draw_number + 1
  
  while(is_draw_ready(next_draw_number)) {
    
    x <- rbind(x, extract_draw_result(next_draw_number))
    next_draw_number <- next_draw_number + 1
  }
  
  write.csv(x = x, file = "extracted_data.csv", row.names = F, quote = F)  
}






