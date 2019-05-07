

sa_lotto_date_extractor <- function(browser) {
  browser$findElement("id", "drawDateLotto")
}

sa_powerball_date_extractor <- function(browser) {
  browser$findElement("class", "dateWrap")$findChildElement("class", "date")
}

sa_draw_extractor <- function(browser, date_extractor, class_name1, class_name2) {
  
  library(magrittr)
  
  game_table <- browser$findElement("css", paste0("div[class='", class_name1, "']"))
  
  draw_date <- date_extractor(browser)$getElementText()[[1]] %>%
    as.Date() %>%
    format("%Y-%b-%d")
  
  print(draw_date)
  
  tab1 <- game_table$findChildElements("class", "dataVal1") %>% 
    sapply(function(x){x$getElementText()[[1]]}) %>% 
    matrix(byrow = T, ncol = 3)
  
  winners <- as.numeric(tab1[,2])
  prize <- gsub("[R,]", "", tab1[,3]) %>% as.numeric()
  
  lotto_table <- browser$findElement("css", paste0("div[class='", class_name2, "']")) 
  
  tab2 <- lotto_table$findChildElements("class", "col") %>%
    sapply(function(x){x$getElementText()[[1]]}) %>%
    matrix(byrow = T, ncol = 2)
  
  more_info <- tab2[c(1,3,4,5),2] %>% 
    gsub("[R,]", "", .) %>% 
    as.numeric() %>% 
    set_names(c("rollover_amount", "prize_pool", "sales", "next_jackpot"))
  
  list(date = draw_date, winners = winners, prize = prize) %>% c(as.list(more_info))
}

read_page <- function(browser, partial_link_name, date_extractor, class_name1, class_name2) {
  
  i <- 1
  out <- list()
  draw_names <- browser$findElements("partial link text", partial_link_name) %>% sapply(function(x){x$getElementText()[[1]]})
  
  for(dn in draw_names) {
    
    browser$findElement("link text", dn)$clickElement()
    Sys.sleep(2)
    out[[i]] <- sa_draw_extractor(browser, date_extractor, class_name1, class_name2)
    browser$findElement("id", "back")$clickElement()
    Sys.sleep(2)
    i <- i+1
  }
  
  out
}

read_pages <- function(browser, partial_link_name, date_extractor, class_name1, class_name2) {
  
  out <- list()
  num_pages <- length(browser$findElements("class", "page_number"))
  
  if(num_pages) for(page in 1:num_pages) {
    
    pages <- browser$findElements("class", "page_number")
    pages[[page]]$clickElement()
    Sys.sleep(2)
    page_results <- read_page(browser, partial_link_name, date_extractor, class_name1, class_name2)
    out <- c(out, page_results)
  }
  else {
    out <- read_page(browser, partial_link_name, date_extractor, class_name1, class_name2)
  }
  
  rev(out)
}

navigate_to_results <- function(browser, initial_page, from_date, to_date) {

  from_date <- as.Date(from_date)
  to_date <- as.Date(to_date)
  browser$navigate(initial_page)
  Sys.sleep(2)
  browser$findElement("link", "HISTORICAL RESULTS")$clickElement()
  Sys.sleep(2)
  browser$findElement("id", "fromDate")$setElementAttribute("value", format(from_date, "%d/%m/%Y"))
  Sys.sleep(2)
  browser$findElement("id", "toDate")$setElementAttribute("value", format(to_date, "%d/%m/%Y"))
  Sys.sleep(2)
  browser$findElement("class", "btnBox")$clickElement()
  Sys.sleep(2)
}

update_data <- function(current_data_filename, browser, results_url, partial_link_name, date_extractor, class_name1, class_name2) {
  
  current_data <- read.csv(current_data_filename)
  latest_date <- as.Date(tail(current_data$date, 1), "%Y-%b-%d")
  navigate_to_results(browser, results_url, latest_date + 1, Sys.Date())
  Sys.sleep(2)
  out <- read_pages(browser, partial_link_name, date_extractor, class_name1, class_name2)
  new_data <- lapply(out, serialise_draw) %>% do.call(rbind, .) %>% rbind(current_data, .)
  write.csv(new_data, file = current_data_filename, quote = F, row.names = F)
}


