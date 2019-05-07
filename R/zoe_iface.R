game_query <- function(game)
{
  query <- "//a[contains(@href, 'game')]"
  sub("game", game, query)
}

pool_query <- function(game_date)
{
  query <- "//*[contains(@data-participation-pool-id, 'game_date') and contains(@title, 'Bet count breakdown')]"
  sub("game_date", game_date, query)
}

modal_query <- function(game_date)
{
  query <- "//*[contains(text(), 'game_date')]//ancestor::*[contains(@class, 'modal-content')]"
  sub("game_date", game_date, query)
}

extract_rate_on_closest_date <- function(date, src_currency, dst_currency)
{
  library(dplyr)
  download.file(url = 'https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip', destfile = 'exchange_rate.zip')
  unzip("exchange_rate.zip")
  file.remove("exchange_rate.zip")
    
  x <- read.csv("eurofxref-hist.csv")
  x[,1] <- as.Date(x[,1])
  y <- (x %>% filter(Date <= date))[1,]
  if(src_currency == dst_currency) return("1")
  else if(src_currency == "EUR") return(y[[dst_currency]])
  else return(paste0('=', y[[dst_currency]], '/', y[[src_currency]]))
}

extract_bets_on_date <- function(browser, game, date, game_cur)
{
  date <- as.Date(date)
  game_date <- paste0(game, "/", date)
  fx_rate <- extract_rate_on_closest_date(date - 1, game_cur, "USD")
  
  browser$navigate("https://zoe-ui-m24.livec.sg-cloud.co.uk/processing")
  Sys.sleep(3)
  browser$findElement("xpath", game_query(game))$clickElement()
  Sys.sleep(2)
  browser$findElement("xpath", pool_query(game_date))$clickElement()
  Sys.sleep(2)
  modal <- browser$findElement("xpath", modal_query(game_date))
  Sys.sleep(1)
  plus_btns <- modal$findChildElements("class", "glyphicon-plus")
  sapply(plus_btns, function(e){e$clickElement()})
  Sys.sleep(2)
  table <- modal$findChildElement("xpath", "//div[contains(@id, 'betCountDetails')]//tbody")
  table_nodes <- table$findChildElements("xpath", "node()")
  out <- table_nodes[-c(1:3)] %>% sapply(function(e){e$getElementText()}) %>%
    lapply(function(x){
      y <- x %>% gsub(",", "", .) %>% gsub("\n", " ", .)
      name <- gsub(" *(.*) (\\d+ \\d+ \\d+)$", "\\1", y)
      figures_txt <- gsub(" *(.*) (\\d+ \\d+ \\d+)$", "\\2", y)
      figures <- as.numeric(strsplit(figures_txt, " ")[[1]])
      data.frame(date = format(date, "%d/%m/%Y"), name = name, onrisk = figures[1], offrisk = figures[2], total = figures[3]) %>%
        split_entry()
    }) %>% 
    do.call(rbind, .)
  
  exclude_names <- name_map(as.character(out$name)) %>% sapply(is.null) %>% which()
  out <- out[-exclude_names,]
  out$name <- name_map(as.character(out$name))
  out$func <- func_map(game, out$name %>% unlist())
  out$special <- special_map(game, out$name %>% unlist())
  out$fx_rate <- fx_rate
  out$game <- "normal"
  out$type <- "MUST_FILL"
  out[c("date", "name", "game", "type", "offrisk", "func", "special", "fx_rate", "bets")]
}

split_entry <- function(e)
{
  out <- NULL
  if(e$onrisk > 0) out <- data.frame(date = e$date, name = e$name, offrisk = "N", bets = e$onrisk)
  if(e$offrisk > 0) 
  {
    off_risk <- data.frame(date = e$date, name = e$name, offrisk = "Y", bets = e$offrisk)
    out <- rbind(out, off_risk)
  }
  out
}

name_map <- function(zoe_names)
{
  map <- list(
    TIPP24_COM = "tipp24.com",
    MYLOTTO24_UK = "ML24.uk",
    MYLOTTO24_IE = "ML24.ie",
    "Gameworx Ltd." = "Gameworx",
    "Lottoday Ltd." = "Lottoday"
    )
  
  map[zoe_names]
}

func_map <- function(game, partners)
{
  map <- list(
    gls = list(tipp24.com = "MAX", ML24.uk = "SUM", ML24.ie = "SUM"),
    ems = list(tipp24.com = "MAX", ML24.uk = "SUM", ML24.ie = "SUM", Gameworx = "NULL", Lottoday = "SUM"),
    ejs = list(tipp24.com = "MAX", ML24.uk = "SUM", ML24.ie = "SUM", Lottoday = "SUM"),
    uspbls = list(tipp24.com = "SUM", ML24.uk = "SUM", ML24.ie = "SUM", Gameworx = "NULL", Lottoday = "SUM"),
    mmls = list(tipp24.com = "SUM", ML24.uk = "SUM", ML24.ie = "SUM", Lottoday = "SUM"),
    irls = list(tipp24.com = "SUM", ML24.uk = "SUM", ML24.ie = "SUM"),
    ukls = list(ML24.ie = "SUM")
  )
  
  map[[game]][partners]
}

special_map <- function(game, partners)
{
  map <- list(
    gls = list(tipp24.com = "Y", ML24.uk = "N", ML24.ie = "N"),
    ems = list(tipp24.com = "Y", ML24.uk = "Y", ML24.ie = "Y", Gameworx = "N", Lottoday = "N"),
    ejs = list(tipp24.com = "Y", ML24.uk = "N", ML24.ie = "N", Lottoday = "N"),
    uspbls = list(tipp24.com = "N", ML24.uk = "N", ML24.ie = "N", Gameworx = "N", Lottoday = "N"),
    mmls = list(tipp24.com = "N", ML24.uk = "N", ML24.ie = "N", Lottoday = "N"),
    irls = list(tipp24.com = "N", ML24.uk = "N", ML24.ie = "N"),
    ukls = list(ML24.ie = "N")
  )
  
  map[[game]][partners]
}


library(magrittr)
driver <- RSelenium::rsDriver()
browser <- driver$client
browser$navigate("https://zoe-ui-m24.livec.sg-cloud.co.uk/login")

z1 <- extract_bets_on_date(browser, "gls", "2019-1-12", "EUR")
z2 <- extract_bets_on_date(browser, "gls", "2019-1-16", "EUR")
write.csv(as.matrix(rbind(z1, z2)), "gls.csv")

z1 <- extract_bets_on_date(browser, "ems", "2019-1-11", "EUR")
z2 <- extract_bets_on_date(browser, "ems", "2019-1-15", "EUR")
write.csv(as.matrix(rbind(z1, z2)), "ems.csv")

z1 <- extract_bets_on_date(browser, "ejs", "2019-1-11", "EUR")
write.csv(as.matrix(z1), "ejs.csv")

z1 <- extract_bets_on_date(browser, "uspbls", "2019-1-12", "USD")
z2 <- extract_bets_on_date(browser, "uspbls", "2019-1-16", "USD")
write.csv(as.matrix(rbind(z1, z2)), "pbs.csv")

z1 <- extract_bets_on_date(browser, "mmls", "2019-1-11", "USD")
z2 <- extract_bets_on_date(browser, "mmls", "2019-1-15", "USD")
write.csv(as.matrix(rbind(z1, z2)), "mms.csv")

z1 <- extract_bets_on_date(browser, "irls", "2019-1-12", "EUR")
z2 <- extract_bets_on_date(browser, "irls", "2019-1-16", "EUR")
write.csv(as.matrix(rbind(z1, z2)), "irls.csv")

z1 <- extract_bets_on_date(browser, "ukls", "2019-1-12", "GBP")
z2 <- extract_bets_on_date(browser, "ukls", "2019-1-16", "GBP")
write.csv(as.matrix(rbind(z1, z2)), "ukls.csv")
