read_ms_entry <- function(file, sheet_name, currency_map, name_map) {
  
  x <- readxl::read_excel(file, sheet_name, col_names = F)
  colnames(x) <- c("partner", "on_risk", "off_risk", "total", "date")
  x$partner <- sub("\\s*", "", x$partner)
  x$partner <- sapply(x$partner, function(p){subset(name_map, zoe_name == p)$ms_name})
  x$date <- as.Date(x$date)
  x <- as.data.frame(x)
  x <- split(x, x$date)
  
  game_currency <- subset(currency_map, game == sheet_name)$currency
  
  attr(x, "currency") <- game_currency
  x
}

read_fx <- function(file = '~/Downloads/eurofxref-hist.csv') {
  
  x <- read.csv(file)
  x[[1]] <- as.Date(x[[1]])
  
  # deal with "N/A" to suppress warnings
  # we will exclude the date and add it in the end
  y <- lapply(x[-1], function(e){
    e <- as.character(e)
    e[e == "N/A"] <- NA
    as.numeric(e)
  })
  
  do.call(data.frame, c(x[1], y))
}

get_fx <- function(fx_table, target_date, source_currency, target_currency) {
  
  k <- which(fx_table$Date <= target_date)[1]
  out <- ifelse(target_currency == "EUR", 1, fx_table[k, target_currency])/ifelse(source_currency == "EUR", 1, fx_table[k, source_currency])
  out
}

make_entry <- function(r, fx_table, game_currency) {
  
  draw_date <- r$date
  fx_rate <- get_fx(fx_table, draw_date - 1, game_currency, "USD")
  data.frame(date = format(draw_date, "%d/%m/%Y"),
             partner = r$partner, off_risk = c("N", "Y"), fx_rate = fx_rate, bets = c(r$on_risk, r$off_risk))
}

process_unit <- function(x, fx_table, game_currency) {
  
  y <- list()
  for(i in 1:nrow(x)) y[[i]] <- make_entry(x[i,], fx_table, game_currency)
  y <- do.call(rbind, y)
  if(mean(subset(y, off_risk == "Y")$bets == 0) == 1) y <- subset(y, off_risk == "N")
  rownames(y) <- NULL
  y
}

process_game <- function(x, fx_table) {
  
  currency <- attr(x, "currency")
  y <- lapply(x, process_unit, fx_table, currency)
  z <- do.call(rbind, y)
  rownames(z) <- NULL
  z
}

process_sheet <- function(file, fx_table, out_path) {
  
  require(xlsx)
  
  if(file.exists(out_path)) file.remove(out_path)
  
  sheet_names <- readxl::excel_sheets(file)
  nsheets <- length(sheet_names)
  sheet_names <- sheet_names[-c(nsheets-1, nsheets)]
  
  currency_map <- readxl::read_excel(file, "currencies", col_names = T)
  name_map <- readxl::read_excel(file, "names", col_names = T)
  
  lapply(sheet_names, function(s) {
    x <- read_ms_entry(file, s, currency_map, name_map)
    y <- process_game(x, fx_table)
    write.xlsx(y, out_path, s, row.names = F, append = T)
  })
}


