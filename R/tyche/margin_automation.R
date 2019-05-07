exr <- function(date, source_currency, target_currency) {
  
  fx_table <- read_fx(file = "eurofxref-hist.csv")
  get_fx(fx_table, date, source_currency, target_currency)
}

keno_cost <- function(prize, match, select)
{
  probs <- dhyper(x = match, m = 20, n = 50, k = select)
  sum(probs*prize)
}

keno2_cost <- function() {
  keno_cost(prize = 6, match = 2, select = 2)
}

keno3_cost <- function() {
  keno_cost(prize = c(16,1), match = 3:2, select = 3)
}

keno4_cost <- function() {
  keno_cost(prize = c(22,2,1), match = 4:2, select = 4)
}

keno5_cost <- function() {
  keno_cost(prize = c(100,7,2), match = 5:3, select = 5)
}

keno6_cost <- function() {
  keno_cost(prize = c(500,15,2,1), match = 6:3, select = 6)
}

keno7_cost <- function() {
  keno_cost(prize = c(1000,100,12,1), match = 7:4, select = 7)
}

keno8_cost <- function() {
  keno_cost(prize = c(10000,100,15,2,1,1), match = c(8:4,0), select = 8)
}

keno9_cost <- function() {
  keno_cost(prize = c(50000,1000,20,5,2,2), match = c(9:5,0), select = 9)
}

keno10_cost <- function() {
  keno_cost(prize = c(100000,1000,100,15,5,2,2), match = c(10:5,0), select = 10)
}

convert_to <- function(money, target_currency, date) {
  
  source_currency <- class(money)[1]
  rate <- exr(date, source_currency, target_currency)
  as_currency(money@amount*rate, target_currency)
}

sum_costs <- function(costs_list, output_currency, date) {
  
  total_cost <- as_currency(0, output_currency)
  for(cost in costs_list) total_cost <- total_cost + convert_to(cost, output_currency, date)
  total_cost
}

margins_block <- function(jackpot, lower_costs, bet_price, odds, tower, jackpot_multiplier = 1, ticket_buying = NULL, date = Sys.Date()) {
  
  if(!is.list(lower_costs)) lower_costs <- list(lower_costs)
  n_jackpot <- length(jackpot@amount)
  is_hedged <- !is.null(ticket_buying)
  hedged_type <- FALSE
  if(is_hedged) hedged_type <- ticket_buying$type
  needs_ils <- !is_hedged | (hedged_type == "partial")
  
  jackpot_currency <- class(jackpot)[1]
  output_currency <- class(bet_price)[1]
  total_cost <- 0.0216*bet_price
  
  if(is_hedged) {
    total_cost <- total_cost + convert_to(ticket_buying$cost, output_currency, date)
  }
  else {
    total_cost <- total_cost + sum_costs(lower_costs, output_currency, date)
  }
  
  if(needs_ils) {
    
    usd_jackpot <- convert_to(jackpot, "USD", date)*jackpot_multiplier
    
    ils_cost <- marginal_cost(usd_jackpot@amount, odds, tower)
    retention_cost <- as_currency(ils_cost[, 2], "USD")
    retention_cost <- convert_to(retention_cost, output_currency, date)
    ils_cost <- as_currency(ils_cost[,ncol(ils_cost)], "USD")
    ils_cost <- convert_to(ils_cost, output_currency, date)
    total_cost <- total_cost + ils_cost
  }
  
  if(needs_ils) {
    jackpot_col <- jackpot@amount
    retention_col <- retention_cost@amount
  }
  else{
    jackpot_col <- "All"
    retention_col <- 0
  } 
  
  out <- data.frame(jackpot = jackpot_col, retention_cost = retention_col, total_cost = total_cost@amount, price = bet_price@amount, margin = 1 - total_cost/bet_price)
  out_names <- c(paste0("Jackpot", ' (', jackpot_currency , ')'),
                 paste0("Retention Cost", ' (', output_currency , ')'),
                 paste0("Total Cost", ' (', output_currency, ')'), 
                 paste0("Price", ' (', output_currency, ')'), 
                 'Margin')
  
  colnames(out) <- out_names
  attr(out, "type") <- export_margins_block
  out
}

scalar_block <- function(lower_costs, bet_price, date = Sys.Date()) {
  
  if(!is.list(lower_costs)) lower_costs <- list(lower_costs)
  output_currency <- class(bet_price)[1]
  
  values <- c(bet_price@amount, sum_costs(lower_costs, output_currency, date)@amount,
              0.0216*bet_price@amount, use.names = F)
  
  out_names <- c(paste0("Bet price", ' (', output_currency, ')'), 
                 paste0("Lower classes", ' (', output_currency, ')'),
                 paste0("Other expenses", ' (', output_currency, ')'))
  
  out <- matrix(values)
  rownames(out) <- out_names
  attr(out, "type") <- export_scalar_block
  out
}

fixed_odds_block <- function(class_costs, bet_price, date = Sys.Date()) {
  
  output_currency <- class(bet_price)[1]
  class_costs <- convert_to(class_costs, output_currency, date)
  out <- c(bet_price@amount, class_costs@amount,  0.0216*bet_price@amount, 1 - (class_costs + 0.0216*bet_price)/bet_price)
  names(out) <- c(paste0("Bet price (", output_currency, ")"),
                  paste0("Classes Cost (", output_currency, ")"),
                  paste0("Other expenses (", output_currency, ")"),
                  "Margin")
  out <- as.matrix(out)
  attr(out, "type") <- export_fixed_odds_block
  out
}

uncovered_block <- function(jackpot, lower_costs, bet_price, odds) {
  
  jackpot_currency <- class(jackpot)[1]
  output_currency <- class(bet_price)[1]
  total_cost <- jackpot/odds + lower_costs + 0.0216*bet_price
  out <- data.frame(jackpot = jackpot@amount, total_cost = total_cost@amount, price = bet_price@amount, margin = 1 - total_cost/bet_price)
  out_names <- c(paste0("Jackpot", ' (', jackpot_currency , ')'),
                 paste0("Total cost", ' (', output_currency , ')'),
                 paste0("Price", ' (', output_currency, ')'),
                 'Margin')
  
  colnames(out) <- out_names
  attr(out, "type") <- export_uncovered_block
  out
}

margins_scalar_block <- function(jackpot, lower_costs, bet_price, odds, tower, jackpot_multiplier = 1, ticket_buying = NULL, date = Sys.Date()){
  
  list(
    margins_block(jackpot, lower_costs, bet_price, odds, tower, jackpot_multiplier, ticket_buying, date),
    scalar_block(lower_costs, bet_price, date)
  )
}

uncovered_scalar_block <- function(jackpot, lower_costs, bet_price, odds, date = Sys.Date()){
  
  list(
    uncovered_block(jackpot, lower_costs, bet_price, odds),
    scalar_block(lower_costs, bet_price, date)
  )
}

export_margins_block <- function(data, sheet, wb) {
  
  setColumnWidth(sheet = sheet, colIndex = 1:5, colWidth = 20)
  cs1 <- CellStyle(wb, DataFormat("#,##0.00"))
  cs2 <- CellStyle(wb, DataFormat("0.00"))
  cs3 <- CellStyle(wb, DataFormat("#0.00%"))
  addDataFrame(data, sheet = sheet, row.names = F, startColumn = 1, colStyle = list(`1`=cs1, `2`=cs2, `3`=cs2, `4`=cs2, `5`=cs3))
}

export_uncovered_block <- function(data, sheet, wb) {
  
  setColumnWidth(sheet = sheet, colIndex = 1:4, colWidth = 20)
  cs1 <- CellStyle(wb, DataFormat("#,##0.00"))
  cs2 <- CellStyle(wb, DataFormat("0.00"))
  cs3 <- CellStyle(wb, DataFormat("#0.00%"))
  addDataFrame(data, sheet = sheet, row.names = F, startColumn = 1, colStyle = list(`1`=cs1, `2`=cs2, `3`=cs2, `4`=cs3))
}

export_scalar_block <- function(data, sheet, wb) {
  
  setColumnWidth(sheet = sheet, colIndex = 7:8, colWidth = 20)
  cs2 <- CellStyle(wb, DataFormat("0.00"))
  addDataFrame(data, sheet = sheet, row.names = T, col.names = F, startColumn = 7, colStyle = list(`1`=cs2))
}

export_fixed_odds_block <- function(data, sheet, wb) {
  setColumnWidth(sheet = sheet, colIndex = 1:2, colWidth = 20)
  cs2 <- CellStyle(wb, DataFormat("0.00"))
  cs3 <- CellStyle(wb, DataFormat("#0.00%"))
  addDataFrame(data, sheet = sheet, row.names = T, col.names = F, startColumn = 1, colStyle = list(`1`=cs2))
  setCellStyle(cell = getCells(row = getRows(sheet = sheet, rowIndex = 4), colIndex = 2, simplify = T)[[1]], cellStyle = cs3)
}

export_unit <- function(data_list, sheet_name, wb) {
  if(!is.null(attr(data_list, 'type'))) data_list <- list(data_list)
  sheet <- createSheet(wb, sheet_name)
  for(my_data in data_list) {
    export_f <- attr(my_data, "type")
    export_f(my_data, sheet, wb);
  }
}

export_tenant <- function(data_list, export_filename) {
  wb <- createWorkbook()
  mapply(export_unit, data_list, names(data_list), MoreArgs = list(wb)) 
  saveWorkbook(wb, export_filename)
}
