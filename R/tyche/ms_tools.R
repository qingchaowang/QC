export_monitoring_sheet_unit <- function(input_file_name, output_file_name, game_name, sheet_index, start_date, end_date) {
  
  require(readxl)
  
  file <- read_excel(input_file_name, sheet_index, skip = 3, col_names = T)[,c("date", "off risk", "function", "shared winners", 
                                                                               "primary winners", "ML24 jackpot (USD)", "secondary winners", "bets", "odds")]
  file[,1] <- as.Date(file[,1]$date)
  
  inc_rows <- (file[,1] >= start_date)
  if(!is.null(end_date)) inc_rows <- inc_rows & (file[,1] < end_date)
  
  file <- file[inc_rows,]
  file[,1] <- format(file[[1]], "%Y-%b-%d")
  file <- cbind(game_name, file)
  
  write.table(file, file = output_file_name, append = T, row.names = F, quote = F, col.names = F)
}

export_monitoring_sheet <- function(input_file_name, output_file_name, game_names, start_date, end_date = NULL) {
  
  if(file.exists(output_file_name)) file.remove(output_file_name)
  for(i in 1:length(game_names)) export_monitoring_sheet_unit(input_file_name, output_file_name, game_names[i], i, start_date, end_date)
}

game_names <- c("glp", "emp", "ejp", "irlp", "pbp", "mmp", "uklp")
export_monitoring_sheet("/Volumes/dfs/Hedging Management/002_Operational/020_Tyche_Monitoring/005_Monitoring/001_Weekly_Monitoring/Monitoring_Sheet_ML24_31122018.xlsx", "~/Desktop/monitoring_sheet/accounts_year.txt", game_names, "2018-1-1")

min_el_t(modelled_el = read_modelled_el(), min_rol_multiple = 0.75, rol_i = c(24.25, 14.75, 9)/100, el_factor = 1.35, date = "2018-12-31", last_date = "2018-12-31")
