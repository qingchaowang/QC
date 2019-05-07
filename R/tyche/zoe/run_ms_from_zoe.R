source("ms_from_zoe.R")

fx_table <- read_fx("~/Downloads/eurofxref-hist.csv")
process_sheet("~/Desktop/source.xlsx", fx_table, "/Users/georgios.sermaidis/Desktop/todel.xlsx")

