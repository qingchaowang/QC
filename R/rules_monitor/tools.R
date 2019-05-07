source("~/GitHub/R/tools/tools.R")



on_rule_error <- function(game_name) {
  
  function(e) {
    
    print(e)
    send_email(to = "georgios.sermaidis@mylotto24.co.uk", subject = paste0("Could not check rules for ", game_name), body = "", attachment = "run.Rout")
    file.remove("run.Rout")  
  }
}

camelot_check_rule_change <- function(rules_page, current_edition, game_name) {
  
  library(magrittr)
  
  my_url <- url(rules_page)
  lines <- readLines(my_url)
  index <- grep("edition", ignore.case = T, lines)
  today_edition <- lines[index] %>% sub(".*<p>(.*)</p>", "\\1", .)
  if(today_edition != current_edition) send_email("georgios.sermaidis@mylotto24.co.uk", paste0("Potential rule change for ", game_name), "")
  close(my_url)
  file.remove("run.Rout")
}

ithuba_check_rule_change <- function(browser, rules_page, game_name) {
  
  library(magrittr)
  
  browser$navigate(rules_page)
  button <- browser$findElement("xpath", "//button[text()='Rules and regulations']")
  button$clickElement()
  handles <- browser$getWindowHandles()
  rules_tab <- handles %>% sapply(function(h){h != browser$getCurrentWindowHandle()}) %>% handles[.]
  browser$switchToWindow(rules_tab[[1]])
  download.file(browser$getCurrentUrl()[[1]], "today_rules.pdf")
  today_md5 <- tools::md5sum("today_rules.pdf")
  ml24_md5 <- tools::md5sum("ml24_rules.pdf")
  if(today_md5 != ml24_md5) send_email("georgios.sermaidis@mylotto24.co.uk", paste0("Potential rule change for ", game_name), "")  
  file.remove("today_rules.pdf")
  file.remove("run.Rout")
}

pdf_rule_change <- function(browser, rules_page, game_name, find_element, extract_from_element = function(e)
{
  e$getElementAttribute("href")[[1]]
})
{
  library(RSelenium)
  library(magrittr)
  
  browser$navigate(rules_page)
  pdf_link <- find_element(browser) %>% extract_from_element()
  download.file(pdf_link, "today_rules.pdf")
  today_md5 <- tools::md5sum("today_rules.pdf")
  ml24_md5 <- tools::md5sum("ml24_rules.pdf")
  if(today_md5 != ml24_md5) send_email("georgios.sermaidis@mylotto24.co.uk", paste0("Potential rule change for ", game_name), "")
  file.remove("today_rules.pdf")
  if(file.exists("run.Rout")) file.remove("run.Rout") 
}


hessen_lotto_rules_page <- "https://www.lotto-hessen.de/lotto6aus49/teilnahmebedingungen?gbn=5"
hessen_eurojackpot_rules_page <- "https://www.lotto-hessen.de/eurojackpot/teilnahmebedingungen?gbn=5"
hessen_keno_rules_page <- "https://www.lotto-hessen.de/keno/teilnahmebedingungen?gbn=5"
lottery_ie_rules_page <- "https://www.lottery.ie/games/lotto"
bonoloto_rules_page <- "http://www.selae.es/es/web-corporativa/normativa/normativa-de-los-juegos/bonoloto/.corporativa"
el_gordo_rules_page <- "http://www.selae.es/es/web-corporativa/normativa/normativa-de-los-juegos/el-gordo-de-la-primitiva"

