round_down <- function(cents) {
  
  function(x) {
    floor(x*cents)/cents
  }
}

find_first_winner <- function(winners, from, to) {
  
  offset <- from - 1
  k <- min(which(winners[from:to] > 0)) + offset
  k
}

must_go <- function(jpad, winners, from, to) {
  
  k <- find_first_winner(winners, from, to)
  
  if(k > from) {
    jpad[k] <- sum(jpad[from:k])
    jpad[from:(k-1)] <- 0
  }
  
  jpad
}

pool_seniority <- function(jpad, winners, prize, from, to, round_f) {
  
  tags <- from:to
  
  for(i in 1:(length(tags)-1)) {
    
    fund_i <- jpad[tags[i]]
    winners_i <- winners[tags[i]]
    
    if(winners_i) {
      
      for(j in (i+1):length(tags)) {
        
        fund_j <- jpad[tags[j]]
        prize_j <- prize[tags[j]]
        winners_j <- winners[tags[j]]
        
        if(winners_j) {
          
          fund_i <- fund_i + fund_j
          winners_i <- winners_i + winners_j
          
          if(prize[tags[i]] < prize_j) {
            
            for(k in i:j) {
              if(winners[tags[k]]) prize[tags[k]] <- round_f(fund_i/winners_i)
            }            
          }
        }
      }
    }
  }
  prize
}

# creates a named list of draw attributes from a file
# all names in the file which are suffixed by an "name_digits" will become part of the "name" attribute
# for instance, "winners_1", "winners_2", will create an vector in the list "winners" with two elements
deserialise_draw <- function(draw) {
  
  draw_names <- names(draw)
  out <- list()
  while(length(draw_names)) {
    name <- draw_names[1]
    name <- sub("(.*)_\\d+", "\\1", name)
    indices <- grep(paste(name, "_\\d+", sep = ""), draw_names)
    if(length(indices) == 0) {
      out[[name]] <- draw[[name]]
      draw_names <- draw_names[-which(draw_names == name)]
    }
    else {
      out[[name]] <- c(sapply(draw_names[indices], getElement, object = draw), use.names = F)
      draw_names <- draw_names[-indices]
    }
  }
  
  out$date <- as.Date(out$date, "%Y-%b-%d")
  out
}

serialise_draw <- function(draw) {
  
  flat_draw <- data.frame(remove = NA)
  
  for(el in draw) {
    if(length(el) == 1) flat_draw <- cbind(flat_draw, el)
    else flat_draw <- cbind(flat_draw, t(el))
  }
  
  flat_draw <- flat_draw[-1]
  
  names(flat_draw) <- do.call(c, mapply(function(x,y){
    if(y == 1) x
    else paste(x, 1:y, sep = "_")
  }, as.list(names(draw)), lapply(draw, length)))
  
  flat_draw
}

historical_data <- function(file_name) {
  
  my_data <- read.csv(file_name, header = T)
  latest_draw <- deserialise_draw(my_data[nrow(my_data),])
  
  call <- function(new_data) {
    
    names_data <- names(new_data)
    date <- format(new_data$date, "%Y-%b-%d")
    entries <- new_data$entries
    winners <- c(sapply(names_data[grep("winners_", names_data)], getElement, object = new_data), use.names = F)
    
    latest_draw <<- next_draw(latest_draw, date, entries, winners)
    serialise_draw(latest_draw)
  }
  
  append <- function(serialized_data) {
    
    s_data <- do.call(rbind, serialized_data)
    my_data <- rbind(my_data, s_data)
    write.csv(my_data, file = file_name, row.names = F, quote = F)
  }
  
  list(call = call, append = append, most_recent_date = latest_draw$date)
}