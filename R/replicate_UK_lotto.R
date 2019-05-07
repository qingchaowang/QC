library(htmltab)
WN_1994 <- htmltab("https://www.lottery.co.uk/lotto/results/archive-1994", 1)
WN_1995 <- htmltab("https://www.lottery.co.uk/lotto/results/archive-1995", 1)
WN_1996 <- htmltab("https://www.lottery.co.uk/lotto/results/archive-1996", 1)

processing_data <- function(data) {
  length <- length(data[, 2])
  out <- matrix(0, nrow = length, ncol = 7)
  for (i in 1:length) {
    data_tmp <- strsplit(data[i, 2], " ")[[1]]
    data_tmp <- sapply(data_tmp, function(x) {if (x != "")
      x})
    data_tmp <- as.numeric(unlist(data_tmp))
    out[i, ] <- as.numeric(data_tmp)
  }
  out
}

processed_1994 <- processing_data(WN_1994)
processed_1995 <- processing_data(WN_1995)
processed_1996 <- processing_data(WN_1996)

data <- rbind(processed_1994, processed_1995, processed_1996[16:52, ])[,1:6]

data

N.draws <- length(data[, 1])
M <- 49
m <- 6

Occuring <- matrix(0, nrow = N.draws, ncol = M)
# Occurance <- matrix(0, nrow = N.draws, ncol = M)
# counter <- rep(0, M)
for (d in 1:N.draws) {
  for (i in 1:m) {
    for (j in 1:M) {
      if(data[d, i] == j) 
        Occuring[d, j] = 1;
    }
  }
}

# Case 1: W_1(i) = 1 for firstly drawed numbers & W_0(i) = 0 for lastly drawed numbers
Counter1 <- 0
for (j in 1:M) {
  if (Occuring[1, j] == 1)
    Counter1 = Counter1 + 1
  for (d in 1:(N.draws - 1)) {
    if (Occuring[d, j] == 1 && Occuring[d+1, j] == 1)
      Counter1 = Counter1 + 1     
  }
}
Counter1

CounterK <- rep(0, N.draws - 1)
for (k in 2:(N.draws - 1)) {
  for (j in 1:M) {
    if(Occuring[k, j] == 1 && sum(Occuring[1:(k-1), j]) == 0)
      CounterK[k - 1] = CounterK[k - 1] + 1
  #   if(Occuring[M - 1, j] == 1 && Occuring[M, j] == 0)
  #  Counter2 = Counter2 + 1
  for (d in 1:(N.draws - k)) {
    if (Occuring[d, j] == 1 && Occuring[d+k, j] == 1 && sum(Occuring[(d+1):(d+k-1), j]) == 0)
      CounterK[k - 1] = CounterK[k - 1] + 1     
    }
  }
}

CounterK
final_counter <- c(Counter1, CounterK)
final_counter <- c(final_counter[1:20], sum(final_counter[21:22]), sum(final_counter[23:24]), 
                   sum(final_counter[25:27]), sum(final_counter[28:31]), sum(final_counter[32:length(final_counter)]))
final_counter

Counter_zero <- rep(0, M)
for (d in 1:N.draws - 1) {
  for (j in 1:M) {
    if(Occuring[d, j] == 1 && sum(Occuring[(d + 1):(N.draws), j]) == 0)
      Counter_zero[j] = N.draws - d
  }
}
Counter_zero

#value to be tested again
counter_default <- c(71, 58, 65, 45, 48, 28, 41, 27, 27, 21, 16, 19, 20, 16, 13, 9, 8, 7, 7, 0, 7, 8,7, 4, 4)
sum(counter_default)

#example
# draw 1 number out of 0 and 1 for 5 times 
winning_numer <- c(1, 0, 0, 1, 0)
Occuring <- matrix(c(1, 0, 0, 1, 0, 0, 1, 1, 0, 1), nrow = 5, ncol = 2)
M <- 2
N.draws <- 5