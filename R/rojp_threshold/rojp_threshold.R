find_rojp_cases <- function(file, tower_capacity) {

  my_data <- read.table(file)
  rojp <- my_data[-nrow(my_data),1]
  jpad <- my_data[-1,2]
  
  k <- (rojp < tower_capacity) & (jpad > tower_capacity)
  rojp[k]
}


pb1 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/pb_rollover_tracker.txt", 153e6/0.372)
mm1 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/mm_rollover_tracker.txt", 153e6/0.372)
em1 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/em_rollover_tracker.txt", 153e6/1.24)

pb2 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/pb_rollover_tracker.txt", 203e6/0.372)
mm2 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/mm_rollover_tracker.txt", 203e6/0.372)
em2 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/em_rollover_tracker.txt", 203e6/1.24)

pb3 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/pb_rollover_tracker.txt", 228e6/0.372)
mm3 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/mm_rollover_tracker.txt", 228e6/0.372)
em3 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/em_rollover_tracker.txt", 228e6/1.24)

pb4 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/pb_rollover_tracker.txt", 253e6/0.372)
mm4 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/mm_rollover_tracker.txt", 253e6/0.372)
em4 <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output/em_rollover_tracker.txt", 253e6/1.24)


sep <- find_rojp_cases("~/GitHub/build-Tyche-tests-Desktop_Qt_5_10_0_clang_64bit-Release/output_50_test/sep_rollover_tracker.txt", 200e6)
