# libraries ---------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

depths <- read.delim("data/day1part1.txt", 
                     header = FALSE,
                     sep = ""
                     )

# part one ----------------------------------------------------------------

# count the number of times a depth measurement increases from
# the previous measurement.

depths <- depths %>% select(depth = V1)

sum(diff(depths$depth) > 0)


# part two ----------------------------------------------------------------

# Consider sums of a three-measurement sliding window. How many 
# sums are larger than the previous sum?

sum(diff(depths$depth, lag = 3) > 0)
