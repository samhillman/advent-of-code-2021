
# libraries ---------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

dat <- readLines("data/day4.txt")


# part one ----------------------------------------------------------------

values <- str_split(dat[1], ",") %>% unlist()

length(dat)

boards <- dat[2:601] %>% trimws() %>% as.data.frame()

boards_columns <- boards %>%
  separate(col = 1, into = c("a", "b", "c", "d", "e")) 

#rows_that_match <-
boards_columns %>%
  mutate(row_number()) %>%
  filter(complete.cases(.)) %>%
  filter(a %in% values &
         b %in% values &
         c %in% values &
         d %in% values &
         e %in% values
         )

  columns_that_match 

boards_columns %>%
  mutate(row_number()) %>%
  filter(complete.cases(.)) %>% 
  filter(a %in% values) %>%
  filter(         b %in% values) %>%
filter(       c %in% values) %>%
                filter(       d %in% values) %>%
                                filter(       e %in% values
  )




  boards_list[1]
boards_list <- boards_columns %>%
 filter(complete.cases(.)) %>%
  split(rep(1:100, by = 5)) %>%
  nest()

lapply(boards_list,   filter(a %in% values,
                             b %in% values,
                             c %in% values,
                             d %in% values,
                             e %in% values
) )

column_function <- function(dataframe) {
  for (i in 1:nrow(dataframe)) {
    
  }
}