
# libraries ---------------------------------------------------------------

library(tidyverse)


# data --------------------------------------------------------------------

bits <- readLines("data/day3.txt")

# part one ----------------------------------------------------------------

values <- bits %>%
  as.character() %>%
  str_split(pattern = "") %>%
  unlist() %>%
  matrix(nrow = 1000, byrow = T) %>%
  data.frame() %>%
  mutate_if(is.character, as.numeric) %>%
  summarise(across(X1:X12, mean)) %>%
  pivot_longer(cols = X1:X12, names_to = "column", values_to = "mean") %>%
  mutate(
    most_common_value =
      case_when(
        mean > 0.5 ~ 1,
        mean < 0.5 ~ 0
      ),
    least_common_value =
      case_when(
        mean < 0.5 ~ 1,
        mean > 0.5 ~ 0
      )
  )

gamma <-
  values %>%
  pull(most_common_value) %>%
  paste0(collapse = "") %>%
  strtoi(base = 2)

epsilon <-
  values %>%
  pull(least_common_value) %>%
  paste0(collapse = "") %>%
  strtoi(base = 2)

gamma * epsilon

# part two ----------------------------------------------------------------

# start with the full list of binary numbers from your diagnostic report and 
# consider just the first bit of those numbers.

# To find oxygen generator rating, determine the most common value (0 or 1) 
# in the current bit position, and keep only numbers with that bit in that 
# position. If 0 and 1 are equally common, keep values with a 1 in the 
# position being considered.

# To find CO2 scrubber rating, determine the least common value (0 or 1) 
# in the current bit position, and keep only numbers with that bit in that
# position. If 0 and 1 are equally common, keep values with a 0 in the 
# position being considered.

# already have some of this!
head(values)

# full list of binary numbers
split_binary_numbers <-
  bits %>% 
  str_split(pattern = "") %>%
  unlist() %>%
  matrix(nrow = 1000, byrow = T) %>%
  data.frame()  %>%
  add_column(bits, .before = 1) %>%
  mutate_at(vars(matches("X")), as.numeric) 

oxygen <-
split_binary_numbers %>%
  filter(X1 == floor(0.5 + mean(X1))) %>%
  filter(X2 == floor(0.5 + mean(X2))) %>%
  filter(X3 == floor(0.5 + mean(X3))) %>%
  filter(X4 == floor(0.5 + mean(X4))) %>%
  filter(X5 == floor(0.5 + mean(X5))) %>%
  filter(X6 == floor(0.5 + mean(X6))) %>%
  filter(X7 == floor(0.5 + mean(X7))) %>%
  filter(X8 == floor(0.5 + mean(X8))) %>%
  filter(X9 == floor(0.5 + mean(X9))) %>%
  filter(X10 == floor(0.5 + mean(X10))) %>%
  filter(X11 == floor(0.5 + mean(X11))) %>%
  filter(X12 == floor(0.5 + mean(X12)))

co2 <- 
split_binary_numbers %>%
  filter(X1 == round(1-mean(X1))) %>%
  filter(X2 == round(1-mean(X2))) %>%
  filter(X3 == round(1-mean(X3))) %>%
  filter(X4 == round(1-mean(X4))) %>%
  filter(X5 == round(1-mean(X5))) %>%
  filter(X6 == round(1-mean(X6))) %>%
  filter(X7 == round(1-mean(X7))) %>%
  filter(X8 == round(1-mean(X8))) 

strtoi(oxygen$bits, base = 2) * strtoi(co2$bits, base = 2) 
