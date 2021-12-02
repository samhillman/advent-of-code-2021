
# libraries ---------------------------------------------------------------

library(tidyverse)

# data --------------------------------------------------------------------

direc <- read.delim("data/day2.txt",
  header = FALSE,
  sep = ""
)

# part one ----------------------------------------------------------------

# Calculate the horizontal position and depth you would have
# after following the planned course.

direc %>%
  select(direction = V1, number = V2) %>%
  group_by(direction) %>%
  summarise(total = sum(number)) %>%
  pivot_wider(names_from = direction, values_from = total) %>%
  summarise(solution = forward * (down - up))

# part two ----------------------------------------------------------------

# In addition to horizontal position and depth, you'll also need to track
# a third value, aim, which also starts at 0

# down adds to your aim, up decreases aim
# forward now also changes depth by forward * aim

direc %>%
  select(direction = V1, number = V2) %>%
  mutate(
    number = as.numeric(number),
    aim = case_when(
      direction == "down" ~ number,
      direction == "up" ~ number * -1,
      direction == "forward" ~ 0
    ),
    cumulative_aim = cumsum(aim),
    lagged_aim = lag(cumulative_aim, n = 1)
  ) %>%
  filter(direction == "forward") %>%
  mutate(move_from_aim = number * lagged_aim) %>%
  summarise(
    total_move_from_aim = sum(move_from_aim, na.rm = TRUE),
    total_forward_move = sum(number),
    solution = total_move_from_aim * total_forward_move
  )
