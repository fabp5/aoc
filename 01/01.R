# AOC day 1
require(tidyverse)

# read input
input <- read_csv("01/input.txt",
                  col_names = "x",
                  skip_empty_rows = FALSE)

# str_split method --------
# collapse into string, split into individual groups, split elements of groups, concert to numeric, sum groups
cal_groups <- paste(input$x,collapse = ",") %>%
  str_split(., ",NA,") %>%
  .[[1]] %>%
  str_split(., ",") %>%
  lapply(., as.numeric) %>%
  sapply(., sum)

# get top elf's calories
sort(cal_groups, decreasing = TRUE)[1]

# get top 3 elves' calories
sort(cal_groups, decreasing = TRUE)[1:3] %>% sum()

# or: cumsum method --------
# top elf's calories
input %>%
  mutate(group = cumsum(is.na(x))) %>%
  group_by(group) %>%
  summarise(cal=sum(x, na.rm=TRUE)) %>%
  pull(cal) %>%
  max()

# get top 3 elves' calories
input %>%
  mutate(group = cumsum(is.na(x))) %>%
  group_by(group) %>%
  summarise(cal=sum(x, na.rm=TRUE)) %>%
  arrange(desc(cal)) %>%
  head(3) %>%
  pull(cal) %>% 
  sum()