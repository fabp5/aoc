# day 6: boat race --------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_csv("2023/06/input.txt",
                  col_names = "x")

races <- input %>%
  separate_wider_delim(x, regex(":\\s+"), names = c("name", "value")) %>%
  separate_wider_delim(value, regex("\\s+"), names_sep = "_") %>%
  pivot_longer(!name, names_to = "num", values_to = "value") %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(-num) %>%
  mutate(across(everything(), as.numeric)) %>%
  rename(time = Time,
         record = Distance)

races %>%
  mutate(distances = map(time, ~ seq(0,.x)*seq(.x,0))) %>%
  unnest(distances) %>%
  mutate(beat_record = distances > record) %>%
  filter(beat_record) %>%
  count(time) %>%
  pull(n) %>%
  prod()

# part 2 ------------------------------------------------------------------
time <- as.numeric(paste0(races$time,collapse=""))
record <- as.numeric(paste0(races$record,collapse=""))
sum(seq_len(time) * (time - seq_len(time)) > record)