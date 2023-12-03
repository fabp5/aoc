# day 3 -------------------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_csv("2023/03/input.txt", 
                  col_names = "x")

input_width <- nchar(input$x[1])

# extract any digits
schematic <- input %>%
  mutate(lin = row_number(),
         num = str_extract_all(x, "\\d+")) %>%
  unnest(cols = c(num)) %>%
  mutate(len_num = nchar(num)) %>%
  mutate(start_end_pos = str_locate_all(x, paste0("(?<=^|[^0-9])",num,"(?=$|[^0-9])")),
         start_pos = map(start_end_pos, ~ .x[,1])) %>%
  unnest(start_pos) %>%
  distinct() %>%
  mutate(end_pos = start_pos + len_num - 1,
         start_pos_near = ifelse(start_pos == 1, 1, start_pos - 1),
         end_pos_near = ifelse(end_pos == input_width, input_width, end_pos + 1)) %>%
  mutate(all_pos = map2(start_pos_near, end_pos_near, ~ seq(.x, .y))) %>%
  select(-c(x, start_end_pos, start_pos_near, end_pos_near)) %>%
  unnest(all_pos) %>%
  rowwise() %>%
  mutate(lin_near = list(c(lin - 1, lin, lin + 1))) %>%
  ungroup() %>%
  unnest(lin_near) %>%
  mutate(coord = paste0(lin_near,"|", all_pos)) %>%
  # create an id for each number
  group_by(lin, num, start_pos) %>%
  mutate(number_id = cur_group_id()) %>%
  ungroup()

# get the coords of the symbols
symbol_positions <- input %>% 
  mutate(lin = row_number(),
         sym = str_locate_all(x, "[^0-9|\\.]"),
         sym_pos = map(sym, ~.x[,1])) %>%
  unnest(sym_pos) %>%
  select(-c(x, sym)) %>%
  mutate(coord = paste0(lin, "|", sym_pos))

# which numbers overlap with symbols?
schematic %>%
  select(num, coord, number_id) %>%
  distinct() %>%
  filter(coord %in% symbol_positions$coord) %>%
  select(num, number_id) %>%
  distinct() %>%
  pull(num) %>%
  as.numeric() %>%
  sum()

# part 2 ------------------------------------------------------------------
# get positions of stars
star_positions <- input %>% 
  mutate(lin = row_number(),
         sym = str_locate_all(x, "\\*"),
         sym_pos = map(sym, ~.x[,1])) %>%
  unnest(sym_pos) %>%
  select(-c(x, sym)) %>%
  mutate(coord = paste0(lin, "|", sym_pos))

# get number rows that are adjacent to a star
schematic %>%
  select(num, coord, number_id) %>%
  distinct() %>%
  filter(coord %in% star_positions$coord) %>%
  # get number of adjacent numbers
  group_by(coord) %>%
  mutate(count_adj_nums = n()) %>%
  filter(count_adj_nums == 2) %>%
  mutate(ratio = prod(as.numeric(num))) %>%
  ungroup() %>%
  select(coord, ratio) %>%
  distinct() %>%
  pull(ratio) %>%
  sum()
