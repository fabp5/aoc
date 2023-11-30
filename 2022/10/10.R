# day 10: cathode-ray tube

require(tidyverse)

# inputs
example_small <- tibble(op = c("noop", "addx", "addx"),
              add = c(NA, 3, -5))

example_input <- read_table("10/example_input.txt", 
                            col_names = c("op", "add"))

input <- read_table("10/input.txt", 
                    col_names = c("op", "add"))

inst <- input

# code
inst <- inst %>%
  mutate(cycles = ifelse(op == "noop", 1, 2),
         register = numeric(nrow(inst)),
         inst_num = 1:nrow(inst)) %>%
  relocate(inst_num) %>%
  mutate(add = replace_na(add, 0)) %>%
  mutate(register = cumsum(add)+1,
         register = lag(register),
         register = replace_na(register, 1)) %>%
  uncount(cycles) %>%
  rownames_to_column("cycle") %>%
  mutate(cycle = as.numeric(cycle),
         signal_strength = cycle * register)

cycles_to_look_at <- seq(from = 20, to = nrow(inst), by = 40)

inst %>%
  slice(cycles_to_look_at) %>%
  pull(signal_strength) %>%
  sum()

# -------- part 2 --------

inst2 <- inst %>%
  select(cycle, op, register) %>%
  mutate(pixel = cycle - 1,
         pixel_h = pixel %% 40,
         pixel_v = pixel %/% 40,
         sprite_visible = abs(register - pixel_h) <= 1,
         sprite_char = ifelse(sprite_visible, "#", " "))

inst2 %>%
  group_by(pixel_v) %>%
  summarise(paste0(sprite_char, collapse = ""))

# or in ggplot
inst2 %>%
  filter(sprite_visible) %>%
  ggplot() +
  geom_point(aes(x = pixel_h, y = pixel_v), shape = 15, size = 6) +
  theme_void() +
  scale_y_reverse() +
  coord_fixed()
