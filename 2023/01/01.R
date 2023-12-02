# day 1 -------------------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_csv("2023/01/input.txt", 
                          col_names = "x")

digits <- input %>%
  pull(x) %>%
  str_extract_all(., "[0-9]")

paste0(map(digits, ~ .x[1]),
       map(digits, ~ .x[length(.x)])) %>%
  as.numeric() %>%
  sum()

# part 2 ------------------------------------------------------------------
# make digit/word map
num_map <- tibble(digit = as.character(seq(1:9)),
              word = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))

# for each line, which number is first?
word_positions <- list()
digit_positions <- list()
min_pos_total <- list()
max_pos_total <- list()

# loop over number 1-9
for (d in 1:9){
  
  word_positions[[d]] <- map(input$x,
                             ~ str_locate_all(.x, num_map$word[d]) %>% pluck(1) %>% .[,1])
  digit_positions[[d]] <- map(input$x,
                              ~ str_locate_all(.x, num_map$digit[d]) %>% pluck(1) %>% .[,1])
  
  # make df for min positions for this digit
  min_pos <- tibble(line = seq(1, nrow(input)),
                    word = NA,
                    digit = NA)
  max_pos <- tibble(line = seq(1, nrow(input)),
                    word = NA,
                    digit = NA)
  
  # loop through lines
  for (l in 1:nrow(input)){
    
    # get first numbers
    min_pos$word[l] <- word_positions[[d]][[l]] %>% hablar::min_()
    min_pos$digit[l] <- digit_positions[[d]][[l]] %>% hablar::min_()
    
    min_pos <- min_pos %>%
      mutate(number = pmin(word, digit, na.rm = TRUE))
    
    # get last numbers
    max_pos$word[l] <- word_positions[[d]][[l]] %>% hablar::max_()
    max_pos$digit[l] <- digit_positions[[d]][[l]] %>% hablar::max_()
    
    max_pos <- max_pos %>%
      mutate(number = pmax(word, digit, na.rm = TRUE))
  }
  
  min_pos_total[[d]] <- min_pos
  max_pos_total[[d]] <- max_pos
}

min_pos_df <- bind_rows(min_pos_total, .id = "p")
max_pos_df <- bind_rows(max_pos_total, .id = "p")
  
# for each line, get the first and last number
full_join(min_pos_df %>%
            select(-c(word, digit)) %>%
            group_by(line) %>%
            summarise(first_num = which.min(number)),
          max_pos_df %>%
            select(-c(word, digit)) %>%
            group_by(line) %>%
            summarise(last_num = which.max(number)),
          by = "line") %>%
  mutate(composite = paste0(first_num, last_num)) %>%
  pull(composite) %>%
  as.numeric() %>%
  sum()
