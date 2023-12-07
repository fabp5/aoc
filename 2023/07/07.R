# day 7: camel cards ------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_table("2023/07/input.txt", 
                    col_names = c("hand", "bid"))

# convert letters
input <- input %>%
  mutate(hand = str_replace_all(hand, "A", "E"),
         hand = str_replace_all(hand, "K", "D"),
         hand = str_replace_all(hand, "Q", "C"),
         hand = str_replace_all(hand, "J", "B"),
         hand = str_replace_all(hand, "T", "A"))

classify_hand <- function(hand) {
  
  hand_s <- str_split_1(as.character(hand), "") %>%
    as_tibble() %>%
    count(value) %>%
    arrange(desc(n)) %>%
    pull(n) %>%
    paste0(collapse = "")
  
  hand_type <- case_when(hand_s == "5"  ~ "five_kind",
                         hand_s == "41" ~ "four_kind",
                         hand_s == "32"  ~ "full_house",
                         hand_s == "311" ~ "three_kind",
                         hand_s == "221" ~ "two_pair",
                         hand_s == "2111"  ~ "one_pair",
                         hand_s == "11111" ~ "high_card")
  
  hand_type_value <- case_when(hand_s == "5" ~ 7,
                               hand_s == "41" ~  6,
                               hand_s == "32"  ~ 5,
                               hand_s == "311" ~  4,
                               hand_s == "221" ~  3,
                               hand_s == "2111"  ~ 2,
                               hand_s == "11111" ~ 1)
  
  final_value <- paste0(hand_type_value, hand)
  return(final_value)
}

cards <- input %>%
  mutate(hand_value = map_vec(hand, classify_hand),
         hand_value_dec = as.integer(as.hexmode(hand_value))) %>%
  arrange(hand_value_dec) %>%
  rownames_to_column() %>%
  rename(rank = rowname) %>%
  mutate(rank = as.numeric(rank),
         winnings = rank * bid)

cards %>%
  pull(winnings) %>%
  sum()

# part 2 ------------------------------------------------------------------

input <- read_table("2023/07/input.txt", 
                    col_names = c("hand", "bid"))

# convert letters
input <- input %>%
  mutate(hand = str_replace_all(hand, "A", "E"),
         hand = str_replace_all(hand, "K", "D"),
         hand = str_replace_all(hand, "Q", "C"),
         hand = str_replace_all(hand, "T", "A"),
         # change j to 1 (now lowest value)
         hand = str_replace_all(hand, "J", "1"))

classify_hand_joker <- function(hand) {
  
  if(hand == "11111"){return("711111")}
  
  hand_t <- str_split_1(as.character(hand), "") %>%
    as_tibble() %>%
    count(value) %>%
    arrange(desc(n), desc(value))
  
  grps <- hand_t %>%
    filter(value != 1) %>%
    pull(n)
  
  if((hand_t %>% filter(value ==1) %>% nrow() > 0)){
  num_jokers <- hand_t %>%
    filter(value == 1) %>%
    pull(n)
  
  grps[1] <- grps[1] + num_jokers
  }
    
  hand_s <- paste0(grps, collapse = "")
  
  hand_type_value <- case_when(hand_s == "5"     ~ 7,
                               hand_s == "41"    ~ 6,
                               hand_s == "32"    ~ 5,
                               hand_s == "311"   ~ 4,
                               hand_s == "221"   ~ 3,
                               hand_s == "2111"  ~ 2,
                               hand_s == "11111" ~ 1)
  
  final_value <- paste0(hand_type_value, hand)
  return(final_value)
}

cards2 <- input %>%
  mutate(hand_value = map_vec(hand, classify_hand_joker),
         hand_value_dec = as.integer(as.hexmode(hand_value))) %>%
  arrange(hand_value_dec) %>%
  rownames_to_column() %>%
  rename(rank = rowname) %>%
  mutate(rank = as.numeric(rank),
         winnings = rank * bid)

cards2 %>%
  pull(winnings) %>%
  sum()
