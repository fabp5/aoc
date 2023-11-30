# day 11: monkeys

require(tidyverse)

input <- read_delim("11/input.txt",
                 delim = "|",
               trim_ws = FALSE,
               col_names = "x")

mon <- input %>%
  mutate(monkey = ifelse(!grepl("^ ", x),
                         str_extract(x,"[0-9]+"),
                         NA)) %>%
  fill(monkey) %>%
  mutate(inst_type = case_when(grepl("^  Starting items:", x) ~ "starting_items",
                               grepl("^  Operation:", x) ~ "operation",
                               grepl("^  Test:", x) ~ "divisible_by",
                               grepl("^    If true:",x) ~ "if_true",
                               grepl("^    If false:",x) ~ "if_false")) %>%
  filter(!grepl("^Monkey", x)) %>%
  pivot_wider(names_from = inst_type, values_from = x) %>% 
  mutate(across(c(divisible_by, if_true, if_false),
                ~ as.numeric(str_extract(.x, "[0-9]+"))),
         starting_items = gsub("^  Starting items: ", "", starting_items),
         starting_items_vec = str_split(starting_items, ", "),
         operation = gsub("^.+= ","", operation),
         operation = gsub("old","item", operation),
         monkey = paste0("mon",monkey)) %>%
  relocate(starting_items_vec, .after = starting_items)
         
num_monkeys <- nrow(mon)

# function to calculate worry during monkey inspection
monkey_inspect <- function(item, operation, divisible_by, if_true, if_false){
  worry <- eval(parse(text = operation))
  divided_worry <- worry %/% 3
  monkey_to <- ifelse(divided_worry %% divisible_by == 0,
                      if_true,
                      if_false)
  return(c(divided_worry, monkey_to))
}

monkey_items <- as.list(setNames(mon$starting_items_vec, mon$monkey)) %>%
  map(., as.numeric)
monkey_inspect_count <- as.list(setNames(rep(0, num_monkeys), mon$monkey))

# 20 rounds
for (k in 1:20){

  # single round of all monkeys
  for (j in 0:(num_monkeys-1)){
  
    monkey <- paste0("mon",j)
    
    if(length(monkey_items[[monkey]]) > 0) {
    # single monkey
      for (i in 1:length(monkey_items[[monkey]])){
        
        monkey_items[[monkey]][1]
        
        inspect_result <- monkey_inspect(item = monkey_items[[monkey]][1],
                       operation = mon$operation[mon$monkey == monkey],
                       divisible_by = mon$divisible_by[mon$monkey == monkey],
                       if_true = mon$if_true[mon$monkey == monkey],
                       if_false = mon$if_false[mon$monkey == monkey])
        
        monkey_inspect_count[[monkey]] <- monkey_inspect_count[[monkey]] + 1
        throw_to <- paste0("mon",inspect_result[2])
        item_to_throw <- inspect_result[1]
        monkey_items[[monkey]] <- monkey_items[[monkey]][-1]
        monkey_items[[throw_to]] <- append(monkey_items[[throw_to]], item_to_throw)
      }
    }
  }
}

# get level of monkey business
sort(unlist(monkey_inspect_count)) %>% tail(n=2) %>% prod()

# -------- part 2 --------

# reset monkey items
monkey_items <- as.list(setNames(mon$starting_items_vec, mon$monkey)) %>%
  map(., as.numeric)
monkey_inspect_count <- as.list(setNames(rep(0, num_monkeys), mon$monkey))

# get product of divisors
divprod <- prod(mon$divisible_by)

monkey_inspect_p2 <- function(item, operation, divisible_by, if_true, if_false){
  worry <- eval(parse(text = operation))
  reduced_worry <- worry - (worry %/% divprod)*divprod
  monkey_to <- ifelse(reduced_worry %% divisible_by == 0,
                      if_true,
                      if_false)
  return(c(reduced_worry, monkey_to))
}

# run through 10000 rounds
for (k in 1:10000){
  
  # single round of all monkeys
  for (j in 0:(num_monkeys-1)){
    monkey <- paste0("mon",j)
    if(length(monkey_items[[monkey]]) > 0) {
      
      # single monkey
      for (i in 1:length(monkey_items[[monkey]])){
        
        monkey_items[[monkey]][1]
        inspect_result <- monkey_inspect_p2(item = monkey_items[[monkey]][1],
                                         operation = mon$operation[mon$monkey == monkey],
                                         divisible_by = mon$divisible_by[mon$monkey == monkey],
                                         if_true = mon$if_true[mon$monkey == monkey],
                                         if_false = mon$if_false[mon$monkey == monkey])
        monkey_inspect_count[[monkey]] <- monkey_inspect_count[[monkey]] + 1
        throw_to <- paste0("mon",inspect_result[2])
        item_to_throw <- inspect_result[1]
        monkey_items[[monkey]] <- monkey_items[[monkey]][-1]
        monkey_items[[throw_to]] <- append(monkey_items[[throw_to]], item_to_throw)
      }
    }
  }
}

# get level of monkey business
sort(unlist(monkey_inspect_count)) %>% tail(n=2) %>% prod()