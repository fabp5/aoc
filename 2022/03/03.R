# day 3: rucksacks
require(tidyverse)

example_input <- read_csv("03/example_input.txt",
                          col_names = FALSE)

input <- read_csv("03/input.txt",
                  col_names = FALSE)

contents <- input

# -------- part 1 --------

# item/priority lookup table
lookup <- tibble(
  item = paste(c(letters, LETTERS)),
  priority = seq(1:52)
)

# function to get common item
get_common_item <- function(str){
  strlen = nchar(str)
  
  comp1 <- substr(str,1,strlen/2) %>%
    strsplit(., "") %>%
    unlist()
  
  comp2 <- substr(str,strlen/2+1,strlen) %>%
    strsplit(., "") %>%
    unlist()

  return(intersect(comp1,comp2))
}

map(input$X1, get_common_item) %>%
  unlist() %>%
  as_tibble() %>%
  left_join(., lookup, by = c("value" = "item")) %>%
  pull(priority) %>%
  sum()


# ------ or: list/loop method ----
contents <- input %>%
  mutate(strlen = nchar(X1),
         c1 = NA,
         c2 = NA,
         c1_s = NA,
         c2_s = NA,
         common_item = NA)

for(i in 1:nrow(contents)){
  
  h <- contents$strlen[i]/2
  
  contents$c1[i] <- substring(contents$X1[i],1,h)
  contents$c2[i] <- substring(contents$X1[i],h+1,h*2)
  
  contents$c1_s[i] <- strsplit(contents$c1[i], "")
  contents$c2_s[i] <- strsplit(contents$c2[i], "")
  
  contents$common_item[i] <- intersect(contents$c1_s[[i]], contents$c2_s[[i]])
  
}

# item/priority lookup table
lookup <- tibble(
  item = paste(c(letters, LETTERS)),
  priority = seq(1:52)
)
  
left_join(contents, lookup, by = c("common_item" = "item")) %>%
  pull(priority) %>%
  sum()

# -------- part 2 --------
input %>%
  mutate(group = ceiling(seq_along(X1) / 3),
         items = strsplit(X1, "")) %>%
  group_by(group) %>%
  summarise(badge = reduce(items, intersect)) %>%
  left_join(., lookup, by = c("badge" = "item")) %>%
  pull(priority) %>%
  sum()


# ------ or: list/loop method ----
elf_groups <- split(strsplit(contents$X1,""),
                    ceiling(seq_along(contents$X1) / 3))

badge <- c()
for (i in 1:length(elf_groups)) {
  badge[i] <- Reduce(intersect, list(elf_groups[[i]][[1]],elf_groups[[i]][[2]],elf_groups[[i]][[3]]))
  
}

left_join(as_tibble(badge), lookup, by = c("value" = "item")) %>%
  pull(priority) %>%
  sum()
