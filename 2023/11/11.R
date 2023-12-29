# day 11: cosmic expansion ------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input_raw <- read_csv("2023/11/input.txt", 
                      col_names = "x")

galaxy <- input_raw %>%
  mutate(x = map(x, ~ str_split_1(.x, ""))) %>%
  unnest_wider(x, names_sep = "") %>%
  mutate(across(everything(), ~ ifelse(.x == "#", 1, 0)),
         across(everything(), ~ as.numeric(.x)))

# names to 0-padded
names(galaxy) <- paste0("x",
       str_pad(str_replace(names(galaxy), "x", ""), 3, side = "left", pad = "0"))

# expand the empty rows and columns --------------
# add row IDs, expand rows
galaxy <- galaxy %>%
  mutate(empty_row = rowSums(across(everything())) == 0) %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname))

galaxy <- bind_rows(galaxy, galaxy %>%
            filter(empty_row)) %>%
  arrange(rowname)

# expand columns
empty_col <- colSums(galaxy) == 0

galaxy <- bind_cols(galaxy, galaxy[, empty_col], .name_repair = "universal")
galaxy <- galaxy %>% select(order(colnames(galaxy))) %>%
  select(-c(empty_row, rowname))

galaxy_mat <- as.matrix(galaxy)

# find all the galaxies and get pairwise distances
get_galaxy_distance <- function(gal1, gal2) {
  
  gal1 <- unname(gal1)
  gal2 <- unname(gal2)
  dis_vert <- abs(gal1[1] - gal2[1])
  dis_horiz <- abs(gal1[2] - gal2[2])
  return(dis_vert + dis_horiz)
}

# find all the galaxy positions
galaxies <- which(galaxy_mat == 1, arr.ind = T)

galaxies_df <- as_tibble(galaxies) %>%
  mutate(id = 1:n()) %>%
  relocate(id)

# get pairwise combinations
combos <- combn(galaxies_df$id, 2)
num_combos <- dim(combos)[2]

pairwise_dists <- c()
for (i in 1:num_combos){
  pair <- combos[,i]
  pairwise_dists[i] <- get_galaxy_distance(galaxies[pair[1], ],
                       galaxies[pair[2], ])
}
sum(pairwise_dists)

# part 2 ------------------------------------------------------------------
# empty rows are now a million
# so instead of adding 1, add 999,999
# don't expand as above, but identify the empty rows
input_raw <- read_csv("2023/11/input.txt", 
                      col_names = "x")

galaxy <- input_raw %>%
  mutate(x = map(x, ~ str_split_1(.x, ""))) %>%
  unnest_wider(x, names_sep = "") %>%
  mutate(across(everything(), ~ ifelse(.x == "#", 1, 0)),
         across(everything(), ~ as.numeric(.x)))

# names to 0-padded
names(galaxy) <- paste0("x",
                        str_pad(str_replace(names(galaxy), "x", ""), 3, side = "left", pad = "0"))

# expand the empty rows and columns --------------
# add row IDs, expand rows
empty_rows <- rowSums(galaxy) == 0
empty_cols <- unname(colSums(galaxy)) == 0

galaxy_mat <- as.matrix(galaxy)

# create new distances
row_dists <- tibble(row_id = c(1:nrow(galaxy)),
                      empty = empty_rows,
                      height = ifelse(empty,1000000,1)) %>%
  mutate(cdistance = cumsum(height))

col_dists <- tibble(col_id = c(1:length(galaxy)),
                      empty = empty_cols,
                      width = ifelse(empty,1000000,1)) %>%
  mutate(cdistance = cumsum(width))

# find all the galaxies and get pairwise distances
get_galaxy_distance <- function(gal1, gal2) {
  
  gal1 <- unname(gal1)
  gal2 <- unname(gal2)
  
  dis_vert <- abs(row_dists$cdistance[gal1[1]] - row_dists$cdistance[gal2[1]])
  dis_horiz <- abs(col_dists$cdistance[gal1[2]] - col_dists$cdistance[gal2[2]])
  
  return(dis_vert + dis_horiz)
}

# find all the galaxy positions
galaxies <- which(galaxy_mat == 1, arr.ind = T)
galaxies_df <- as_tibble(galaxies) %>%
  mutate(id = 1:n()) %>%
  relocate(id)

# get pairwise combinations
combos <- combn(galaxies_df$id, 2)
num_combos <- dim(combos)[2]

pairwise_dists <- c()
for (i in 1:num_combos){
  pair <- combos[,i]
  pairwise_dists[i] <- get_galaxy_distance(galaxies[pair[1], ],
                                           galaxies[pair[2], ])
}
sum(pairwise_dists)