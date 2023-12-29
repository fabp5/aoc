# day 5: seeds ------------------------------------------------------------
library(tidyverse)

# part 1 ------------------------------------------------------------------
input <- read_lines("2023/05/input.txt")

# parse the map, split into parts
x2 <- tibble(x = input) %>%
  mutate(spl = x == "",
         id_group = cumsum(spl)) %>%
  group_by(id_group) %>%
  filter(!spl) %>%
  summarise(map_info = list(x))

create_map_name <- function(map_raw){
  name_source <- str_extract(map_raw[1], "^[a-z]+")
  name_dest   <- str_extract(map_raw[1], "(?<=-)\\w+(?=\\s)")
  name_map    <- paste(name_source, name_dest, sep = "_")
  return(name_map)
}

create_map_df <- function(map_raw){
  
  map_lines <- map_raw[-1]
  
  map_df <- map(map_lines, ~ as.numeric(str_split_1(.x, " "))) %>%
    tibble(map_info = .) %>%
    unnest_wider(map_info, names_sep = "_") %>%
    rename(dest = map_info_1,
           source = map_info_2,
           range = map_info_3) %>%
    mutate(diff = dest-source,
           source_nums = map2(source, range, ~ c(.x, .x + .y - 1)))
  return(map_df)
}

map_source_to_dest <- function(number, mapdf){
  
  mapdf_temp <- mapdf %>%
    rowwise() %>%
    mutate(num_present = between(as.numeric(number), source_nums[1], source_nums[2])) %>%
    ungroup()
  
  if(sum(mapdf_temp$num_present) == 0){
    return(number)
  } else {
    return(as.numeric(number) + (mapdf_temp$diff[mapdf_temp$num_present == TRUE]))
  }
}

# function to map a number across all the maps ----------------------------
map_seed_across_all_maps <- function(seed_no) {
  
  num_to_map <- seed_no
  
  for(i in 1:length(maps_list)){
    num_to_map <- map_source_to_dest(num_to_map, maps_list[[i]])
  }
  return(num_to_map)
}

# create list of maps
seeds <- x2$map_info[[1]]
seeds <- str_extract_all(seeds, "\\d+") %>% unlist()
raw_maps_list <- x2$map_info[-1]

# get names for the maps
maps_names <- map_vec(raw_maps_list, create_map_name)
maps_list <- map(raw_maps_list, create_map_df)
names(maps_list) <- maps_names

# map across all the seeds
map_vec(seeds, map_seed_across_all_maps) %>% min()