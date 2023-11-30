# day 7
require(tidyverse)

input <- read_csv("07/input.txt",
                          col_names = FALSE)

num_lines <- nrow(input)

paths <- tibble(ln = head(input$X1,num_lines),
                ln_type = character(num_lines),
                path = character(num_lines),
                ) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  # get the line type
  mutate(ln_type = case_when(grepl("^\\$ cd",ln) ~ "cd",
                             grepl("^\\$ ls",ln) ~ "ls",
                             TRUE ~ "output"),
         contents_grp = ifelse(ln_type == "output",
                               cumsum(ln_type == "ls"),
                               NA),
         contents = ifelse(ln_type == "output",
                           ln,
                           NA),
         contents_type = ifelse(ln_type == "output",
                                ifelse(grepl("^dir",contents),
                                       "dir",
                                       "file"),
                                NA),
         filename = ifelse(contents_type == "file",
                           gsub("^[0-9]+ ","",contents),
                           NA),
         filesize = as.numeric(ifelse(contents_type == "file",
                           str_extract(contents, "^[0-9]+"),
                           NA)),
         dirname = ifelse(contents_type == "dir",
                          gsub("^dir ","",contents),
                          NA),
         cd_cmd = ifelse(ln_type == "cd",
                         gsub("\\$ cd ","",ln),
                         NA),
         cd_type = case_when(cd_cmd == "/" ~ "root",
                             cd_cmd == ".." ~ "back",
                             grepl("[a-z]+",cd_cmd) ~ "dir")
  )

fpath <- "/"

for (i in 1:num_lines){
  
  if(paths$ln_type[i] == "cd"){
    fpath <- case_when(paths$cd_type[i] == "dir"  ~ paste0(fpath,paths$cd_cmd[i],"/"),
                       paths$cd_type[i] == "back" ~ gsub("[a-z]+/$", "", fpath),
                       paths$cd_type[i] == "root" ~ "/")
  
    paths$path[i] <- fpath
  }
}

# get sizes of individual directories
dir_sizes <- paths %>%
  select(ln_type, path, contents_grp, contents_type, filesize, dirname) %>%
  fill(path) %>%
  group_by(path) %>%
  summarise(dirsize = sum(wt = filesize, na.rm = TRUE)) %>%
  mutate(dirsize_r = NA_real_)

# get the sizes of the below directories
for (i in 1:nrow(dir_sizes)){
  pathstart <- dir_sizes$path[i]
  
  dir_sizes$dirsize_r[i] <- dir_sizes %>%
    filter(grepl(paste0("^",pathstart),path)) %>%
    pull(dirsize) %>%
    sum()
}

# get the sum of all directories with a total size of at most 100000
dir_sizes %>%
  filter(dirsize_r <= 100000) %>%
  pull(dirsize_r) %>%
  sum()

# --------- part 2 ---------
totalsize <- 70000000
space_needed <- 30000000

used <- dir_sizes$dirsize_r[dir_sizes$path == "/"]
used == (dir_sizes$dirsize %>% sum()) # this matches

unused <- totalsize - used
to_delete <- space_needed - unused

dir_sizes %>%
  filter(dirsize_r >= to_delete) %>%
  slice_min(dirsize_r) %>%
  pull(dirsize_r)
