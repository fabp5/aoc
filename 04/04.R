# day 4: cleaning
require(tidyverse)

input <- read_delim("04/input.txt",
                            col_names = FALSE,
                            delim = ".")

# -------- part 1: complete overlaps --------
input %>%
  separate(X1, into = c("g1_f", "g1_l", "g2_f", "g2_l"),
           sep = ",|-") %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(g1_first_bigger = g1_f >= g2_f,
         g1_last_smaller = g1_l <= g2_l,
         g2_first_bigger = g2_f >= g1_f,
         g2_last_smaller = g2_l <= g1_l,
         within = (g1_first_bigger & g1_last_smaller) | g2_first_bigger & g2_last_smaller) %>%
  pull(within) %>%
  sum()
  
# -------- part 2: any overlaps --------

rota <- input %>%
  separate(X1, into = c("g1_f", "g1_l", "g2_f", "g2_l"),
           sep = ",|-") %>%
  mutate(across(everything(), as.numeric))

g1_seq <- list()
g2_seq <- list()

for (i in 1:nrow(rota)){
  g1_seq[[i]] <- seq(rota$g1_f[i], rota$g1_l[i])
  g2_seq[[i]] <- seq(rota$g2_f[i], rota$g2_l[i])
}

overlaps <- c()
for(i in 1:length(g1_seq)){
  overlaps[i] <- length(intersect(g1_seq[[i]], g2_seq[[i]])) >= 1
}
sum(overlaps)
