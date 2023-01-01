# day 9: rope bridge

require(tidyverse)
require(gganimate)

input <- read_table("09/input.txt", 
                    col_names = c("direction", "moves"))

inst <- input %>%
  uncount(moves)

num_moves <- nrow(inst)

# first calculate where the head goes
hx <- 1
hy <- 1

# function to calculate where the tail is
move_tail <- function(hp, tp){
  hx <- hp[1]
  hy <- hp[2]
  tx <- tp[1]
  ty <- tp[2]
  
  # see if tail doesn't need to move
  if(abs(tx - hx) < 2 & abs(ty - hy) < 2){
    return(c(tx, ty))
  } else {
    # on same line
    if(hx == tx) {
      ty <- ty + sign(hy - ty)
      } else if(hy == ty){
      tx <- tx + sign(hx - tx)
      # diagonals
      } else {
        ty <- ty + sign(hy - ty)
        tx <- tx + sign(hx - tx)
      }
  }
  return(c(tx, ty))
}

# move head: requires vector input
move_head_1 <- function(hp, dir) {
  hx <- hp[1]
  hy <- hp[2]
  if(dir == "R"){
    hx <- hx + 1
  } else if(dir == "L") {
    hx <- hx - 1
  } else if(dir == "U") {
    hy <- hy + 1
  } else if(dir == "D") {
    hy <- hy - 1
  }
  return(c(hx,hy))
}

# set head and tail positions
hp <- c(1,1)
tp <- c(1,1)

rope_positions <- tibble(hx = numeric(num_moves),
                         hy = numeric(num_moves),
                         tx = numeric(num_moves),
                         ty = numeric(num_moves))
  
for(i in 1:num_moves){
  hp <- move_head_1(hp, inst$direction[i])
  tp <- move_tail(hp,tp)
  rope_positions$hx[i] <- hp[1]
  rope_positions$hy[i] <- hp[2]
  rope_positions$tx[i] <- tp[1]
  rope_positions$ty[i] <- tp[2]
}

n_distinct(select(rope_positions,tx,ty))

# visualise the rope over the starting moves
for(i in 1:24){
  ggplot(rope_positions[i,]) +
    geom_point(aes(x = hx, y = hy), shape = 15, size = 10, alpha = 0.5, colour = "red") +
    geom_point(aes(x = tx, y = ty), shape = 15, size = 10, alpha = 0.5, colour = "blue") +
    theme_minimal() +
    xlim(-6,6) +
    ylim(-6,6) +
    coord_fixed()
  ggsave(filename = paste0("09/plot/frame",str_pad(i, width = 2, pad = 0),".png"))
}

# --------- part 2: long rope --------

move_tail_2 <- function(tp, hp){
  hx <- hp[1]
  hy <- hp[2]
  tx <- tp[1]
  ty <- tp[2]
  
  # check if tail doesn't need to move
  if(abs(tx - hx) < 2 & abs(ty - hy) < 2){
    return(c(tx, ty))
  } else {
    # on same line
    if(hx == tx) {
      ty <- ty + sign(hy - ty)
    } else if(hy == ty){
      tx <- tx + sign(hx - tx)
      # diagonals
    } else {
      ty <- ty + sign(hy - ty)
      tx <- tx + sign(hx - tx)
    }
  }
  return(c(tx, ty))
}

longrope_positions <- rope_positions %>%
  mutate(hp = map2(hx,hy,c),
         t1 = map2(tx,ty,c)) %>%
  rownames_to_column("move")

for (i in 2:9){
  longrope_positions[[paste0("t",i)]] <- accumulate(longrope_positions[[paste0("t", i-1)]],
                                                   move_tail_2,
                                                   .init = c(1,1))[-1]
}

# get number of positions visited by tail
n_distinct(longrope_positions$t9)

# visualise the rope
longrope_positions_l <- longrope_positions[-(2:5)] %>%
  pivot_longer(-move,
               names_to = "tail_sec") %>%
  hoist(value, x = 1, y = 2)

# visualise - gganimate method
pl2 <- ggplot(longrope_positions_l[1:800, ]) +
  geom_point(aes(x = x, y = y, colour = tail_sec), shape = 15, size = 8, alpha = 0.5) +
  theme_void() +
  xlim(-15, 15) +
  ylim(-15, 15) +
  transition_states(move) +
  labs(title = "frame: {frame}")

animate(pl2, fps = 2, nframes = 80)

# or method to generate frames (first 80 moves)
for(i in 1:80){
  ggplot(longrope_positions_l[((i*10)-9):(i*10), ]) +
    geom_point(aes(x = x, y = y, colour = tail_sec), shape = 15, size = 8, alpha = 0.5) +
    theme_void() +
    xlim(-10, 10) +
    ylim(-15, 5) +
    theme(legend.position = "none") +
    coord_fixed()
  ggsave(filename = paste0("09/plot/rope",str_pad(i, width = 2, pad = 0),".png"))
}

# convert to gif with imagemagick
# magick convert -delay 30 -loop 0 plot/rope*.png rope_animate.gif