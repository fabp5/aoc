# day 9: rope bridge

require(tidyverse)
require(gganimate)

example_input <- read_table("09/example_input.txt", 
                            col_names = c("direction", "moves"))

input <- read_table("09/input.txt", 
                    col_names = c("direction", "moves"))

inst <- example_input

num_lines <- nrow(inst)
num_moves <- sum(inst$moves)

# how big is the grid?
# do I need to calculate it?

# first calculate where the head goes

hx <- 1
hy <- 1

# function to calculate where the tail is
move_tail <- function(hp, tp){
  
  hx <- hp[1]
  hy <- hp[2]
  tx <- tp[1]
  ty <- tp[2]
  #print(paste("h:",hx, hy, "| t:",tx, ty))
  
  # see if tail doesn't need to move
  if(abs(tx - hx) < 2 & abs(ty - hy) < 2){
    #print("stay")
    return(c(tx, ty))
    
  } else {
    
   # print("move")
    
    if(hx == tx) {
      ty <- ty + sign(hy - ty)
      } else if(hy == ty){
      tx <- tx + sign(hx - tx)
      # diagonals - if not on the same line either way
      } else {
        ty <- ty + sign(hy - ty)
        tx <- tx + sign(hx - tx)
      }
  }
  return(c(tx, ty))
}

move_tail(c(4,3),c(2,2))

# requires vector input
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

hp <- c(1,1)
tp <- c(1,1)
tail_positions <- tibble(hx = numeric(num_moves),
                         hy = numeric(num_moves),
                         tx = numeric(num_moves),
                         ty = numeric(num_moves))
  
move_count <- 0
for(i in 1:num_lines){
  
  for(j in 1:inst$moves[i]){
    
    move_count <- move_count + 1
    
    hp <- move_head_1(hp, inst$direction[i])
    #print(hp)
    tp <- move_tail(hp,tp)
    tail_positions$hx[move_count] <- hp[1]
    tail_positions$hy[move_count] <- hp[2]
    
    tp <- move_tail(hp,tp)
    tail_positions$tx[move_count] <- tp[1]
    tail_positions$ty[move_count] <- tp[2]
    
    print(move_count)
  }
}

n_distinct(select(tail_positions,tx,ty))

# visualise the rope

test_df <- tibble(
  x = rep(1:3,3),
  y = c(1,1,1,2,2,2,3,3,3),
  bw = c(0,0,0,1,1,0,0,0,0)
)

tail_positions <- rownames_to_column(tail_positions, "move")

# tile method
ggplot(test_df, aes(x=x, y=y, fill=bw)) + 
  geom_tile(colour = "white")

tail_positions_long <- tail_positions %>%
  pivot_longer(-move,
               names_to = "head_tail",
               values_to = "position") %>%
  separate(head_tail,into = c("head_tail","xy"), sep=1) %>%
  pivot_wider(id_cols = c(head_tail),
              names_from = xy,
              values_from = position)

# scatter method
pl <- ggplot(tail_positions_long, aes(x = x, y = y, colour = head_tail)) +
  geom_point(shape = 15, size = 10, alpha = 0.5) +
  theme_minimal() +
  transition_states(move) +
  labs(title = "Move: {frame}")

pl <- ggplot(tail_positions) +
  geom_point(aes(x = hx, y = hy), shape = 15, size = 10, alpha = 0.5, colour = "red") +
  geom_point(aes(x = tx, y = ty), shape = 15, size = 10, alpha = 0.5, colour = "blue") +
  theme_minimal() +
  transition_states(move) +
  labs(title = "frame: {frame}")

animate(pl, fps = 2, nframes = 24)

for(i in 1:24){
  ggplot(tail_positions[i,]) +
    geom_point(aes(x = hx, y = hy), shape = 15, size = 10, alpha = 0.5, colour = "red") +
    geom_point(aes(x = tx, y = ty), shape = 15, size = 10, alpha = 0.5, colour = "blue") +
    theme_minimal() +
    xlim(0,6) +
    ylim(0,6)
    #transition_states(move) +
    #labs(title = "frame: {frame}")
  ggsave(filename = paste0("frame_",i,".png"))
}


# --------- part 2: long rope --------
