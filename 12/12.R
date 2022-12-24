# day 12: hills
require(tidyverse)

example_input <- read_csv("12/example_input.txt", 
                          col_names = "x")

input <- read_csv("12/input.txt",
                  col_names = "x")

hill <- input

hillwidth <- nchar(hill$x[1])
hillheight <- nrow(hill)

hill <- hill %>%
  separate(x, into = paste0("col",seq(1:hillwidth)), sep = seq(1:(hillwidth-1)))
  
start_sq <- which(hill == "S", arr.ind = TRUE)
end_sq <- which(hill == "E", arr.ind = TRUE)

# convert start and end to numeric values
hill[hill == "S"] <- "a"
hill[hill == "E"] <- "z"

# convert letters to numeric heights
hilln <- hill %>%
  mutate(across(everything(), ~ match(.x, letters)))

# find next possible moves from a square
find_next_squares <- function(row,col) {
  valid_move <- function(currentsq_val, nextsq_val)  {nextsq_val <= currentsq_val+1}
  
  square_val <- hilln[row,col]
  
  valid_moves <- c(up    = ifelse(row != 1,
                                  valid_move(hilln[row,col], hilln[row-1,col]),
                                  FALSE),
                   right = ifelse(col != hillwidth,
                                  valid_move(hilln[row,col], hilln[row,col+1]),
                                  FALSE),
                   down  = ifelse(row != hillheight,
                                  valid_move(hilln[row,col], hilln[row+1,col]),
                                  FALSE),
                   left  = ifelse(col != 1,
                                  valid_move(hilln[row,col], hilln[row,col-1]),
                                  FALSE))
  next_squares <- list()
  
  if(valid_moves["up"]){
    next_squares[[1]] <- c(row-1,col)
  }
  if(valid_moves["right"]){
    next_squares[[2]] <- c(row,col+1)
  }
  if(valid_moves["down"]){
    next_squares[[3]] <- c(row+1,col)
  }
  if(valid_moves["left"]){
    next_squares[[4]] <- c(row,col-1)
  }
  
  if(length(next_squares) > 0){
    return(next_squares[!sapply(next_squares,is.null)])
  } else {return(NA)}
  
}

# find possible moves each round
sqs_per_round <- list()
sqs_per_round[[1]] <- paste0(c(start_sq[1],start_sq[2]),collapse="|")
stop = FALSE
for (rd in 1:1000) {
  
  squares_sublist <- c()
  for (j in 1:length(sqs_per_round[[rd]])){
    row <- as.numeric(str_extract(sqs_per_round[[rd]][[j]],"^[0-9]+"))
    col <- as.numeric(str_extract(sqs_per_round[[rd]][[j]],"[0-9]+$"))
    
    all_sq <- map_chr(find_next_squares(row,col), ~ paste0(.x, collapse="|"))
    all_sq <- all_sq[!all_sq == "NA"]
    
    squares_sublist <- append(squares_sublist,all_sq)
    squares_sublist <- unique(squares_sublist)

    if(paste0(end_sq[1],"|",end_sq[2]) %in% squares_sublist){
      stop = TRUE
      break
    }
  }
  # make the previous squares impassable
  for (k in 1:length(sqs_per_round[[rd]])){
    row <- as.numeric(str_extract(sqs_per_round[[rd]][[k]],"^[0-9]+"))
    col <- as.numeric(str_extract(sqs_per_round[[rd]][[k]],"[0-9]+$"))
    hilln[row,col] <- 99
  }
  if(stop){
    print(paste0("STOP:", rd))
    break}
  
  sqs_per_round[[rd + 1]] <- squares_sublist
}