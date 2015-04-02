#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  colored_cars <- floor(r*c*p)
  total_spaces <- r*c #size of population
  if (colored_cars %% 2 == 1){
    colored_cars <- colored_cars - 1
  }
  car_location <- c(rep(0, total_spaces - colored_cars), rep(1, colored_cars/2), rep(2, colored_cars/2))
  m <- matrix(sample(car_location, total_spaces), nrow = r, ncol = c) #car_location cannot be less than total_spaces
  return(m)
}


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the bmred cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  
  matrix.move.east <- move.east(m) #move car east
  
  matrix.move.N.and.E <- move.north(matrix.move.east) #move car north
  
  if (all(matrix.move.N.and.E == m)){ #check if system changed or not
    grid.new <- FALSE
  }
  else{ #system changed and thus need to change the original matrix m
    grid.new <- TRUE
    m <- matrix.move.N.and.E
  }
  
  return(list(m, grid.new))
}

move.east <- function(m){
  new_m <- m
  end_row = ncol(m) #end of row means that you are in the last col
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if (m[i, j] == 1){
        if (j == end_row){ #checking if car is at the end of the matrix
          if (m[i, 1] == 0){ # changed car position if car at beginning of row is 0
            new_m[i, j] <- 0
            new_m[i, 1] <- 1
          }
        }
        else{
          if (m[i, j+1] == 0){ #change car position if next space is 0
            new_m[i, j] <- 0
            new_m[i, j+1] <- 1
          }
        }
      }
    }
  }
  return(new_m)
}

move.north <- function(m){
  new_m <- m
  end_col = nrow(m) #end of column means that you are in the last row
  
  for (i in 1:nrow(m)){ #i will iterate through the rows of m
    for (j in 1:ncol(m)){ #j will iterate through the rows of m
      if (m[i,j] == 2){ #only want to look at car values equal to 2
        if (i == 1){ # check if car is at the top of the matrix
          if (m[end_col, j] == 0){ #check if space at the bottom of the column is empty
            new_m[i, j] <- 0
            new_m[end_col, j] <- 2
          }
        }
        else{
          if (m[i-1,j] == 0){
            new_m[i, j] <- 0
            new_m[i-1, j] <- 2
          }
        }
      }
    }
  }
  return(new_m)
}


#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  new.matrix <- bml.init(r, c, p)
  image(new.matrix, axes = FALSE, col = c("white", "red", "blue"))
  num_iterations = 1000
  num_steps = 0
  new.matrix <- bml.step(new.matrix)
  grid.lock <- !new.matrix[[2]]
  
  while (num_iterations > 0 & grid.lock == FALSE){
    new.matrix <- bml.step(new.matrix[[1]])
    grid.lock <- !new.matrix[[2]]
    num_iterations <- num_iterations - 1
    num_steps <- num_steps + 1    
  }
  image(new.matrix[[1]], axes = FALSE, col = c("white", "red", "blue"))
  return (list(num_steps, grid.lock, new.matrix[[1]]))
}

