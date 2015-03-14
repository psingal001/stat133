#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


bml.sim <- function(r, c, p){
  new.matrix <- bml.init(r, c, p)
  image(new.matrix, axes = FALSE, col = c("white", "red", "blue"))
  num_iterations = 5
  num_steps = 0
  new.matrix <- bml.step(new.matrix)
  grid.lock <- !new.matrix[[2]]
  
  while (num_iterations > 0 & grid.lock == FALSE){
    new.matrix <- bml.step(new.matrix[[1]])
    grid.lock <- !new.matrix[[2]]
    num_iterations <- num_iterations - 1
    num_steps <- num_steps + 1
    image(new.matrix[[1]], axes = FALSE, col = c("white", "red", "blue"))
  }
  image(new.matrix[[1]], axes = FALSE, col = c("white", "red", "blue"))
  return (list(num_steps, grid.lock)) #, new.matrix[[1]]
}

# This function goes through the BML simulation a defined number of iterations
# It stores the number of steps a each simulation goes through before reaching gridlock and returns
#   the vector with the number of steps for each simulation

bml.sim.data.set <- function(r, c, p, num_iter){ 
  num_steps_set <- c()
  for (i in 1:num_iter){
    num_steps_i <- bml.sim(r, c, p)[[1]]
    num_steps_set <- append(num_steps_set, num_steps_i)
  }
  
  return(num_steps_set)
}

bml.sim.data.set(25, 25, .6, 30)


### Creating different box plots depicting the change in state ###
# Graph 1 will be the beginning state of a 50x50 state at density 60%
# Graph 2 will be the end state of the system mentioned above
# Graph 3 will be a line graph comparing the trend of changing the density of the system
#   Will use a 25x25 system with density starting at 10% and increasing to 90% \
# Graph 4 will be a _____ graph comparing the trend of changing the size of the system; constant density
#   Will start with a 10x10 system and increase by 5 on each side (maintain a square status)
#   Density is 50%

# For both graphs, x-axis will be the metric being changed (density or system size)
#   y-axis is the mean number of steps system takes to reach gridlock



### -------------------- ###

# Graph 1 and 2 #
graph1_and2_system <- bml.sim(50, 50, .6)
graph1_and2_system


library("ggplot2")

# Graph 3 #


# This function will go through num_iterations for any size matrix and return a list of 
mean_change_density <- function(r = 25, c = 25, density_vector){
  mean_data_set <- c()
  for (i in 1:length(density_vector)){
    mean_data_set <- append(mean_data_set, 
                            mean(bml.sim.data.set(r, c, density_vector[i], 20)))
  }
  return(mean_data_set)
}

den_vec_sample1 <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9)
graph3_set <- mean_change_density(25, 25, den_vec_sample1)
graph3_data_frame <- data.frame(den_vec_sample1, graph3_set)

graph3 <- ggplot(graph3_data_frame,
       aes(den_vec_sample1, graph3_set)) + geom_point()+ labs(
         title = "Mean # of Steps until Grid Lock vs. System Density", 
         x = "Density", y = "#Number of Steps")
graph3 <- graph3 + theme(axis.title.x = element_text(vjust = -.25), 
        axis.title.y = element_text(vjust = .35)) + scale_x_continuous(
          breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
graph3


#### Graph 4 ####


# This function will go through num_iter
mean_change_size <- function(p = .5, size_vector){
  mean_data_set <- c()
  for (i in 1:length(size_vector)){
    mean_data_set <- append(mean_data_set, 
                            mean(bml.sim.data.set(size_vector[i], size_vector[i], p, 20)))
  }
  return(mean_data_set)
}

size_vector <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)
graph4_xlabels <- c("10 x 10", "15 x 15", "20 x 20", "25 x 25", "30 x 30", 
                    "35 x 35", "40 x 40", "45 x 45", "50 x 50")
graph4_set <- mean_change_size(.6, size_vector)
graph4_data_frame <- data.frame(graph4_xlabels, graph4_set)


graph4 <- ggplot(graph4_data_frame,
                 aes(size_vector, graph4_set)) + geom_point()+ labs(
                   title = "Mean # of Steps until Grid Lock vs. System Size", 
                   x = "Dimension value of square matrix system", y = "#Number of Steps")
graph4 <- graph4 + theme(axis.title.x = element_text(vjust = -.25), 
                         axis.title.y = element_text(vjust = .35)) + scale_x_continuous(
                           breaks = size_vector) + ylim(c(0,160))
graph4
