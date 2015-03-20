# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
  has_adopted <- matrix(nrow = n.doctors, ncol = n.days)
  has_adopted[, 1] <- initial.doctors
  
  for (i in 1:(n.days-1)){
    random_doctors <- sample(n.doctors, 2)
    ran_doc1 <- random_doctors[1]
    ran_doc2 <- random_doctors[2]
    doctors_dayi <- has_adopted[,i]
    if (has_adopted[ran_doc1, i] == 1){
      if (has_adopted[ran_doc2, i] == 0){
      adopted <- sample(c(0, 1), 1, replace = TRUE, c(1-p,p))
        if (adopted == 1){
          doctors_dayi[ran_doc2] <- 1
        }
      }
    }
    else{
      if (has_adopted[ran_doc2, i] == 1){
        if (has_adopted[ran_doc1, i] == 0){
          adopted <- sample(c(0, 1), 1, replace = TRUE, c(1-p,p))
          if (adopted == 1){
            doctors_dayi[ran_doc1] <- 1
          }
        }
      }
    }
    
    has_adopted[,i+1] <- doctors_dayi
  }
  
  return (has_adopted)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

nonbel.doctors <- rep(0, times = 90)
bel.doctors <- rep(1, times = 10)
initial.doctors <- sample(c(nonbel.doctors, bel.doctors), 100)

n.days <- 50
test1 <- sim.doctors(initial.doctors, length(initial.doctors), n.days, .4)
test2 <- sim.doctors(initial.doctors, length(initial.doctors), n.days, .5)
test3 <- sim.doctors(initial.doctors, length(initial.doctors), n.days, .6)
test4 <- sim.doctors(initial.doctors, length(initial.doctors), n.days, .7)
test5 <- sim.doctors(initial.doctors, length(initial.doctors), n.days, .8)

num_doctors_adopt <- function(sample_matrix){
  doctors_adopt <- c()
  for (i in 1:ncol(sample_matrix)){
    doctors_adopt_day <- 0
    for (j in 1:nrow(sample_matrix)){
      if (sample_matrix[j, i] == 1){
        doctors_adopt_day <- doctors_adopt_day + 1
      }
    }
    
    doctors_adopt <- append(doctors_adopt, doctors_adopt_day)
  }
  
  return(doctors_adopt)
}

test1_num_adopt <- num_doctors_adopt(test1)
test2_num_adopt <- num_doctors_adopt(test2)
test3_num_adopt <- num_doctors_adopt(test3)
test4_num_adopt <- num_doctors_adopt(test4)
test5_num_adopt <- num_doctors_adopt(test5)

plot(test5_num_adopt, main = "Doctors who adopted based on probability of adoption", 
     xlab = "Day", ylab = "Number of doctors who adopted", type = 'l', col = "red")
lines(test4_num_adopt)
lines(test3_num_adopt, col = "green")
lines(test2_num_adopt, col = "blue")
lines(test1_num_adopt, col = "orange")


