install.packages("scales")
library("scales")
library("ggplot2")

xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

###
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  return (unlist(tapply(unique(x), y, sample, size = length(y), replace = TRUE)))
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  resampled_err <- sample(err, length(fit))
  return (fit + resampled_err)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree == 1){
    coeff <- c(coef(lm(y ~ x)))
  } else if (degree == 2){
    coeff <- c(coef(lm(y~x + I(x^2))))
  }
  return(coeff)
}

oneBoot = function(data, fit = NULL, err = NULL degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  
  ### Use fitModel to fit a model to this bootstrap Y 
  if (fit == NULL){
    new_y <- genBootY(data$x, data$y)
    return(fitModel(data$x, new_y, degree))
  } else {
    new_y <- genBootR(fit$x, err)
    return(fitModel(fit$x, new_y, degree))
  }
}

repBoot = function(data, B = 1000){
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  
  lin_model <- fitModel(data$x, data$y, degree = 1)
  lin_fit <- fitted(lin_model)
  lin_err <- data$y - lin_fit
  
  quad_model <- fitModel(data$x, data$y, degree = 2)
  quad_fit <- fitted(quad_model)
  quad_err <- data$y - quad_fit
  
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  lin_Y <- data.frame()
  lin_R <- data.frame()
  quad_Y <- data.frame()
  quad_R <- data.frame()
  
  for (i in 1:B){
    coef_lin_Y <- oneBoot(data, degree = 1)
    lin_Y <- rbind(lin_Y, coef_lin_Y)
    
    coef_lin_R <- oneBoot(data, lin_fit, lin_err, degree = 1)
    lin_R <- rbind(lin_R, coef_lin_R)
    
    coef_quad_Y <- oneBoot(data, degree = 2)
    quad_Y <- rbind(quad_Y, coef_quad_Y)
    
    coef_quad_R <- oneBoot(data, quad_fit, quad_err, degree = 2)
    quad_R <- rbind(quad_R, coef_quad_R)
  }
  
  coeff <- list(lin_Y, lin_R, quad_Y, quad_R)
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  return(coeff)
}

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out

  plot(x, y, type = "l", col = alpha("red", 0.3))
  myCurve <- function(a, b, c){
    curve(a*x^2 + b*x + c, from = 0, to = 1000)
  }
  
  mapply(myCurve, coeff)
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
runSim()
