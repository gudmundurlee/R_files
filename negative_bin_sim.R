setwd("~/AGR/R_Projects/SafetyStock")

library(MASS)
library(tseries)
library(ggplot2)
library(seasonal)
library(forecast)

hist(rnegbin(100, mu = 100, theta = 0.5))


NBsim <- rnegbin(100, mu = 100, theta = 0.5)

mean(NBsim)
var(NBsim)

# define the function
simulate_neg_binom <- function(mean, var, n) {
  
  # calculate theta and size from the mean and variance
  theta <- (mean^2)/(var - mean)
  size <- mean/theta
  
  # simulate values from a negative binomial distribution with the calculated theta and size
  #sim_values <- rnbinom(n, size, theta)
  sim_values <- rnegbin(n, mu = mean, theta = theta)
  
  # return a list with the simulated values and the calculated theta and size
  list(values = sim_values, theta = theta, size = size)
}

# test the function
NBsim <- simulate_neg_binom(mean = 4.0623, var = 6.7055, n = 360*2)
mean(NBsim$values)
var(NBsim$values)
NBsim$theta
NBsim$size


calculateSum <- function(x) {
  # Determine the length of the input vector
  n <- length(x)
  
  # Create a sequence of numbers from 1 to n
  numbers <- seq(1, n)
  
  # Split the sequence into subgroups of size 30
  subgroups <- split(numbers, ceiling(seq_along(numbers)/30))
  
  # Apply the sum function to each subgroup and return the result
  result <- unlist(lapply(subgroups, function(i) sum(x[i])))
  
  # Return the result
  return(result)
}

test <- calculateSum(NBsim$values)
plot(ts(test))
test

# define the function
