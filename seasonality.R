setwd("~/AGR/R_Projects/Seasonality")

library(MASS)
library(tseries)
library(ggplot2)
library(seasonal)
library(forecast)
library(dplyr)
library(readxl)
demo <- read_excel("AGR/R_Projects/Seasonality/demo.xlsx")

dim(demo)



agr_info <- function(item){
  agr_df <- demo %>% 
    filter(item_id == item)
  
  start_yr <- unique(agr_df$start_yr)
  start_mth <- unique(agr_df$start_mth)
  
  agr_ts <- ts(agr_df$sale, start = c(start_yr, start_mth), frequency = 12)
  
  output <- decompose_ts(agr_ts)
  df <- data.frame(time = seq(1,length(output$y)), 
                   y = output$y, 
                   seasonality = output$seasonality,
                   trend = output$trend, 
                   random = output$remainder)
  #plot_df(df)
  return(df)
}

agr_df <- agr_info(61)
plot_df(agr_df)


is_seasonal_regression <- function(y){
  n <- length(y)
  
  x <- seq(1,n)
  season <-  as.factor(((x-1) %% 12)+1)
  df <- data.frame(y = y, Time = x, Month = season)
  fit0 <- lm(y~Time, data = df)
  fit1 <- lm(y~Time+Month, data = df)
  
  
  
  s_test <- anova(fit0, fit1)
  
  p_value <- s_test$`Pr(>F)`[2]
  
  if(p_value < 0.05){
    #print(sprintf("P-value is %f so there we reject the hypothesis that the models are the same therefore we have a seasonal effect", p_value))
    return(TRUE)
  }
  else {
    #print(sprintf("P-value is %f so there we cannot reject the hypothesis that the models are the same therefore no seasonal effect", p_value))
    return(FALSE)
  }
  
  #return(df)
}

is_seasonal_regression(agr_df$y)

decompose_ts <- function(y) {
  
  # check the length of y and return an error if it is less than 24
  if (length(y) < 24) {
    stop("Error: time series vector must have at least 24 values")
  }
  
  # decompose the time series into its seasonal and trend components using the stl() function
  ts_decomp <- stl(y, s.window = "periodic")
  
  # extract the seasonal and trend components from the decomposition
  seasonality <- ts_decomp$time.series[, 1]
  trend <- ts_decomp$time.series[, 2]
  remainder <- ts_decomp$time.series[, 3]
  
  # return a list with the original time series, the seasonal component, and the trend component
  list(y = y, seasonality = seasonality, trend = trend, remainder = remainder)
}

# Load the forecast package
library(forecast)
lambdas <- seq(-1,1,0.1)
# Fit a Box-Cox transformation to the data

bc_fit <- lapply(lambdas, FUN = function(x){BoxCox(agr_df$y, lambda = x)}) #BoxCox(agr_df$y, lambda = 1.999994)
bc_fit[which.min(bc_fit$y)]
# Extract the estimated lambda value
lambda <- BoxCox.lambda(agr_df$y, lower = 0, upper = 1)
plot_df(agr_df)
# Test whether seasonality is additive or multiplicative
if (abs(lambda) < 0.1) {
  print("Seasonality is likely additive")
} else if (abs(lambda - 1) < 0.1) {
  print("Seasonality is likely multiplicative")
} else {
  print("Seasonality is neither additive nor multiplicative")
}


# Plot 
plot_df <- function(df) {
  # get the column names of the data frame
  col_names <- names(df[,-1])
  
  # create a list of ggplot layers, one for each column
  layers <- lapply(col_names, function(col) {
    print(col)
    # check if the column is the second column
    if (col == col_names[1]) {
      geom_bar(aes(x = df[[1]], y = df[[col]], fill = col), stat = "identity", fill=c("#FF8000"), alpha = 0.8)
    } else {
      geom_line(aes(x = df[[1]], y = df[[col]], color = col), size = 2)
    }
  })
  
  # create the ggplot object and add the layers
  p <- ggplot()
  p <- p + layers
  
  # return the plot
  return(p)
}

# plot the data frame using the function
plot_df(df)


isStationary <- function(x) {
  # Perform the ADF test on the time series
  result <- adf.test(x)
  
  # Check the p-value of the test to determine if the time series is stationary
  if (result$p.value < 0.05) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
adf.test(output$seasonality)

plot(output$seasonality)

isStationary(simulate_neg_binom(10,20,360)$values)

makeStationary <- function(x) {
  # Use first-order differencing to remove the linear trend
  result <- diff(x)
  
  # Return the stationary time series
  return(result)
}

# define the function
simulate_ts <- function(n_days, include_seasonality = FALSE, include_trend = FALSE) {
  # simulate daily values from a Poisson distribution
  daily_values <- rpois(n_days, lambda = 10)
  
  # convert daily values to a time series object
  daily_ts <- ts(daily_values, start = c(2020, 1), end = c(2020, n_days), frequency = 1)
  
  # aggregate the daily time series to a monthly level
  monthly_ts <- ts(calculateSum(daily_ts), start = c(2022,1), frequency = 12)
  
  # check if seasonality should be included
  if (include_seasonality) {
    # add the seasonal components to the monthly time series
    monthly_ts <-monthly_ts + mean(monthly_ts)*sin(2 * pi * seq(1,length(monthly_ts)) / 6)
  }
  
  # check if trend should be included
  if (include_trend) {
    # create a trend component
    trend <- mean(monthly_ts/6)*seq(1,length(monthly_ts))
    
    # add the trend component to the monthly time series
    monthly_ts <- monthly_ts + trend
  }
  
  # return the monthly time series
  return(monthly_ts)
}

# Test the function

ts.y <- simulate_ts(360*3, 1)

output <- decompose_ts(ts.y)
df <- data.frame(time = seq(1,length(output$y)), 
                 y = output$y, 
                 seasonality = output$seasonality,
                 trend = output$trend, 
                 remainder = output$remainder)
plot_df(df)
# Load the forecast package
adf.test(output$y)
adf.test(output$trend)
adf.test(output$seasonality)
df

# Define the function
find_distribution <- function(time_series) {
  
  # Specify the distributions to test against
  distributions <- c("normal", "t", "exponential")
  
  # Use the Kolmogorov-Smirnov test to compare the time series to each distribution
  ks_results <- sapply(distributions, function(x) {
    ks.test(time_series, x)
  })
  
  # Return the distribution with the smallest p-value (indicating the best fit)
  return(names(which.min(ks_results$p.value)))
}

# Test the function with a sample time series
poisson_test <- function(time_series, lambda) {
  # Calculate the Poisson probability for each value in the time series
  probabilities <- ppois(time_series, lambda)
  
  # Return the probabilities
  return(probabilities)
}
# Load the required library
library(stats)

# Generate a sample time series
time_series <- rnorm(1000)

# Perform the Shapiro-Wilk test
shapiro.test(output$remainder)

# Load the required libraries
library(ggplot2)
library(ggpubr)

# Generate a sample time series
time_series <- data.frame(time_series = rnorm(1000))

# Create a histogram
hist <- ggplot(data = time_series, aes(x = time_series)) +
  geom_histogram(bins = 50)

# Create a Q-Q plot
qqplot <- ggplot(data = time_series, aes(sample = time_series)) +
  stat_qq()

# Display the histogram and Q-Q plot side by side
ggarrange(hist, qqplot, nrow = 1)


