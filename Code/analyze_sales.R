# Creating dummy variables seasonality & trend

season_trend_df <- function(y){
  n <- length(y)
  x <- seq(1,n)
  season <-  as.factor(((x-1) %% 12)+1)
  df <- data.frame(y = y, Time = x, Month = season)
  return(df)
}

p_threshold <- function(x,p.val = 0.05){
  if(x < p.val){
    return(TRUE)
  }
  return(FALSE)
}

df <- season_trend_df(ts.y)
names(df)
fit <- lm(y~Time+Month, data = df)
fit1 <- lm(y~Time, data = df)
fit2 <- lm(y~Month, data = df)

summary(fit)
summary(fit1)
summary(fit2)
anova(fit, fit1)
anova(fit, fit2)
summary(fit)
season_trend_test <- function(y){
  n <- length(y)
  x <- seq(1,n)
  
  season <-  as.factor(((x-1) %% 12)+1)
  df <- data.frame(y = y, Time = x, Month = season)
  # Fit linear models
  fit <- lm(y~Time+Month, data = df)
  fit0 <- lm(y~Time, data = df) # Drop seasonality
  fit1 <- lm(y~Month, data = df) # Drop Trend

  se_test <- anova(fit0, fit) # Without seasonality
  tr_test <- anova(fit1, fit) # Without trend
  

  p_se <- se_test$`Pr(>F)`[2]
  p_tr <- tr_test$`Pr(>F)`[2]
  
  
  seasonality <- p_threshold(p_se)
  trend <- p_threshold(p_tr)
  
  return(c(seasonality, trend))
}

season_trend_test(agr_df$y)

df <- season_trend_df(agr_df$y)

items <- unique(demo$item_id)
trend <- rep(0, length(items))
for(item in items){
  print(item)
  agr_df <- demo %>% 
    filter(item_id == item)
  if(nrow(agr_df) >= 24){
    trend[item] <- is_trend_regression(agr_df$sale)  
  }
  else {
    trend[item] <- FALSE
  }
}
length(items)
length(trend)
new_df <- data.frame(items, trend)
# Function that checks if time series is stationary or not.

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

# Seasonal test

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

# Test for trend

has_trend <- function(y) {
  # Perform an Augmented Dickey-Fuller test on the time series
  result <- tseries::adf.test(y)
  # Extract the p-value from the test result
  pvalue <- result$p.value
  # If the p-value is less than 0.05, the time series is considered to have a trend
  if (pvalue < 0.05) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

has_trend(agr_df$y)
# BoxCox estimation

estimate_lambda <- function(y) {
  
  # Fit a Box-Cox transformation to the data
  fit <- MASS::boxcox(y ~ 1)
  l.range <- max(abs(fit$x))
  # Extract the lambda parameter from the fit
  lambda <- fit$x[which.max(fit$y)]
  
  if (abs(lambda) < 0.1) {
    print("Seasonality is likely additive") # return 1
  } 
  else if (abs(lambda - l.range) < 0.1) {
    print("Seasonality is likely multiplicative") #return 2
  } 
  else {
    print("No seasonality") #return 0
  }
  
  
  return(lambda)
}


## Function that takes in a time series y. We start by only checking sales in months
## We want it to return information about y, such as distribution, if it has trend or not and seasonality.
# 1. We need at least 2 years of data so that is the first check
# 2. 
#   a) We then perform a test to check if there is seasonality or trend
#   b) If there is seasonality, we can test for additive or multiplicative.
# 3. Decompose the time series into components if needed.
# 4. Somewhere we need to estimate which distribution the data is coming from.

fitdistr(y$y, densfun = "poisson")
fitdistr(y$y, densfun = "normal")
fitdistr(y$y, densfun = "negative binomial")
fitdistr(y$y, densfun = "t")
fitdistr(y$y, densfun = "exponential")
fitdistr(y$y, densfun = "cauchy")

analyse_data <- function(y){
  ns <- length(y)
  # Check the length of a 
  
  if(ns < 24){
    break # or somehow return
  }
  
  # Is y stationary?
  
  test <- season_trend_test(y)
  seasonality <- test[1]
  trend <- test[2]
  return(seasonal)
}
analyse_data(agr_df$y)
estimate_lambda(agr_df$y)

## 