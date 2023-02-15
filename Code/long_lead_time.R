
library(ggplot2)
library(gridExtra)
library(forecast)


safety_stock_calc <- function(OP){
  n <- OP  
  (m <- 0:ceiling(OP/30))
  
  (m.sqrt <- sqrt(m))
  
  (m1 <- m.sqrt[-1])
  (m2 <- m.sqrt[-length(m.sqrt)])

  #scaler <- unname(na.omit(m.sqrt-lag(m.sqrt)))
  scaler <- m1-m2
  
  sequence_length <- 30
  w <- rep(seq(from = 1, by = 1, length.out = ceiling(n/sequence_length)),
           each = sequence_length, length.out = n)
  counts <- sapply(unique(w), function(x) sum(w == x))
  return(sum(counts*scaler))
}
run_output <- list()

ts_cv <- function(y, year){
  f.years <- 2
  folds <- year-f.years
  d <- 360
  period <- 30
  
  cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  sl.mat <- matrix( nrow = folds, ncol = length(cf))
  for(i in 1:folds){
    train_split <- c(1:(d*i))
    test_split <- c(1:(d*f.years)) + (d*i)
    
    y_train <- y[train_split]
    y_test <- y[test_split]
    y_train_m <- unname(tapply(y_train, (seq_along(y_train)-1) %/% 30, sum))
    
    model <- fit_model(y_train_m, h = 12*f.years)
    #Extract features
    
    demand_p.m <- model$pred
    #sigma2 <- model$sigma2/30^2
    sigma <- sqrt(model$sigma2)
    demand_a <- y_test# Transform data to daily
    demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
    

    #print(range(train_split))
    #print(range(test_split))
    # Initalize saftey stock
    lt <- 150
    of <- 30
    #ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)

    #print(sigma)
    ss <- (sigma/30)*qnorm(z_score)*safety_stock_calc(lt + of)
    #print(sigma*qnorm(z_score)**safety_stock_calc(lt + of))
    st <- sum(demand_p[1:lt]) + ss
    #print(ss)
    #print(ss)
    sim <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of, lost_sale  = T)})
    tt <- ncol(sim)
    (SL <- sapply(1:tt, FUN= function(x){mean(sim[,x]$sl)}))
    #run_output <<- append(run_output, sim)
    sl.mat[i,] <- SL
  }
  return(colMeans(sl.mat))
}

sqrt(180/12)
safety_stock_calc(180)
## Plot
plot_cv <- function(y, year){
  
  d <- 360
  period <- 30
  
  f.years <- 2
  folds <- year-f.years
  
  cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  sl.mat <- matrix( nrow = folds, ncol = length(cf))
  plots <- list()
  
  
  for(i in 1:folds){
    train_split <- c(1:(d*i))
    test_split <- c(1:(d*f.years)) + (d*i)
    
    y_train <- y[train_split]
    y_test <- y[test_split]
    y_train_m <- unname(tapply(y_train, (seq_along(y_train)-1) %/% 30, sum))
    y_test_m <- unname(tapply(y_test, (seq_along(y_test)-1) %/% 30, sum))
    
    model <- fit_model(y_train_m, h = 12*f.years)
    #Extract features
    
    demand_p.m <- model$pred
    #sigma2 <- model$sigma2/30^2
    sigma2 <- model$sigma2/30
    demand_a <- y_test# Transform data to daily
    demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
    

    
    # Create separate data frames for the train and test vectors
    train_df <- data.frame(time = 1:length(y_train_m), value = y_train_m, type = "Train", forecast = (rep(NA, length(y_train_m))))
    test_df <- data.frame(time = length(y_train_m)+(1:length(y_test_m)), value = y_test_m, type = "Test",forecast=demand_p.m)
    
    # Combine the train and test data frames into a single data frame
    df <- rbind(train_df, test_df)
    
    # Plot the train and test vectors as columns and the forecast vector as a line
    p <- ggplot(df, aes(x = time)) + 
      geom_col(aes(y = value, fill = type), position = "dodge") +
      geom_line(aes(y = forecast, color = "Forecast")) +
      scale_fill_manual(values = c("Train" = "blue", "Test" = "red")) +
      scale_color_manual(values = c("Forecast" = "green")) +
      labs(x = "Time", y = "Values", fill = "", color = "")
    
    plots[[i]] <- p
  }
  return(plots)
}
year <- 12
y <- ts_generate(yr = year, plot = F)
sim <- ts_cv(y,year)
sim
test <- run_output[[1]]
run_output[[2]]
run_output[[3]]


one_run <- function(year, lambda, h, lt, of, lost_sale = TRUE){
  period <- 30
  days <- year*360
  # get y
  if(runif(1) < kappa){
    y <- random_ts(year,lambda)  
  }
  else {
    y <- ts_generate(yr = year)
  }
  
  y_train <- y[1:(days-h*period)]
  y_test <- y[(days-h*period+1):days]
  # Fit model
  y_train <- unname(tapply(y_train, (seq_along(y_train)-1) %/% period, sum))  
  
  # extract data
  model <- fit_model(y_train, h)
  #Extract features
  
  demand_p.m <- model$pred
  sigma2 <- model$sigma2/30^2
  
  sigma <- sqrt(model$sigma2)
  
  
  demand_a <- y_test# Transform data to daily
  demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
  
  
  # Fix some values of the order
  #cf <- c(0,0.5,0.6,0.7,0.8,0.9,0.95,0.99)
  cf <- 0.9#c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  # Initalize saftey stock
  
  ss <- sqrt(sigma/30)*qnorm(z_score)*safety_stock_calc(lt+of)
  st <- sum(demand_p[1:lt]) + ss
  
  
  output <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of, lost_sale  = lost_sale)})
  return(output)
}

one <- one_run(6, 10, 12, 150, 30)
#one[[1]]
df.one <- do.call(cbind, one)
head(df.one)
mean(df.one[,9])
#sim <- ts_cv_gpt(y,year)

run_sim <- function(year){
  y <- ts_generate(yr = year, plot = F)
  sim <- ts_cv(y,year)
  return(sim)
}
run_sim(12)


plots <- plot_cv(y, year)
grid.arrange(grobs = plots, ncol = 3)
year <- 11
y <- ts_generate(year, plot = T)

plots <- plot_cv(y, year)
grid.arrange(grobs = plots, ncol = 3)
###
year <- 12
N <- 100
#Y <- apply(rep(year,10), 1, function(x){ts_generate(yr = x)})

Y <- sapply(rep(year,N), FUN = function(x){ts_generate(yr = x)})

sim.Y <- apply(Y, 2, FUN = function(x){ts_cv(x, year = year)})
colMeans(t(sim.Y))
apply(t(sim.Y), 2, median)
list_output <- lapply(rep(10,500), FUN = function(x){run_sim(year = x)})  
sl.df <- as.data.frame(do.call(rbind, list_output))
sl.df
names(sl.df) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
sl.df
kable(t(apply(sl.df, 2, median)))
kable(apply(sl.df, 2, mean))



ts_cv_gpt <- function(y, year){
  f.years <- 2
  folds <- year-f.years
  d <- 360
  period <- 30
  
  cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  sl.mat <- matrix( nrow = folds, ncol = length(cf))
  
  # Create a list of train and test split indices
  splits <- lapply(1:folds, function(i) {
    train_split <- c(1:(d*i))
    test_split <- c(1:(d*f.years)) + (d*i)
    return(list(train_split = train_split, test_split = test_split))
  })
  
  # Use lapply to process each split
  sl_list <- lapply(splits, function(split) {
    y_train <- y[split$train_split]
    y_test <- y[split$test_split]
    y_train_m <- unname(tapply(y_train, (seq_along(y_train)-1) %/% 30, sum))
    
    model <- fit_model(y_train_m, h = 12*f.years)
    #Extract features
    
    demand_p.m <- model$pred
    #sigma2 <- model$sigma2/30^2
    sigma2 <- model$sigma2/30
    demand_a <- y_test# Transform data to daily
    demand_p <- demand_p.m[rep(seq_along(demand_p.m), each = period)]/period
    
    
    #print(range(train_split))
    #print(range(test_split))
    # Initalize saftey stock
    lt <- 90
    of <- 90
    ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
    st <- sum(demand_p[1:lt])
    print(ss)
    sim <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of, lost_sale  = T)})
    tt <- ncol(sim)
    (SL <- sapply(1:tt, FUN= function(x){mean(sim[,x]$sl)}))
    return(SL)
  })
  
  # Return the mean of all SL values
  return(colMeans(do.call(rbind,sl_list)))
}

tscv <- function(data, h = 12, fh = 12, 
                 train_frac = 2/3, seed = 1) {
  
  set.seed(seed)
  
  # Determine the number of observations
  n <- length(data)
  
  # Initialize a list to store the forecast errors
  errors <- list()
  
  # Perform cross-validation
  for (i in 1:(n - (train_frac * n + h))) {
    
    # Split the data into training and test sets
    train <- data[i:(i + train_frac * n)]
    test <- data[(i + train_frac * n):(i + train_frac * n + h - 1)]
    
    # Fit the model to the training set
    model <- forecast(train, h = fh)
    
    # Make forecasts for the test set
    forecast <- model$mean
    
    # Calculate the forecast error
    error <- test - forecast
    errors[[i]] <- error
  }
  
  # Return the forecast errors
  return(errors)
}
