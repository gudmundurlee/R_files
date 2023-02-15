library(forecast)
library(ggplot2)
library(gridExtra)
library(dplyr)  
library(purrr)
library(reshape2)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(reshape2)
library(knitr)
## Demand Function ##


lost_sale <- TRUE # Global variable
min_stock <- 0 # Gobal variable
kappa <- 0.1

# Instead of a for loop we can use the following:
demand <- function(df, i, n, ss, lt, of, lost_sale = TRUE) {
  #demand <- function(df, i, n, ...) {  
  # df has to have names: "demand_a","demand_p","est_stock","acc_est_stock","delivered","is_order","is_delivery" 
  ii <- min(i+1,n)
  t_i <- min(i+lt+of, n)
  of_i <- min(i+of, n)
  lt_i <- min(i+lt, n)
  op_i <- min(i+(lt + of), n)
  if(i == 2){
    df$net_demand[1] <- df$est_stock[1] - ss - min_stock - ( sum(df$demand_p[2:(lt+of)]) - sum(df$delivered[2:(lt+of)]))
    df$demand_next_op[1] <- sum(df$demand_p[2:(lt+of)])
    
    if( ( df$est_stock[1] - ss - min_stock <  ( sum(df$demand_p[2:op_i]) - sum(df$delivered[2:op_i])) )  & ( df$is_order[1] == 1 )  )  {  
      #if( ( df$est_stock[i] <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {  
      
      #df$delivered[lt_i] <- max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]) - df$est_stock[i] + ss),0)
      df$delivered[lt_i-1] <- max(ceiling(sum(df$demand_p[2:(lt+of)]) - sum(df$delivered[2:(lt+of)]) - df$est_stock[1] + ss),0)
    }
    df$incoming[1] <- sum(df$delivered[lt_i-1])
  }
  if(i <= n){
    df$acc_est_stock[i] <- df$est_stock[i-1]- df$demand_a[i] + df$delivered[i]
    # Lost sale: 
    
    df$est_stock[i] <- df$est_stock[i-1]- df$demand_a[i] + df$delivered[i]
    
    if(lost_sale == TRUE){
      df$est_stock[i] <-max(df$est_stock[i],0)     
    }
    
    df$net_demand[i] <- df$est_stock[i] - ss - min_stock - ( sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))
    df$demand_next_op[i] <- sum(df$demand_p[ii:op_i])
    #Using Standard: 245.0000[stock] - 740.8296[demand] + 736.0000[undeliv. arrived] - 184.3050[safety stock] - 0.0000[min stock] = 55.8654[result] is greater than 0 so calculated order qty is 0. Order qty: 0
    df$incoming[i] <- sum(df$delivered[ii:lt_i])
    if( ( df$est_stock[i] - ss - min_stock <  ( sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i])) )  & ( df$is_order[i] == 1 )  )  {  
    #if( ( df$est_stock[i] - ss - min_stock <  ( sum(df$demand_p[ii:op_i]) - sum(df$delivered[of_i:op_i])) )  & ( df$is_order[i] == 1 )  )  {  
      #if( ( df$est_stock[i] <  (sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]))  & ( df$is_order[i] == 1 ) ) )  {  
      
      df$delivered[lt_i] <- max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[ii:op_i]) - df$est_stock[i] + ss),0)
      #df$delivered[lt_i-1] <- df$delivered[lt_i-1] + max(ceiling(sum(df$demand_p[ii:op_i]) - sum(df$delivered[of_i:op_i]) - df$est_stock[i] + ss),0)
      
    }
    
    return(demand(df, i+1, n, ss, lt, of, lost_sale = lost_sale))
  }
  else {
    filter <- which(df$is_delivery == 1)[1]
    
    return(df[(filter:nrow(df)),])
  }
}



## Helper Function - Gen Demand Data ##

gen_demand_data <- function(demand_a, demand_p, ss, st, of, lt){
  # Demand_p: Demand predicted
  # Demand_a: Demand actual.
  
  n <- length(demand_p)
  days <- seq(1,n,1)
  
  delivered <- rep(0,n)
  
  est_stock <- rep(0,n)
  est_stock[1] <- max(st - demand_p[1],0) #sum(demand_p[1:lt]) + st
  # Initalize 
  
  is_order <- rep(0,n) 
  is_delivery <- rep(0,n)
  
  order_days <- days[days %% of == 0] - (of - 1) #(seq(1,floor(n/of))*of) - (of - 1)
  #delivered_days <- days[days %% (of+lt) == 0] - (of -1) #(seq(1,floor(n/of))*of) - (of-1) + lt
  delivered_days <- days[days %% (lt) == 0]
  #If it is a delivery or order
  is_order[order_days] <- 1
  is_delivery[delivered_days] <- 1
  # Service Level
  sl <- rep(1,n)
  
  op <- min((lt+of),1)
  
  # initalize all values
  est_stock[1] <- est_stock[1] - demand_a[1]
  acc_est_stock <- est_stock
  # If the estimated stock does not cover the OP then order the demand for the LT.
  if( est_stock[1] - ss  < sum(demand_p[2:op]) - sum(delivered[2:op]) ){
    delivered[lt] <- max(ceiling((demand_p[2:op]) - est_stock[1] + ss),0) #ceiling(ss) #ceiling(lt*(lambda) + ss)
  }
  
  net_demand <- rep(0,n)
  demand_next_op <- rep(0,n)
  incoming <- rep(0,n)
  
  net_demand[1] <- est_stock[1] - ss  - sum(demand_p[2:op]) + sum(delivered[2:op]) 
  
  demand_next_op[1] <- sum(demand_p[2:op])
  ss_op <- rep(ss,n)
  data <- data.frame(demand_a = demand_a, demand_p = demand_p, est_stock, acc_est_stock, delivered, is_order, is_delivery, net_demand, demand_next_op, ss_op = ss_op, incoming = incoming)
  
  return(data)
}

# Calculate the service level with cf.

cf_sl <- function(demand_a,demand_p, ss, st, lt, of, lost_sale = TRUE){
  n <- length(demand_a)
  df <- gen_demand_data(demand_a, demand_p, ss,st, of = of, lt = lt)
  
  output <- demand(df,2, n, ss = ss, lt = lt, of = of, lost_sale = lost_sale )
  output$ss_ratio <- sapply(output$est_stock, FUN = function(x){ss_ratio(ss = ss,st = x)})
  
  sl <- sapply(output$est_stock, FUN = function(x){if(x > 0) return(1) else return(0)})
  output$sl <- sl
  return(output)
  #return(list(ServiceLevel = sl, output$est_stock))
}

ts_generate <- function(yr, plot = FALSE){
  d <- 360 # days
  s <- round(runif(1)) # seasonality
  t <- round(runif(1)) # trend
  l <- runif(1,1,100) # Monthly lambda
  c <- round(runif(1))
  s.l <- runif(1)*(l/30)
  t.l <- runif(1)*(l/30)
  y <- rpois(d*yr, l/30) + s*s.l*rep(sin(pi*seq(0,1,length.out = d)), yr) + (1-s)*s.l*rep(rep(round(1),d),yr)+ t*t.l*seq(1,d*yr)/30^2
  #if(c == 1){
  y <- y*round(runif(d*yr,0,1))  
  #}
  
  if(plot == TRUE){
    
    ym <- unname(tapply(y, (seq_along(y)-1) %/% 30, sum)) 
    l.n <- length(ym)
    
    months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec")
    title <- sprintf("l = %#.2f, s = %#.2f and t = %#.2f", round(l,2), round(s*s.l,2), round(t*t.l,2))
    #print(title)
    df_plot <- data.frame(Y = ym, Time = seq(1,l.n), Months = rep(months,yr), Year = seq(1,yr), Labels = rep("Simulation", l.n))
    fill <- c(rep("Train", (yr-1)*12),rep("Test", 12))
    df_plot$Labels <- fill
    plot <- ggplot() +
      geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge") +
      theme_ipsum() +
      labs(title = title)
    print(plot)
  }
  return(y)
}

get_time_series <- function(year, kappa = 0.1){
  if(runif(1) < kappa){
    y <- random_ts(year = year)  
  }
  else {
    y <- ts_generate(yr = year)
  }
  return(y)
}

## Helper functions ##
## Random TS ##
random_ts <- function(years, lambda = 10, plot = FALSE){
  n <- years*360
  w <- n/30
  lambda <- runif(1,1,100)
  # Uniform lambda values
  (lambdas <- runif(w,0,lambda))
  
  (y_mat <- sapply(lambdas, FUN = function(x){rpois(30,x)}))
  
  y_vec <- c(matrix(y_mat,nrow = 1, byrow = TRUE))
  
  ym_vec <- unname(tapply(y_vec, (seq_along(y_vec)-1) %/% 30, sum))  
  
  if(plot == TRUE){
    
    years <- length(ym_vec)/12
    
    months <- c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec")
    
    df_plot <- data.frame(Y = ym_vec, Time = seq(1,length(ym_vec)), Months = rep(months,years), Year = seq(1,years), Labels = rep("Simulation", length(ym_vec)))
    fill <- c(rep("Train", (years-1)*12),rep("Test", 12))
    df_plot$Labels <- fill
    plot <- ggplot() +
      geom_bar(data = df_plot, aes(Time, Y , fill = Labels),width=.5, stat="identity", position="dodge") +
      theme_ipsum()
    print(plot)
  }
  return(y_vec)
}

# Fit models
fit_model <- function(y, h){
  fit <- auto.arima(y)
  f <- forecast::forecast(fit, h = h)
  f_v <- f$mean
  f_sigma2 <- f$model$sigma2
  return(list(pred = f_v, sigma2 = f_sigma2, fit = fit))
}

# New safetystock calculations

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


## Preparation ##
cf_data <- function(year, lambda, h, lt, of, lost_sale = TRUE){
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
  cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  # Initalize saftey stock
  
  #ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
  ss <- sqrt(sigma2/30)*qnorm(z_score)*safety_stock_calc(of+lt)
  st <- sum(demand_p[1:lt])
  
  
  output <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of, lost_sale  = lost_sale)})
  return(output)
}

ts_cv <- function(y, year, of = 90, lt = 90){
  f.years <- 2
  folds <- year-f.years
  d <- 360
  period <- 30
  
  #cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  cf <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.82,0.84,0.86,0.88,0.9,0.92,0.94,0.96,0.98,0.99)
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
    #lt <- 120#180
    #of <- 30 #180
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


run_sim <- function(year){
  y <- ts_generate(yr = year, plot = F)
  sim <- ts_cv(y,year)
  return(sim)
}
year <- 12
y <- ts_generate(yr = year, plot = T)
sim1 <- ts_cv(y,year, of = 120, lt = 30)
sim2 <- ts_cv(y,year, of = 30, lt = 150)
sim1
sim2

mean(sim1)
mean(sim2)


N <- 250 #500
#Y <- apply(rep(year,10), 1, function(x){ts_generate(yr = x)})

Y <- sapply(rep(year,N), FUN = function(x){ts_generate(yr = x)})

sim.Y <- apply(Y, 2, FUN = function(x){ts_cv(x, year = year, of = 30, lt = 150)})
colMeans(t(sim.Y))

cfs
zeats

result <- data.frame(CF = zeats, SL = colMeans(t(sim.Y)))
result
dim(Y)
apply(Y,2,max)
write.csv2(result, "~/AGR/R_Projects/Service_level/result.csv", row.names=FALSE)

round(colMeans(t(sim.Y)),3)
apply(t(sim.Y), 2, median)




#list_output <- lapply(rep(10,500), FUN = function(x){run_sim(year = x)})  
#sl.df <- as.data.frame(do.call(rbind, list_output))
#sl.df
#names(sl.df) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
#sl.df
#kable(t(apply(sl.df, 2, median)))
#kable(apply(sl.df, 2, mean))

z_score_to_cf <- function(z_score){
  alpha <- 2-2*z_score 
  cf <- 1-alpha
  return(cf)
}


# R
main <- function(){
  cf <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.82,0.84,0.86,0.88,0.9,0.92,0.94,0.96,0.98,0.99)
  zeats <- 1-(1-cf)/2
  year <- 10
  N <- 250#100
  #Y <- sapply(rep(year,N), FUN = function(x){ts_generate(yr = x)})
  Y <- sapply(rep(year,N), FUN = function(x){get_time_series(year = x, kappa = 0.5)})
  sim.Y <- apply(Y, 2, FUN = function(x){ts_cv(x, year = year, of = 30, lt = 150)})
  means <- colMeans(t(sim.Y))
  meds <- apply(t(sim.Y), 2, median)
  kable(cbind(zeats,round(means,3)))
  kable(cbind(zeats,round(meds,3)))
  
}

kappa <- 0.2

one <- ss_sl_sim_repeat(12, lambda = 10, h = 24, lt = 150, of = 30, N = 100)
colMeans(one$ServiceLevel)



## I need to understand what is going on with the demand calculations
debug <- function(year, lambda, h, lt, of, cf = 0.9, lost_sale = TRUE){
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
  #cf <- c(0,0.2,0.4,0.6,0.8,0.9,0.99)
  alpha <- 1-cf
  z_score <- 1-alpha/2
  
  # Initalize saftey stock
  
  #ss <- sqrt(sigma2)*qnorm(z_score)*sqrt(of+lt)
  ss <- (sigma/30)*qnorm(z_score)*safety_stock_calc(lt+of)
  
  st <- sum(demand_p[1:lt]) + ss
  
  print(sprintf("safety stock for order period is: %f and variation of model fit is %f", ss, sigma))
  #output <- sapply(ss, FUN = function(x){cf_sl(demand_a = demand_a, demand_p, ss = x, st = st, lt = lt, of = of, lost_sale  = lost_sale)})
  output <- cf_sl(demand_a = demand_a, demand_p = demand_p, ss = ss, st = st, lt = lt, of = of, lost_sale = lost_sale)
  return(output)
}


db <- debug(year = 6, lambda = 10, h = 12, lt =  150, of = 30, cf = 0)
View(db)
names(db)

db$time <- seq(1,nrow(db))

df_agg <- aggregate(. ~ cut(seq(nrow(db)), 30), db, sum)
df_agg$time <- seq(1,nrow(df_agg))
names(db)
#df <- data.frame(time = 1:10, col1 = 1:10, col2 = 10:1, col3 = runif(10), col4 = rnorm(10))
ggplot(db, aes(x = time)) +
  geom_bar(aes(y = demand_a), stat = "identity", fill = "blue", width = 0.4) +
  geom_bar(aes(y = demand_p), stat = "identity", fill = "red", width = 0.4,
           position = position_dodge(width = 0.4)) +
  geom_area(aes(y = est_stock), alpha = 0.3, fill = "green") +
  geom_line(aes(y = delivered), color = "purple")


head(db)
write.csv2(db, "~/AGR/R_Projects/Service_level/debug.csv", row.names=FALSE)
## Safety stock ratio
df <- data.frame(matrix(debug, ncol = 7, byrow = TRUE))


ss_ratio <- function(ss, st){
  # If saftey stock is 0 then we do not need anything
  if(ss == 0){
    return(0)
  }
  if(ss >= st){
    return(1)
  }
  else {
    return(ss/st)
  }
}

lt_of <- function(order_period){
  if(order_period == 1){
    min <- 1
    max <- 7
  }
  else if(order_period == 2){
    min <- 7
    max <- 14
  }
  else if(order_period == 3){
    min <- 14
    max <- 30 
  }
  my_sample <- cumsum(runif(2))
  my_sample <- c(0, my_sample/max(my_sample))
  lt_of_sim <- round(diff(my_sample) * runif(1, min, (max-1))) + 1
  return(lt_of_sim)
}


ss_sl_sim <- function(year,lambda, h, lt, of, lost_sale = TRUE){
  sim <- cf_data(year = year,lambda = lambda, h = h, lt = lt, of = of, lost_sale = lost_sale)
  tt <- ncol(sim)
  SS <- sapply(1:tt, FUN= function(x){mean(sim[,x]$ss_ratio)})
  SL <- sapply(1:tt, FUN= function(x){mean(sim[,x]$sl)})
  #return(cbind(SS,SL))
  return(list(SS = SS, SL = SL))
}



# Random order period
## order_period = c("short", "medium", "long")
ss_sl_sim_OP <- function(year,lambda, h, order_period = 1, lost_sale = TRUE){
  OP <- lt_of(order_period)
  lt <- OP[1]
  of <- OP[2]
  sim <- cf_data(year,lambda, h, lt, of, lost_sale = lost_sale)
  tt <- ncol(sim)
  SS <- sapply(1:tt, FUN= function(x){mean(sim[,x]$ss_ratio)})
  SL <- sapply(1:tt, FUN= function(x){mean(sim[,x]$sl)})
  #return(cbind(SS,SL))
  return(list(SS = SS, SL = SL))
}

#ss_sl_sim(4, 10, 12, 7,7)

#test_output <- lapply(rep(4,10), FUN = function(x){ss_sl_sim(x,10,12,7,7)})
#test_output
#x.length <- length(test_output)

#ss_mat <- t(sapply(1:x.length, FUN =function(x){test_output[[x]][["SS"]]}))
#sl_mat <- t(sapply(1:x.length, FUN =function(x){test_output[[x]][["SL"]]}))
#colMeans(sl_mat)
#sl_mat

ss_sl_sim_repeat <- function(year, lambda, h, lt, of, N, lost_sale = TRUE){
  list_output <- lapply(rep(4,N), FUN = function(x){ss_sl_sim(x,lambda = lambda, h = h,lt = lt, of = of, lost_sale = lost_sale)})  
  ll <- length(list_output) #Should be the same length as N
  ss_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SS"]]})))
  sl_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SL"]]})))
  names(ss_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  names(sl_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  return(list(SafteyStock = ss_mat, ServiceLevel = sl_mat))
}

ss_sl_sim_repeat_OP <- function(year, lambda, h,  order_period = 1, N = 10, lost_sale = TRUE){
  list_output <- lapply(rep(4,N), FUN = function(x){ss_sl_sim_OP(x,lambda,h,order_period, lost_sale)})  
  ll <- length(list_output) #Should be the same length as N
  ss_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SS"]]})))
  sl_mat <- as.data.frame(t(sapply(1:ll, FUN =function(x){list_output[[x]][["SL"]]})))
  names(ss_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  names(sl_mat) <- c("50%","60%", "70%", "80%", "90%", "95%", "99.5%")
  return(list(SafteyStock = ss_mat, ServiceLevel = sl_mat))
}
