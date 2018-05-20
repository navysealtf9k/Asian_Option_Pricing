#Simulating stock prices using the Euler-Maruyama Method

#Initial Values
s0 <- 100
strike <- 100    #For use in fixed rate option
T <- 1           #Time to Maturity
sigma <- 0.2
r <- 0.05
dt <- 1/252
d <- exp(r*T)
t <- seq(0, T, by = dt)

#Create finite paths
pt <- c(10)  #Vector of paths

path <- function(y) {
  s <- y*10
}

for (b in 1:2){
  k <- path(pt[b])
  pt <- c(pt, k)
}

#Calculate geometric mean
geomean <- function(x) {
  exp(mean(log(x)))  #prod(x)^(1/length(x))
}

#Calculate the mode. 
Mode <- function(x) {
  m <- unique(x)
  kd <- match(x, m)
  sd <- tabulate(kd)
  m[which.max(sd)]
}

#Generate a 20 day/monthly sequence 
mnthly_stock_prices <- c(20)
mnth_seq <- function(x) {
  ds <- x + 20
}

for (v in 2:round((252/20)-1)) {
  ms <- mnth_seq(mnthly_stock_prices[v-1])
  mnthly_stock_prices <- c(mnthly_stock_prices, ms)
}

#Calculating arithmetic monthly payoffs
mnth_op_price <- function(x) {
  wrk <- x
  wrk1 <- sum(wrk[mnthly_stock_prices])/length(mnthly_stock_prices)
}

#Calculating geometric monthly payoffs
geomean2 <- function(x) {
  exp(mnth_op_price(log(x)))
}

#Creating stock price matrices
stock_prices <- matrix(rep(0,6*length(pt)), nrow = length(pt))
colnames(stock_prices) <- c('Max', 'Min', 'Mean', 'Median', 'Mode', 'Standard Deviation')
rownames(stock_prices) <- pt

arithmetic_average_prices <- matrix(rep(0,6*length(pt)), nrow = length(pt))
colnames(arithmetic_average_prices) <- c('Max', 'Min', 'Mean', 'Median', 'Mode', 'Standard Deviation')
rownames(arithmetic_average_prices) <- pt

geometric_average_prices <- matrix(rep(0,6*length(pt)), nrow = length(pt))
colnames(geometric_average_prices) <- c('Max', 'Min', 'Mean', 'Median', 'Mode', 'Standard Deviation')
rownames(geometric_average_prices) <- pt

#Creating fixed option price matrices
arithmetic_fixed_option <- matrix(rep(0,8*length(pt)), nrow = length(pt))
colnames(arithmetic_fixed_option) <- c('Max', 'Min', 'Median', 'Mode', 'Daily Averaged Price', 'Monthly Averaged Price', 'European Option price', 'Standard Deviation')
rownames(arithmetic_fixed_option) <- pt

geometric_fixed_option <- matrix(rep(0,8*length(pt)), nrow = length(pt))
colnames(geometric_fixed_option) <- c('Max', 'Min', 'Median', 'Mode', 'Daily Averaged Price', 'Monthly Averaged Price', 'European Option price', 'Standard Deviation')
rownames(geometric_fixed_option) <- pt

#Creating floating option price matrices
arithmetic_floating_option <- matrix(rep(0,8*length(pt)), nrow = length(pt))
colnames(arithmetic_floating_option) <- c('Max', 'Min', 'Median', 'Mode', 'Daily Averaged Price', 'Monthly Averaged Price', 'European Option price', 'Standard Deviation')
rownames(arithmetic_floating_option) <- pt

geometric_floating_option <- matrix(rep(0,8*length(pt)), nrow = length(pt))
colnames(geometric_floating_option) <- c('Max', 'Min', 'Median', 'Mode', 'Daily Averaged Price', 'Monthly Averaged Price', 'European Option price', 'Standard Deviation')
rownames(geometric_floating_option) <- pt

#Creating European option price matrices
european_call_option_prices <- matrix(rep(0,6*length(pt)), nrow = length(pt))
colnames(european_call_option_prices) <- c('Max', 'Min', 'Mean', 'Median', 'Mode', 'Standard Deviation')
rownames(european_call_option_prices) <- pt

#Price path Simulation
for (x in 1:length(pt)) {
  
  z <- matrix(rep(0,length(t)*pt[x]), nrow = pt[x])
  arithmetic_average <- c()
  geometric_average <- c()
  end_stock_prices <- c()
  
  #Store monthly averaged prices
  arithmetic_average_monthly <- c()
  geometric_average_monthly <- c()
  
  for (i in 1:nrow(z)) {

    sp <- c(s0)
    
    for (u in 2:(length(t))) {
      s_2 <- sp[u-1]*(1 + r*dt + sigma*rnorm(1, mean = 0 , sd = 1)*sqrt(dt))  #(exp((r-(0.5*sigma^2))*dt+sigma*rnorm(1, mean = 0, sd = 1)*sqrt(dt)))
      sp[u] <- c(s_2)
    }
    
     z[i, ] <- sp  
     
     arithmetic_average <- c(arithmetic_average, mean(z[i, ]))
     geometric_average <- c(geometric_average, geomean(z[i, ]))
     end_stock_prices <- c(end_stock_prices, z[i,253])
     arithmetic_average_monthly <- c(arithmetic_average_monthly, mnth_op_price(z[i, ]))
     geometric_average_monthly <- c(geometric_average_monthly, geomean2(z[i, ]))
     
     #Arithmetic sampling payoffs - Daily averaging
     art_flt_payoff_cnt_disc <- (end_stock_prices - arithmetic_average)/d
     art_flt_payoff_cnt_disc[art_flt_payoff_cnt_disc <= 0] <- 0  #Cap payoffs at 0
     art_flt_payoff_dsr_disc <- (end_stock_prices - arithmetic_average)/(1+r)
     art_flt_payoff_dsr_disc[art_flt_payoff_dsr_disc <= 0] <- 0
     art_fxd_payoff_cnt_disc <- (arithmetic_average-100)/d
     art_fxd_payoff_cnt_disc[art_fxd_payoff_cnt_disc <= 0] <- 0
     art_fxd_payoff_dsr_disc <- (arithmetic_average-100)/(1+r)
     art_fxd_payoff_dsr_disc[art_fxd_payoff_dsr_disc <= 0] <- 0
     
     #Geometric sampling payoffs - Daily averaging
     geo_flt_payoff_cnt_disc <- (end_stock_prices - geometric_average)/d 
     geo_flt_payoff_cnt_disc[geo_flt_payoff_cnt_disc <= 0] <- 0
     geo_flt_payoff_dsr_disc <- (end_stock_prices - geometric_average)/(1+r)
     geo_flt_payoff_dsr_disc[geo_flt_payoff_dsr_disc <= 0] <- 0
     geo_fxd_payoff_cnt_disc <- (geometric_average - 100)/d
     geo_fxd_payoff_cnt_disc[geo_fxd_payoff_cnt_disc <= 0] <- 0
     geo_fxd_payoff_dsr_disc <- (geometric_average - 100)/(1+r)
     geo_fxd_payoff_dsr_disc[geo_fxd_payoff_dsr_disc <= 0] <- 0
     
     #Arithmetic sampling payoffs - Monthly averaging
     art_flt_payoff_cnt_disc_mnth <- (end_stock_prices - arithmetic_average_monthly)/d
     art_flt_payoff_cnt_disc_mnth[art_flt_payoff_cnt_disc_mnth <= 0] <- 0
     art_flt_payoff_dsr_disc_mnth <- (end_stock_prices - arithmetic_average_monthly)/(1+r)
     art_flt_payoff_dsr_disc_mnth[art_flt_payoff_dsr_disc_mnth <= 0] <- 0
     art_fxd_payoff_cnt_disc_mnth <- (arithmetic_average_monthly-100)/d
     art_fxd_payoff_cnt_disc_mnth[art_fxd_payoff_cnt_disc_mnth <= 0] <- 0
     art_fxd_payoff_dsr_disc_mnth <- (arithmetic_average_monthly-100)/(1+r)
     art_fxd_payoff_dsr_disc_mnth[art_fxd_payoff_dsr_disc_mnth <= 0] <- 0
     
     #Geometric sampling payoffs - Monthly averaging
     geo_flt_payoff_cnt_disc_mnth <- (end_stock_prices - geometric_average_monthly)/d 
     geo_flt_payoff_cnt_disc_mnth[geo_flt_payoff_cnt_disc_mnth <= 0] <- 0
     geo_flt_payoff_dsr_disc_mnth <- (end_stock_prices - geometric_average_monthly)/(1+r)
     geo_flt_payoff_dsr_disc_mnth[geo_flt_payoff_dsr_disc_mnth <= 0] <- 0
     geo_fxd_payoff_cnt_disc_mnth <- (geometric_average_monthly - 100)/d
     geo_fxd_payoff_cnt_disc_mnth[geo_fxd_payoff_cnt_disc_mnth <= 0] <- 0
     geo_fxd_payoff_dsr_disc_mnth <- (geometric_average_monthly - 100)/(1+r)
     geo_fxd_payoff_dsr_disc_mnth[geo_fxd_payoff_dsr_disc_mnth <= 0] <- 0
     
     #European option payoffs - For comparison
     european_option_price <- (end_stock_prices - 100)/(1+r)
     european_option_price[european_option_price <= 0] <- 0
     
  }
  
  # Filling stock price matrices
  temp <- c(max(z), min(z), mean(z), median(z), Mode(z), sd(z))
  print(x)      # Use this to check if for loop is at the right value of i 
  stock_prices[x,] <- temp
  
  temp_0 <- c(max(arithmetic_average), min(arithmetic_average), mean(arithmetic_average), median(arithmetic_average), Mode(arithmetic_average), sd(arithmetic_average))
  arithmetic_average_prices[x,] <- temp_0
  
  temp_1 <- c(max(geometric_average), min(geometric_average), mean(geometric_average), median(geometric_average), Mode(geometric_average), sd(geometric_average))
  geometric_average_prices[x,] <- temp_1
  
  #Filling option price matrices
  temp_2 <- c(max(art_flt_payoff_dsr_disc), min(art_flt_payoff_dsr_disc), median(art_flt_payoff_dsr_disc), Mode(art_flt_payoff_dsr_disc), mean(art_flt_payoff_dsr_disc), mean(art_flt_payoff_dsr_disc_mnth), mean(european_option_price), sd(art_flt_payoff_dsr_disc))
  arithmetic_floating_option[x, ] <- temp_2
  temp_3 <- c(max(art_fxd_payoff_dsr_disc), min(art_fxd_payoff_dsr_disc), median(art_fxd_payoff_dsr_disc), Mode(art_fxd_payoff_dsr_disc), mean(art_fxd_payoff_dsr_disc), mean(art_fxd_payoff_dsr_disc_mnth), mean(european_option_price), sd(art_fxd_payoff_dsr_disc))
  arithmetic_fixed_option[x, ] <- temp_3
  temp_4 <- c(max(geo_flt_payoff_dsr_disc), min(geo_flt_payoff_dsr_disc), median(geo_flt_payoff_dsr_disc), Mode(geo_flt_payoff_dsr_disc), mean(geo_flt_payoff_dsr_disc), mean(geo_flt_payoff_dsr_disc_mnth), mean(european_option_price), sd(geo_flt_payoff_dsr_disc))
  geometric_floating_option[x, ] <- temp_4
  temp_5 <- c(max(geo_fxd_payoff_dsr_disc), min(geo_fxd_payoff_dsr_disc), median(geo_fxd_payoff_dsr_disc), Mode(geo_fxd_payoff_dsr_disc), mean(geo_fxd_payoff_dsr_disc), mean(geo_fxd_payoff_dsr_disc_mnth), mean(european_option_price), sd(geo_fxd_payoff_dsr_disc))
  geometric_fixed_option[x, ] <- temp_5
  temp_6 <- c(max(european_option_price), min(european_option_price), median(european_option_price), Mode(european_option_price), mean(european_option_price), sd(european_option_price))
  european_call_option_prices[x, ] <- temp_6

  #Plot stock price simulations
  plot(t, z[1, ], type = 'l', main = paste('Simulation of',pt[x],'paths'),ylim = c(min(z), max(z)), ylab = 'Price', xlab = 'Time')

  for (i in 2:nrow(z)) {
    lines(t, z[i, ], type = 'l', col=i)
  }

  #Plot Payoff distributions
  hist(art_flt_payoff_dsr_disc, main = paste('Payoffs of',pt[x],'aritmetic simulated paths'), xlab = 'Payoff', col = 'white')
  hist(geo_flt_payoff_dsr_disc, main = paste('Payoffs of',pt[x],'geometric simulated paths'), xlab = 'Payoff', col = 'red')
  hist(european_option_price, main = paste('Payoffs of',pt[x],'European call options'), xlab = 'Payoff', col = 'green')
}

hist(z, main = 'Stock price simulation', xlab = 'prices')
