s0 <- 100
strike <- 100    #For use in fixed rate option
T <- 1           #Time to Maturity
sigma <- 0.2
r <- 0.05
dt <- 1/252
d <- #exp(-r*T)
  t <- seq(0, T, by = dt)
pt <- c(10)#,100,10000,100000, 1000000)

sp <- c(s0)
for (u in 2:(length(t))) {
  s_2 <- sp[u-1]*(1 + r*dt + sigma*rnorm(1, mean = 0, sd = 1)*sqrt(dt))
  sp[u] <- c(s_2)
}