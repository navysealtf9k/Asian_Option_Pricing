stock_price <- 115
cash <-100
profit <- stock_price - cash
x_axis <- stock_price
y_axis <- profit

while (profit > 0) {
  stock_price <- stock_price+(1*rnorm(1, mean = 0, sd = 1))
  profit <- stock_price - cash
  x_axis <- c(x_axis, stock_price)
  y_axis <- c(y_axis, cash)
  plot(x_axis, y_axis, ylab = 'profit', xlab = 'stock_price')
  
  if (profit > 10) {
    print(paste('We have made', profit, 'at', stock_price))
  }
  else {
    print(paste('You have a loss of ', profit))
  }
}

