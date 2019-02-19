# Based on http://r-statistics.co/Loess-Regression-With-R.html

library(dplyr)
library(ggplot2)
library(profvis)

data(economics, package="ggplot2")

l = loess(uempmed ~ as.integer(date), data=economics, span=0.1)

modelr::add_predictions(economics, l) %>%
  ggplot(aes(x=date, y=uempmed)) +
    geom_line() +
    geom_line(aes(y=pred), col='red')

system.time({l = loess(uempmed ~ as.integer(date), data=economics, span=0.1)})

profvis(
  {
    l = loess(uempmed ~ as.integer(date), data=economics, span=0.01)
  },
  interval = 0.001
)

calc_sse = function(span) {
  sse = NA
  
  l = purrr::safely(loess)(
    uempmed ~ as.integer(date), 
    data=economics, span=span
  )
  
  if (!is.null(l$result)) {
    res = l$result$residuals
    sse = sum(res^2)
  }
  
  sse
}

optim(par=c(0.5), calc_sse, method="SANN", control = list(maxit=5000))

profvis(
  {
    optim(par=c(0.5), calc_sse, method="SANN", control = list(maxit=1000))
  },
  interval = 0.005
)

simpleLoess
?simpleLoess
??simpleLoess

stats:::simpleLoess

