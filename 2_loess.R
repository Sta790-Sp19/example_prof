# Based on http://r-statistics.co/Loess-Regression-With-R.html

library(dplyr)
library(ggplot2)

data(economics, package="ggplot2")

l = loess(uempmed ~ as.integer(date), data=economics, span=0.10)

modelr::add_predictions(economics, l) %>%
  ggplot(aes(x=date, y=uempmed)) +
    geom_line() +
    geom_line(aes(y=pred), color = "red")


profvis(
  {
    loess(uempmed ~ as.integer(date), data=economics, span=0.10)
  },
  interval = 0.001
)


profvis({
  calcSSE = function(x){
    sse = NA
    
    l = purrr::safely(loess)(uempmed ~ as.integer(date), data=economics, span=x)
    res = l$result$residuals
    if(!is.null(res)){
      sse = sum(res^2)  
    }
    
    sse
  }
  
  optim(par=c(0.5), calcSSE, method="SANN", control=list(maxit=10000))
})


?simpleLoess
??simpleLoess
stats:::simpleLoess