# Based on http://r-statistics.co/Loess-Regression-With-R.html

library(dplyr)
library(ggplot2)

data(economics, package="ggplot2")

l = loess(uempmed ~ as.integer(date), data=economics, span=0.10)
