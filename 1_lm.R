## Setup

library(dplyr)

n = 1e6

d = tibble(
  x1 = rnorm(n, mean=0, sd=1),
  x2 = rnorm(n, mean=0, sd=2),
  x3 = rnorm(n, mean=0, sd=1/2),
  x4 = rnorm(n, mean=0, sd=3),
  x5 = rnorm(n, mean=0, sd=1/3),
) %>%
  mutate(
    y = x1 - x2 + x5 + rnorm(n, sd=1)
  )


l = lm(y ~ ., data = d)
