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


## Profvis

library(profvis)


profvis(
  {
    l = lm(y ~ ., data = d)$coefficients
  },
  interval = 0.005
)


profvis(
  {
    y = d$y 
    x = select(d, -y) %>% as.matrix() %>% cbind(1, .)
    lm.fit(x,y)$coefficients
  },
  interval = 0.005
)


profvis(
  {
    y = d$y 
    x = select(d, -y) %>% as.matrix() %>% cbind(1, .)
    .lm.fit(x,y)$coefficients
  },
  interval = 0.005
)


profvis(
  {
    x = select(d, -y) %>% as.matrix() %>% cbind(1, .)
    solve(t(x) %*% x, t(x) %*% d$y) %>% c()
  },
  interval = 0.005
)






## bench

f_lm = function() {
  l = lm(y ~ ., data = d)$coefficients
}


f_lm.fit = function() {
  y = d$y 
  x = select(d, -y) %>% as.matrix() %>% cbind(1, .)
  lm.fit(x,y)$coefficients
}

f_.lm.fit = function() {
  y = d$y 
  x = select(d, -y) %>% as.matrix() %>% cbind(1, .)
  .lm.fit(x,y)$coefficients
}

f_hat = function() {
  x = select(d, -y) %>% as.matrix() %>% cbind(1, .)
  solve(t(x) %*% x, t(x) %*% d$y) %>% c()
}

bench::mark(
  f_lm(), f_lm.fit(), f_.lm.fit(), f_hat(), 
  min_iterations = 5, check = FALSE 
)
