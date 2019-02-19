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


system.time({l = lm(y ~ ., data = d)})

## profvis

library(profvis)

profvis(
  {
    l = lm(y ~ ., data = d)
  }
)

profvis(
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    l = lm.fit(x, y)
  },
  interval = 0.005
)

profvis(
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    l = .lm.fit(x, y)
  },
  interval = 0.005
)

profvis(
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    xt = t(x)
    solve(t(x) %*% x, t(x) %*% y) %>% c()
  },
  interval = 0.005
)

profvis(
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    xt = t(x)
    solve(xt %*% x, xt %*% y) %>% c()
  },
  interval = 0.005
)

### bench

library(bench)

bench::mark(
  {
    l = lm(y ~ ., data = d)
  },
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    l = lm.fit(x, y)
  },
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    l = .lm.fit(x, y)
  },
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    xt = t(x)
    solve(t(x) %*% x, t(x) %*% y) %>% c()
  },
  {
    x = d %>% select(-y) %>% as.matrix() %>% cbind(`(Intercept)` = 1, .)
    y = d$y
    xt = t(x)
    solve(xt %*% x, xt %*% y) %>% c()
  },
  check = FALSE, min_iterations = 5
)


