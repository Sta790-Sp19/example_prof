suppressMessages( library(spBayes) )
RhpcBLASctl::blas_set_num_threads(1)


site_data = readRDS("data.rds")


lm = spLM(
  pm25 ~ 1, data = site_data, coords = cbind(x=site_data$day, y=0),
  starting = list(sigma.sq = 1,   tau.sq = 1,   phi = sqrt(3)/30),
  tuning   = list(sigma.sq = 0.1, tau.sq = 0.1, phi = 0.1),
  priors   = list(beta.Norm = list(0,1000), phi.Unif = c(sqrt(3)/30-1e-6, sqrt(3)/30+1e-6),
                  sigma.sq.IG = c(2, 2), tau.sq.IG = c(2, 0.1)),
  cov.model = "gaussian",
  amcmc = list(n.batch = 1000, batch.length = 50, accept.rate = 0.43),
  verbose = TRUE
)

