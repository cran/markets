## ----include = FALSE----------------------------------------------------------
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
}

## ----setup.libraries----------------------------------------------------------
library(markets)

## ----setup.data---------------------------------------------------------------
nobs <- 2000
tobs <- 5

alpha_d <- -1.3
beta_d0 <- 24.8
beta_d <- c(2.3, -1.02)
eta_d <- c(2.6, -1.1)

alpha_s <- 0.6
beta_s0 <- 16.1
beta_s <- c(2.9)
eta_s <- c(-1.5, 3.2)

gamma <- 1.2
beta_p0 <- 0.9
beta_p <- c(-0.1)

sigma_d <- 1
sigma_s <- 1
sigma_p <- 1
rho_ds <- 0.0
rho_dp <- 0.0
rho_sp <- 0.0

seed <- 443

stochastic_adjustment_data <- simulate_data(
  "diseq_stochastic_adjustment", nobs, tobs,
  alpha_d, beta_d0, beta_d, eta_d,
  alpha_s, beta_s0, beta_s, eta_s,
  gamma, beta_p0, beta_p,
  sigma_d = sigma_d, sigma_s = sigma_s, sigma_p = sigma_p,
  rho_ds = rho_ds, rho_dp = rho_dp, rho_sp = rho_sp,
  seed = seed
)

## ----model.parameters.verbose-------------------------------------------------
verbose <- 2

## ----model.parameters.correlated_shocks---------------------------------------
correlated_shocks <- TRUE

## ----model.constructor--------------------------------------------------------
eq <- new(
  "equilibrium_model",
  quantity = Q, price = P,
  demand = P + Xd1 + Xd2 + X1 + X2,
  supply = P + Xs1 + X1 + X2,
  subject = id, time = date,
  data = stochastic_adjustment_data,
  correlated_shocks = correlated_shocks, verbose = verbose
)
bs <- new(
  "diseq_basic",
  quantity = Q, price = P,
  demand = P + Xd1 + Xd2 + X1 + X2,
  supply = P + Xs1 + X1 + X2,
  subject = id, time = date,
  data = stochastic_adjustment_data,
  correlated_shocks = correlated_shocks, verbose = verbose
)
dr <- new(
  "diseq_directional",
  quantity = Q, price = P,
  demand = P + Xd1 + Xd2 + X1 + X2,
  supply = Xs1 + X1 + X2,
  subject = id, time = date,
  data = stochastic_adjustment_data,
  correlated_shocks = correlated_shocks, verbose = verbose
)
da <- new(
  "diseq_deterministic_adjustment",
  quantity = Q, price = P,
  demand = P + Xd1 + Xd2 + X1 + X2,
  supply = P + Xs1 + X1 + X2,
  subject = id, time = date,
  data = stochastic_adjustment_data,
  correlated_shocks = correlated_shocks, verbose = verbose
)
sa <- new(
  "diseq_stochastic_adjustment",
  quantity = Q, price = P,
  demand = P + Xd1 + Xd2 + X1 + X2,
  supply = P + Xs1 + X1 + X2,
  price_dynamics = Xp1,
  subject = id, time = date,
  data = stochastic_adjustment_data,
  correlated_shocks = correlated_shocks, verbose = verbose
)

## ----estimation.parameters.method---------------------------------------------
optimization_method <- "BFGS"
optimization_options <- list(REPORT = 10, maxit = 10000, reltol = 1e-6)

## ----estimation.execution-----------------------------------------------------
estimate(eq, method = "2SLS")
estimate(eq,
  control = optimization_options, method = optimization_method,
  standard_errors = c("id")
)
estimate(bs,
  control = optimization_options, method = optimization_method,
  standard_errors = "heteroscedastic"
)
estimate(bs,
  control = optimization_options, method = "Nelder-Mead",
  standard_errors = "heteroscedastic"
)
estimate(dr,
  control = optimization_options, method = optimization_method,
  standard_errors = "heteroscedastic"
)
estimate(da,
  control = optimization_options, method = optimization_method,
  standard_errors = c("id")
)
estimate(sa,
  control = optimization_options, method = optimization_method
)

