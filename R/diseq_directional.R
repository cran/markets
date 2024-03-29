#' @include disequilibrium_model.R

#' @rdname market_models
#' @description
#' \subsection{Directional disequilibrium model with sample separation
#' (\code{diseq_directional})}{
#' The directional disequilibrium model consists of three equations and a
#' separation rule. The market is described by a linear demand, a linear supply
#' equation and the short side rule. The separation rule splits the sample
#' into states of excess supply and excess demand. If a price change is
#' positive at the time point of the observation, then the observation is
#' classified as being in an excess demand state. Otherwise, it is assumed
#' that it represents an excess supply state. The
#' model is estimated using full information maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = \min\{D_{nt},S_{nt}\},}
#' \deqn{\Delta P_{nt} \ge 0 \Longrightarrow D_{nt} \ge S_{nt}.}
#' }
#' @export
setClass(
  "diseq_directional",
  contains = "disequilibrium_model",
  representation(),
  prototype()
)

#' @describeIn model_initialization Directional disequilibrium model base constructor
#' @examples
#' \donttest{
#' simulated_data <- simulate_data(
#'   "diseq_directional", 500, 3, # model type, observed entities, observed time points
#'   -0.2, 4.3, c(0.03, 0.02), c(0.03, 0.01), # demand coefficients
#'   0.0, 4.0, c(0.03), c(0.05, 0.02) # supply coefficients
#' )
#'
#' # in the directional model prices cannot be included in both demand and supply
#' model <- new(
#'   "diseq_directional", # model type
#'   subject = id, time = date, quantity = Q, price = P,
#'   demand = P + Xd1 + Xd2 + X1 + X2, supply = Xs1 + X1 + X2,
#'   simulated_data, # data
#'   correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' show(model)
#' }
setMethod(
  "initialize", "diseq_directional",
  function(.Object,
           quantity, price, demand, supply, subject, time,
           data, correlated_shocks = TRUE, verbose = 0) {
    specification <- make_specification(
      substitute(quantity), substitute(price),
      substitute(demand), substitute(supply), substitute(subject), substitute(time)
    )
    .Object <- callNextMethod(
      .Object, "Directional", verbose,
      specification, correlated_shocks, data,
      function(...) new("system_directional", ...)
    )

    # Check for mis-specification
    price_column <- all.vars(formula(specification, lhs = 2, rhs = 0))
    if (
      price_column %in% independent_variables(.Object@system@demand) &&
        price_column %in% independent_variables(.Object@system@supply)
    ) {
      print_error(
        .Object@logger,
        "Price cannot be part of both the demand and supply equations here ",
        "(See Maddala, (1974) <https://doi.org/10.2307/1914215>, p1021)."
      )
    }

    ndrows <- sum(.Object@system@demand@separation_subset)
    nsrows <- sum(.Object@system@supply@separation_subset)
    print_info(
      .Object@logger,
      "Sample separated with ", ndrows,
      " row", ifelse(ndrows > 1, "s"), " in excess supply and ",
      nsrows, " row", ifelse(ndrows > 1, "s"), " in excess demand states."
    )

    .Object
  }
)

#' @describeIn single_call_estimation Directional disequilibrium model.
#' @export
setGeneric(
  "diseq_directional",
  function(specification, data,
           correlated_shocks = TRUE, verbose = 0,
           estimation_options = list()) {
    standardGeneric("diseq_directional")
  }
)

#' @rdname single_call_estimation
setMethod(
  "diseq_directional", signature(specification = "formula"),
  function(specification, data, correlated_shocks, verbose,
           estimation_options) {
    initialize_and_estimate(
      "diseq_directional", specification, data, correlated_shocks, verbose,
      estimation_options
    )
  }
)

#' @rdname model_likelihoods
setMethod(
  "log_likelihood", signature(object = "diseq_directional"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)

    loglhd <- sum(
      log(object@system@demand@Psi[object@system@demand@separation_subset] /
        object@system@demand@sigma)
    )
    loglhs <- sum(
      log(object@system@supply@Psi[object@system@supply@separation_subset] /
        object@system@supply@sigma)
    )
    loglhd + loglhs
  }
)

#' @rdname model_likelihoods
setMethod(
  "gradient", signature(object = "diseq_directional"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    colSums(calculate_system_scores(object@system))
  }
)

#' @rdname model_likelihoods
setMethod(
  "scores", signature(object = "diseq_directional"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    calculate_system_scores(object@system)
  }
)

setMethod(
  "calculate_initializing_values", signature(object = "diseq_directional"),
  function(object) {
    demand <- stats::lm(
      object@system@demand@dependent_vector ~
        object@system@demand@independent_matrix - 1,
      subset = object@system@demand@separation_subset
    )
    names(demand$coefficients) <- colnames(
      object@system@demand@independent_matrix
    )
    var_d <- var(demand$residuals)
    names(var_d) <- prefixed_variance_variable(object@system@demand)

    supply <- stats::lm(
      object@system@supply@dependent_vector ~
        object@system@supply@independent_matrix - 1,
      subset = object@system@supply@separation_subset
    )
    names(supply$coefficients) <- colnames(
      object@system@supply@independent_matrix
    )
    var_s <- var(supply$residuals)
    names(var_s) <- prefixed_variance_variable(object@system@supply)

    start <- c(demand$coefficients, supply$coefficients, var_d, var_s)

    if (object@system@correlated_shocks) {
      rho <- 0.0
      names(rho) <- correlation_variable(object@system)

      start <- c(start, rho)
    }

    start
  }
)
