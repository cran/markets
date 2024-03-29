#' @include equation_base.R

#' @title System classes
#' @description Classes with data and functionality describing systems of models.
#' @name system_classes
#' @keywords internal
#' @details
#' \subsection{\code{system_base}}{System base class}
#' @slot demand Demand equation.
#' @slot supply Supply equation.
#' @slot correlated_shocks Boolean indicating whether the shock of the
#' equations of the system are correlated.
#' @slot sample_separation Boolean indicating whether the sample of the
#' system is separated.
#' @slot quantity_vector A vector with the system's observed quantities.
#' @slot price_vector A vector with the system's observed prices.
#' @slot rho Correlation coefficient of demand and supply shocks.
#' @slot rho1 \deqn{\rho_{1} = \frac{1}{\sqrt{1 - \rho}}}
#' @slot rho2 \deqn{\rho_{2} = \rho\rho_{1}}
setClass(
  "system_base",
  representation(
    formula = "Formula",
    demand = "equation_base",
    supply = "equation_base",
    correlated_shocks = "logical",
    sample_separation = "logical",
    quantity_vector = "matrix",
    price_vector = "matrix",
    rho = "numeric",
    rho1 = "numeric",
    rho2 = "numeric"
  ),
  prototype(
    demand = NULL,
    supply = NULL,
    quantity_vector = matrix(NA_real_),
    price_vector = matrix(NA_real_),
    rho = 0,
    rho1 = 1,
    rho2 = 0
  )
)

setMethod(
  "initialize", "system_base",
  function(.Object, specification, data, correlated_shocks,
           demand_initializer, supply_initializer) {
    .Object@formula <- specification
    .Object@demand <- demand_initializer(
      formula(specification, rhs = 1), data, "Demand", "D_"
    )
    .Object@supply <- supply_initializer(
      formula(specification, rhs = 2), data, "Supply", "S_"
    )
    .Object@correlated_shocks <- correlated_shocks
    .Object@sample_separation <- FALSE

    .Object@quantity_vector <- as.matrix(model.frame(specification, data, lhs = 1, rhs = 0))
    .Object@price_vector <- as.matrix(model.frame(specification, data, lhs = 2, rhs = 0))

    .Object
  }
)

setMethod("show_implementation", signature(object = "system_base"), function(object) {
  show_implementation(object@demand)
  show_implementation(object@supply)
})

setGeneric("summary_implementation", function(object) {
  standardGeneric("summary_implementation")
})

setMethod("summary_implementation", signature(object = "system_base"), function(object) {
  sample_separation_output <- ""
  if (object@sample_separation) {
    sample_separation_output <- sprintf(
      "Demand Obs = %d, Supply Obs = %d",
      sum(object@demand@separation_subset),
      sum(object@supply@separation_subset)
    )
  } else {
    sample_separation_output <- "Not Separated"
  }
  cat(
    labels = sprintf("  %-18s:", "Sample Separation"),
    sample_separation_output,
    sep = "", fill = TRUE
  )
  cat(
    labels = sprintf("  %-18s:", "Quantity Var"),
    colnames(object@quantity_vector),
    sep = "", fill = TRUE
  )
  cat(
    labels = sprintf("  %-18s:", "Price Var"),
    colnames(object@price_vector),
    sep = "", fill = TRUE
  )
})

#' @describeIn variable_names Lagged price variable name.
#' @description \code{lagged_price_variable}: The lagged price variable name is
#' constructed by concatenating \code{LAGGED} with the price variable name.
setGeneric("lagged_price_variable", function(object) {
  standardGeneric("lagged_price_variable")
})

#' @describeIn variable_names Price differences variable name.
#' @description \code{price_differences_variable}: The price difference variable name is
#' constructed by concatenating the price variable name with \code{DIFF}.
setGeneric("price_differences_variable", function(object) {
  standardGeneric("price_differences_variable")
})

setGeneric("correlation_variable", function(object) {
  standardGeneric("correlation_variable")
})

setGeneric("likelihood_variables", function(object) {
  standardGeneric("likelihood_variables")
})

setGeneric("calculate_system_moments", function(object) {
  standardGeneric("calculate_system_moments")
})

setGeneric("calculate_system_likelihood", function(object) {
  standardGeneric("calculate_system_likelihood")
})

setGeneric("calculate_system_loglikelihood", function(object) {
  standardGeneric("calculate_system_loglikelihood")
})

setGeneric("calculate_system_gradient", function(object) {
  standardGeneric("calculate_system_gradient")
})

setGeneric("calculate_system_scores", function(object) {
  standardGeneric("calculate_system_scores")
})

#' @rdname variable_names
setMethod("lagged_price_variable", signature(object = "system_base"), function(object) {
  paste0("LAGGED_", colnames(object@price_vector))
})

#' @rdname variable_names
setMethod(
  "price_differences_variable", signature(object = "system_base"),
  function(object) {
    paste0(colnames(object@price_vector), "_DIFF")
  }
)

setMethod("correlation_variable", signature(object = "system_base"), function(object) {
  "RHO"
})

setGeneric("calculate_llh", function(object) {
  standardGeneric("calculate_llh")
})

setMethod("likelihood_variables", signature(object = "system_base"), function(object) {
  likelihood_variables <- c(
    prefixed_price_variable(object@demand),
    prefixed_control_variables(object@demand),
    prefixed_price_variable(object@supply),
    prefixed_control_variables(object@supply),
    prefixed_variance_variable(object@demand),
    prefixed_variance_variable(object@supply)
  )

  if (object@correlated_shocks) {
    likelihood_variables <- c(likelihood_variables, correlation_variable(object))
  }

  likelihood_variables
})

setMethod(
  "set_parameters", signature(object = "system_base"),
  function(object, parameters) {
    object@demand <- set_parameters(object@demand, parameters)
    object@supply <- set_parameters(object@supply, parameters)
    if (object@correlated_shocks) {
      object@rho <- parameters[correlation_variable(object)]
      object@rho <- ifelse(abs(object@rho) > 1, NA_real_, object@rho)
      object@rho1 <- 1 / sqrt(1 - object@rho**2)
      object@rho2 <- object@rho * object@rho1
    }
    object
  }
)
