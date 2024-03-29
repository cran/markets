% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/market_model.R, R/diseq_basic.R,
%   R/diseq_deterministic_adjustment.R, R/diseq_directional.R,
%   R/diseq_stochastic_adjustment.R, R/equilibrium_model.R
\name{single_call_estimation}
\alias{single_call_estimation}
\alias{diseq_basic}
\alias{diseq_basic,formula-method}
\alias{diseq_deterministic_adjustment}
\alias{diseq_deterministic_adjustment,formula-method}
\alias{diseq_directional}
\alias{diseq_directional,formula-method}
\alias{diseq_stochastic_adjustment}
\alias{diseq_stochastic_adjustment,formula-method}
\alias{equilibrium_model}
\alias{equilibrium_model,formula-method}
\title{Single call estimation}
\usage{
diseq_basic(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

\S4method{diseq_basic}{formula}(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

diseq_deterministic_adjustment(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

\S4method{diseq_deterministic_adjustment}{formula}(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

diseq_directional(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

\S4method{diseq_directional}{formula}(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

diseq_stochastic_adjustment(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

\S4method{diseq_stochastic_adjustment}{formula}(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

equilibrium_model(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)

\S4method{equilibrium_model}{formula}(
  specification,
  data,
  correlated_shocks = TRUE,
  verbose = 0,
  estimation_options = list()
)
}
\arguments{
\item{specification}{The model's formula.}

\item{data}{The data to be used with the model.}

\item{correlated_shocks}{Should the model's system entail correlated shocks?
By default the argument is set to \code{TRUE}.}

\item{verbose}{The verbosity with which operations on the model print
messages. By default the value is set to \code{0}, which prints only errors.}

\item{estimation_options}{A list with options to be used in the estimation
call. See \code{\link[markets]{estimate}} for the available options.}
}
\value{
The fitted model.
}
\description{
Single call estimation
}
\details{
The functions of this section combine model initialization and estimation
into a single call. They also provide a less verbose interface to the
functionality of the package. The functions expect a formula following the
specification described in \link[=market_model_formula]{formula}, a
dataset, and optionally further initialization and
estimation options (see \link[=model_initialization]{model initialization}
and \link[=estimate]{model estimation} respectively).

Estimation options are expected to be given in the argument
\code{estimation_options} in a form of a \code{\link{list}}. The list
names should correspond to variables of the \code{\link{estimate}}
function. As a result, optimization options, which are customized using
the \code{control} argument of \code{\link{estimate}} can be passed
as an element of \code{estimation_options}.

Each of these functions parses the given formula, initializes the model
specified by the function's name, fits the model to the given data using
the estimation options and returns fitted model.
}
\section{Functions}{
\itemize{
\item \code{diseq_basic()}: Basic disequilibrium model.

\item \code{diseq_deterministic_adjustment()}: Disequilibrium model with deterministic
price adjustments.

\item \code{diseq_directional()}: Directional disequilibrium model.

\item \code{diseq_stochastic_adjustment()}: Disequilibrium model with stochastic
price adjustments.

\item \code{equilibrium_model()}: Equilibrium model

}}
\examples{
\donttest{
# An example of estimating the equilibrium model
eq <- equilibrium_model(
  HS | RM | ID | TREND ~ RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
    RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
  fair_houses(), estimation_options = list(control = list(maxit = 5000))
)

# An example of estimating the deterministic adjustment model
da <- diseq_deterministic_adjustment(
  HS | RM | ID | TREND ~ RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
    RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
  fair_houses(),
  verbose = 2,
  estimation_options = list(control = list(maxit = 5000))
)

# An example of estimating the directional model
dr <- diseq_directional(
  HS | RM | ID | TREND ~ TREND + W + CSHS + L1RM + L2RM |
    RM + TREND + W + MA6DSF + MA3DHF + MONTH,
  fair_houses(), estimation_options = list(
    method = "Nelder-Mead", control = list(maxit = 5000)
  )
)

# An example of estimating the basic model
start <- coef(eq)
start <- start[names(start) != "RHO"]
bs <- diseq_basic(
  HS | RM | ID | TREND ~ RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
    RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
  fair_houses(), verbose = 2, correlated_shocks = FALSE,
  estimation_options = list(
    start = start,
    control = list(maxit = 5000)
  )
)

# An example of estimating the stochastic adjustment model
sa <- diseq_stochastic_adjustment(
  HS | RM | ID | TREND ~ RM + TREND + W + CSHS + MONTH |
    RM + TREND + W + L1RM + L2RM + MA6DSF + MA3DHF + MONTH |
    TREND + L2RM + L3RM,
  fair_houses() |> dplyr::mutate(L3RM = dplyr::lag(RM, 3)),
  correlated_shocks = FALSE,
  estimation_options = list(
    control = list(maxit = 5000), standard_errors = c("W")
  )
)
}
}
