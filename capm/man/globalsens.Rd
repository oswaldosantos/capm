\name{globalsens}
\alias{globalsens}
\title{Global sensitivity analysis}
\usage{
  globalsens(model.out = NULL, ranges = NULL, all = FALSE)
}
\arguments{
  \item{model.out}{results of one of the following
  functions: \code{\link{sterowned}}}

  \item{ranges}{output from the \code{\link{setranges}}
  function, applied to the \code{pars} argument used in the
  function specified previously in \code{model.out}.}

  \item{all}{logical. If \code{\link{FALSE}}, sensitivity
  ranges are calculated for each parameter. If \code{TRUE},
  sensitivity ranges are calculated for the combination of
  all aparameters.}
}
\value{
  A \code{data.frame} (extended by \code{summary.sensRange}
  when \code{all == TRUE}) containing the parameter set and
  the corresponding values of the sensitivity output
  variables.
}
\description{
  Wraper for \code{\link{sensRange}} function which
  calculate sensitivities of population size, to parameters
  used in one of the following functions:
  \code{\link{sterowned}}.
}
\details{
  When \code{all} is equal to TRUE, \code{dist} argument in
  \code{\link{sensRange}} is defined as "latin" and when
  equal to \code{\link{FALSE}}, as "grid". The \code{num}
  argument in \code{\link{sensRange}} is defined as 100.
}
\examples{
#### example 1 - sterowned function ####
### Global sensitivity analysis for the parameters
### used in sterowned function.

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
state.od <- c(n = 125027.411, q = 0.188)

# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.ranges.1 = setranges(pars = pars.od)

## Calculate golobal sensitivity of combined parameters.
glob.od.all = globalsens(model.out = ster.od, ranges = par.ranges.1, all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.od = globalsens(model.out = ster.od, ranges = par.ranges.1)
}
\references{
  Soetaert K and Petzoldt T (2010). Inverse modelling,
  sensitivity and monte carlo analysis in R using package
  FME. Journal of Statistical Software, 33(3), pp. 1-28.

  Reichert P and Kfinsch HR (2001). Practical
  identifiability analysis of large environmental
  simulation models. Water Resources Research, 37(4),
  pp.1015-1030.
}
\seealso{
  \code{\link{sensRange}}.
}

