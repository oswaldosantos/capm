\name{CalculateGlobalSens}
\alias{CalculateGlobalSens}
\title{Global sensitivity analysis}
\usage{
CalculateGlobalSens(model.out = NULL, ranges = NULL, sensv = NULL,
  all = FALSE)
}
\arguments{
  \item{model.out}{an output from one of the previous
  function or a \code{\link{list}} with equivalent
  structure.}

  \item{ranges}{output from the \code{\link{SetRanges}}
  function, applied to the \code{pars} argument used in the
  function previously specified in \code{model.out}.}

  \item{sensv}{string with the name of the the output
  variables for which the sensitivity needs to be
  estimated.}

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
Wraper for \code{\link{sensRange}} function, which
calculates population size sensitivities, to parameters
used in one of the following functions:
\code{\link{SolveIASA}}, \code{\link{SolveSI}} or
\code{\link{SolveTC}}.
}
\details{
When \code{all} is equal to TRUE, \code{dist} argument in
\code{\link{sensRange}} is defined as "latin" and when
equal to \code{\link{FALSE}}, as "grid". The \code{num}
argument in \code{\link{sensRange}} is defined as 100.
}
\examples{
#####################
## SolveIASA model ##
#####################

## Parameters and intial conditions.
pars.solve.iasa = c(
   b1 = 21870.897, b2 = 4374.179,
   df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
   sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
   k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
   ab = 0.054, ad = 0.1, v = 0.2, z = 0.1)

init.solve.iasa = c(
   f1 = 33425.19, fs1 = 10864.901,
   m1 = 38038.96, ms1 = 6807.759,
   f2 = 3342.519, fs2 = 108.64901,
   m2 = 3803.896, ms2 = 68.07759)


# Solve for point estimates.
solve.iasa.pt <- SolveIASA(pars = pars.solve.iasa,
                          init = init.solve.iasa,
                          time = 0:15, method = 'rk4')

## Set ranges 10 \% greater and lesser than the
## point estimates.
rg.solve.iasa <- SetRanges(pars = pars.solve.iasa)

## Calculate golobal sensitivity of combined parameters.
## To calculate global sensitivity to each parameter, set
## all as FALSE.
glob.all.solve.iasa <- CalculateGlobalSens(
  model.out = solve.iasa.pt,
  ranges = rg.solve.iasa,
  sensv = 'n2', all = TRUE)
}
\references{
Soetaert K and Petzoldt T (2010). Inverse modelling,
sensitivity and monte carlo analysis in R using package
FME. Journal of Statistical Software, 33(3), pp. 1-28.

Reichert P and Kfinsch HR (2001). Practical identifiability
analysis of large environmental simulation models. Water
Resources Research, 37(4), pp.1015-1030.

\url{http://oswaldosantos.github.io/capm}
}
\seealso{
\code{\link{sensRange}}.
}

