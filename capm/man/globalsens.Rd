\name{globalsens}
\alias{globalsens}
\title{Global sensitivity analysis}
\usage{
  globalsens(model.out = NULL, ranges = NULL, sensv = NULL,
    all = FALSE)
}
\arguments{
  \item{model.out}{results of one of the following
  functions: \code{\link{sterowned}}}

  \item{ranges}{output from the \code{\link{setranges}}
  function, applied to the \code{pars} argument used in the
  function specified previously in \code{model.out}.}

  \item{sensv}{string or \code{\link{character}}
  \code{\link{vector}} with the name of the the output
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
#####################
## Example 1       ##
## sterowned model ##
#####################

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.od <- c(b = 0.262, d = 0.075,
             k = 182531.831 * 1.1, s = .056)
state.od <- c(n = 82531.831, q = 0.207)


# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od,
                     state = state.od, time = 0:30)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.od.1 = setranges(pars = pars.od)

## Calculate golobal sensitivity of combined parameters.
glob.od.all = globalsens(
  model.out = ster.od, ranges = par.rg.od.1,
  sensv = 'n', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.od = globalsens(model.out = ster.od,
ranges = par.rg.od.1, sensv = 'n')

################
## Example 2  ##
## rasa model ##
################

## Parameters and intial conditions.
pars.rasa = c(
   b1 = 21585.15, b2 = 2374.367,
   df1 = 0.081, dm1 = 0.069, df2 = 0.089, dm2 = 0.076,
   sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
   k1 = 90785.01, k2 = 9078.501, h1 = 1, h2 = 0.5,
   ab = 0.065, ad = 0.095, v = 0.111
)
state.rasa = c(
   f1 = 41641.785, cf1 = 8423.503,
   m1 = 40890.046, cm1 = 8647.503,
   f2 = 4164.179, cf2 = 208.209,
   m2 = 4089.005, cm2 = 204.45
)

# Solve for point estimates.
rasa.pt <- rasa(pars = pars.rasa,
                state = state.rasa,
                time = 0:30)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.rasa.1 = setranges(pars = pars.rasa)

## Calculate golobal sensitivity of combined parameters.
glob.rasa.all = globalsens(
  model.out = rasa.pt,
  ranges = par.rg.rasa.1, sensv = 'n2', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.rasa = globalsens(
  model.out = rasa.pt, ranges = par.rg.rasa.1,
  sensv = 'n2')
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

