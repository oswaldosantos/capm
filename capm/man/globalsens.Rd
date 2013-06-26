\name{globalsens}
\alias{globalsens}
\title{Global sensitivity analysis}
\usage{
  globalsens(model.out = NULL, ranges = NULL, sensv = NULL,
    all = FALSE)
}
\arguments{
  \item{model.out}{results of one of the following
  functions: \code{\link{sterim}}}

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
  \code{\link{sterim}}.
}
\details{
  When \code{all} is equal to TRUE, \code{dist} argument in
  \code{\link{sensRange}} is defined as "latin" and when
  equal to \code{\link{FALSE}}, as "grid". The \code{num}
  argument in \code{\link{sensRange}} is defined as 100.
}
\examples{
##################
## Example 1    ##
## sterim model ##
##################

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.sterim = c(b = 0.245, d = 0.101,
             k = 98050.49, s = .048)
state.sterim = c(n = 89136.810, q = 0.198)

# Solve for a specific sterilization rate.
sterim.pt = sterim(pars = pars.sterim, state = state.sterim,
                  time = 0:30, dd = 'b', imm = 100)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.sterim.1 = setranges(pars = pars.sterim)

## Calculate golobal sensitivity of combined parameters.
glob.sterim.all = globalsens(
  model.out = sterim.pt, ranges = par.rg.sterim.1,
  sensv = 'n', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.sterim = globalsens(model.out = sterim.pt,
ranges = par.rg.sterim.1, sensv = 'n')

################
## Example 2  ##
## iasa model ##
################

## Parameters and intial conditions.
pars.iasa = c(
   b1 = 21870.897, b2 = 4374.179,
   df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
   sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
   k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
   ab = 0.054, ad = 0.1, v = 0.1
)
state.iasa = c(
   f1 = 33425.19, cf1 = 10864.901,
   m1 = 38038.96, cm1 = 6807.759,
   f2 = 3342.519, cf2 = 108.64901,
   m2 = 3803.896, cm2 = 68.07759
)

# Solve for point estimates.
iasa.pt <- iasa(pars = pars.iasa,
                state = state.iasa,
                time = 0:30)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.iasa.1 = setranges(pars = pars.iasa)

## Calculate golobal sensitivity of combined parameters.
glob.iasa.all = globalsens(
  model.out = iasa.pt,
  ranges = par.rg.iasa.1, sensv = 'n2', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.iasa = globalsens(
  model.out = iasa.pt, ranges = par.rg.iasa.1,
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

