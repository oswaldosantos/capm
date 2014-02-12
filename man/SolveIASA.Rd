\name{SolveIASA}
\alias{SolveIASA}
\title{Immigration, abandonment, sterilization and adoption of companion animals}
\usage{
  SolveIASA(pars = NULL, init = NULL, time = NULL,
    s.range = NULL, ab.range = NULL, ad.range = NULL,
    im.range = NULL, s.fm = TRUE, ...)
}
\arguments{
  \item{pars}{a named \code{\link{vector}} of length 21,
  with point estimates of model parameters (see details).}

  \item{init}{a named \code{\link{vector}} of length 8,
  with point estimates of model parameters (see details).}

  \item{time}{time sequence for which output is wanted; the
  first value of times must be the initial time.}

  \item{s.range}{optional sequence (between 0 and 1) of the
  sterilization rates to be simulated.}

  \item{ab.range}{optional \code{\link{vector}} of length
  2, with range (ie, confidence interval) of abandonment
  rates to be assessed. If given, the rates evaluated are
  those specified by the argument plus the point estimate
  given in \code{pars}.}

  \item{ad.range}{optional \code{\link{vector}} of length
  2, with range (ie, confidence interval) of adoption rates
  to be assessed. If given, the rates evaluated are those
  specified by the argument plus the point estimate given
  in \code{pars}.}

  \item{im.range}{optional \code{\link{vector}} of length
  2, with range of values of immigration rates to be
  assessed. This must be expressed as a percentage of owned
  animals carrying capacity.}

  \item{s.fm}{logical. If \code{TRUE}, s.range is used for
  females and males and if \code{FALSE}, it is only used
  for females (for males, the point estimate given in
  \code{pars} is used.)}

  \item{...}{further arguments passed to
  \link[deSolve]{ode} function.}
}
\value{
  \code{\link{list}} of class \code{capmModels}. The first
  element, \code{name}, is a string with the name of the
  function, the second element, \code{*$model}, is the
  model function. The third, fourth and fifth elements are
  vectors (\code{*$pars}, \code{*$init}, \code{*$time},
  respectively) containing the \code{pars}, \code{init} and
  \code{time} arguments of the function. The sisxthth
  element \code{*$results} is a \code{\link{data.frame}}
  with up to as many rows as elements in time. The first
  column contain the time and subsequent columns contain
  the sise of specific subpopulations, named according to
  above conventions. The \code{group} column differentiate
  between owned and strays. When *.range arguments are
  given, the last fourth columsn specify their instances.
}
\description{
  System of ordinary differential equations to simulate the
  effect of immigration of owned dogs, abandonment,
  sterilization of owned and stray dogs and adoption, on
  population dynamics.
}
\details{
  The \code{pars} argument must contain named values, using
  the following conventions: \code{1}: owned animals;
  \code{2}: stray animals; \code{f}: females; \code{m}:
  males. Then:

  \code{b1} and \code{b2}: number of births.

  \code{df1}, \code{dm1}, \code{df2} and \code{dm2}: death
  rate.

  \code{sf1}, \code{sm1}, \code{sf2} and \code{sm2}:
  sterilization rate.

  \code{k1} and \code{k2}: carrying capacity.

  \code{h1} and \code{h2}: mean harem size.

  \code{ab}: abandonment rate.

  \code{ad}: adoption rate.

  \code{v}: immigration rate.

  \code{z}: proportion of sterilized immigrants.

  The \code{init} argument must contain named values for
  the inital number of animals, using the following
  conventions: \code{1}: owned animals; \code{2}: stray
  animals; \code{f}: females; \code{m}: males; and
  \code{s}: sterilized. Then, number values must be given
  for the categories:

  \code{f1}, \code{fs1}, \code{m1}, \code{ms1}, \code{f2},
  \code{fs2}, \code{m2} and \code{ms2}.

  If any range is specified (e.g \code{s.range}), the
  ramining ranges must be specified too (\code{ab.range},
  \code{ad.range} and \code{im.range}). The function is a
  wrapper around the defaults of \link[deSolve]{ode}
  function, whose help page must be consulted for details.
  An exception is the method argument which is defined as
  "rk4".
}
\note{
  Logistic growth models are not intended for scenarios in
  which population size is greater than carrying capacity
  and growth rate is negative.

  The implemented model is part of an ongoing PhD thesis
  (student: Oswaldo Santos; adviser: Fernando Ferreira) to
  be finished on the next months.
}
\examples{
# Parameters and initial conditions from estimates
# obtained in examples section from svysumm function.
# Note that there is not estimates for all arguments
# and thus, some are a priori defined.
# For example, carrying capacities were estimated as
# 10 \% greater than population sizes and birth and
# death rates for stray animals were estimated as
# 20 \% greater than those rates in owned animals.
# The consequences of those "guesses" can be quantified
# with globalsens and localsens functions.
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

# Solve for parameter ranges.
solve.iasa.rg <- SolveIASA(pars = pars.solve.iasa,
                          init = init.solve.iasa,
                          time = 0:14,
                          s.range = seq(0, .4, l = 20),
                          ab.range = c(0, .2),
                          ad.range = c(0, .2),
                          im.range = c(0, .1),
                          method = 'rk4')
}
\seealso{
  \link[deSolve]{ode}.
}

