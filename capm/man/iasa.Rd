\name{iasa}
\alias{iasa}
\title{Immigration, abandonment, sterilization and adoption of companion animals}
\usage{
  iasa(pars = NULL, state = NULL, time = NULL,
    ster.range = NULL, aban.range = NULL,
    adop.range = NULL, imm.range = NULL, ster.fm = TRUE,
    ...)
}
\arguments{
  \item{pars}{a named \code{\link{vector}} of length 21,
  with point estimates of model parameters (see details).}

  \item{state}{a named \code{\link{vector}} of length 8,
  with point estimates of model parameters (see details).}

  \item{time}{time sequence for which output is wanted; the
  first value of times must be the initial time.}

  \item{ster.range}{optional sequence (between 0 and 1) of
  the sterilization rates to be simulated.}

  \item{aban.range}{optional \code{\link{vector}} of length
  2, with range (ie, confidence interval) of abandonment
  rates to be assessed. If given, the rates evaluated are
  those specified by the argument plus the point estimate
  given in \code{pars}.}

  \item{adop.range}{optional \code{\link{vector}} of length
  2, with range (ie, confidence interval) of adoption rates
  to be assessed. If given, the rates evaluated are those
  specified by the argument plus the point estimate given
  in \code{pars}.}

  \item{imm.range}{optional \code{\link{vector}} of length
  2, with range of values of immigration rates to be
  assessed. This must be expressed as a percentage of owned
  animals carrying capacity.}

  \item{ster.fm}{logical. If \code{TRUE}, ster.range is
  used for females and males and if \code{FALSE}, it is
  only used for females (for males, the point estimate
  given in \code{pars} is used.)}

  \item{...}{further arguments passed to
  \link[deSolve]{ode} function.}
}
\value{
  \code{\link{list}} of class \code{iasa}. The first
  element, \code{*$model}, is the model function. The
  second, third and fourth elements are vectors
  (\code{*$pars}, \code{*$state}, \code{*$time},
  respectively) containing the \code{pars}, \code{state}
  and \code{time} arguments of the function. The fifth
  element \code{*$results} is a \code{\link{data.frame}}
  with up to as many rows as elements in time. Using the
  conventions for state argument (see details), the first
  fourth columns contain the variables: \code{f},
  \code{sf}, \code{m} and \code{sm}. The fifth and sisxth
  columns contain the number of animals and the group
  respectively (\code{n} and \code{group}). Other optional
  arguments are:

  \code{ster}: instance fo sterilization rate (if
  \code{ster.range} is specified).

  \code{aban}: instance fo abandonment rate (if
  \code{aban.range} is specified).

  \code{adop}: instance fo adoption rate (if
  \code{adop.range} is specified).
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

  The \code{state} argument must contain named values for
  the inital number of animals, using the following
  conventions: \code{1}: owned animals; \code{2}: stray
  animals; \code{f}: females; \code{m}: males; and
  \code{s}: sterilized. Then, number values must be given
  for the categories:

  \code{f1}, \code{cf1}, \code{m1}, \code{cm1}, \code{f2},
  \code{cf2}, \code{m2} and \code{cm2}.

  If any range is specified (e.g \code{ster.range}), the
  ramining ranges must be specified too (\code{aban.range},
  \code{adop.range} and \code{imm.range}). The function is
  a wrapper around the defaults of \link[deSolve]{ode}
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
  be finished at the end of 2013.
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

# Solve for parameter ranges.
iasa.rg <- iasa(pars = pars.iasa,
                state = state.iasa,
                time = 0:20,
                ster.range = seq(0, .4, length.out = 20),
                aban.range = c(0, .2),
                adop.range = c(0, .2),
                imm.range = c(0, .1))
}
\references{
  Soetaert K, Cash J and Mazzia F (2012). Solving
  differential equations in R. Springer.
}
\seealso{
  \link[deSolve]{ode}.
}

