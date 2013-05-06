\name{rasa}
\alias{rasa}
\title{Recruitment, abandonment, sterilization and adoption of dogs}
\usage{
  rasa(pars = NULL, state = NULL, time = NULL,
    ster.range = NULL, aban.range = NULL,
    adop.range = NULL, ster.fm = TRUE, ...)
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

  \item{ster.fm}{logical. If \code{TRUE}, ster.range is
  used for females and males and if \code{FALSE}, it is
  only used for females (for males, the point estimate
  given in \code{pars} is used.)}

  \item{...}{further arguments passed to
  \link[deSolve]{ode} function.}
}
\value{
  \code{\link{list}} of class \code{rasa}. The first
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
  effect of recruitment of owned dogs, abandonment,
  sterilization of owned and stray dogs and adoption, on
  population dynamics.
}
\details{
  The \code{pars} argument must contain named values, using
  the following conventions: \code{1}: owned animals;
  \code{2}: stray animals; \code{f}: females; \code{m}:
  males. Then:

  \code{af1}, \code{am1} \code{af2} and \code{af2}: birth
  rate.

  \code{bf1}, \code{bm1} \code{bf2} and \code{bf2}: death
  rate.

  \code{ef1}, \code{em1} \code{ef2} and \code{ef2}:
  sterilization rate.

  \code{k1} and \code{k2}: carrying capacity.

  \code{z1} and \code{z2}: mean harem size.

  \code{h}: abandonment rate.

  \code{j}: adoption rate.

  The \code{state} argument must contain named values for
  the inital number of animals, using the following
  conventions: \code{1}: owned animals; \code{2}: stray
  animals; \code{f}: females; \code{m}: males; and
  \code{s}: sterilized. Then, number values must be given
  for the categories:

  \code{f1}, \code{sf1}, \code{m1}, \code{sm1}, \code{f2},
  \code{sf2}, \code{m2} and \code{sm1}.

  The function is a wrapper around the defaults of
  \link[deSolve]{ode} function, whose help page must be
  consulted for details.
}
\note{
  The implemented model is part of an ongoing PhD thesis
  (student: Oswaldo Santos; adviser: Fernando Ferreira) to
  be finished at the end of 2013.
}
\examples{
# Parameters and initial conditions from estimates
# obtained in examples section from svysumm function.
pars.rasa = #
state.rasa = #

# Solve for point estimates.
rasa.pt <- rasa(pars = pars.rasa, state = state.rasa, time = 0:30)

# Solve for parameter ranges.
rasa.rg <- sterowned(pars = pars.rasa, state = state.rasa, time = 0:30, ster.range = seq(0, .4, length.out = 50), aban.range = c(0, .4), adop.range = c(0, .4))
}
\references{
  Soetaert K, Cash J and Mazzia F (2012). Solving
  differential equations in R. Springer.
}
\seealso{
  \link[deSolve]{ode}.
}

