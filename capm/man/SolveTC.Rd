\name{SolveTC}
\alias{SolveTC}
\title{Reversible contraception for companion animals}
\usage{
  SolveTC(pars = NULL, init = NULL, time = NULL,
    fr.range = NULL, s.range = NULL, z.range = NULL, ...)
}
\arguments{
  \item{pars}{a named \code{\link{vector}} of length 5. The
  values are point estimates of the death rate (d), the
  fertility recovery rate (fr), the sterilization rate (s),
  the proportion of fertile immigrants (z) and the
  proportion of the death rate compensated by immigration
  (r). Abreviations in parentheses indicate the names that
  must be given to the values.}

  \item{init}{a named \code{\link{vector}} of length 2,
  with the total number of fertil (n) and infertil (cn)
  animals.}

  \item{time}{time sequence for which output is wanted; the
  first value of times must be the initial time.}

  \item{fr.range}{optional sequence (between 0 and 1) with
  the fertility recovery rates to be simulated.}

  \item{s.range}{optional \code{\link{vector}} of length 2,
  with a range of sterilization rates to be assessed. If
  given, the rates evaluated are those specified by the
  argument plus the point estimate given in \code{pars}.}

  \item{z.range}{optional \code{\link{vector}} of length 2,
  with a range of the proportion of infertile immigrants.
  If given, the rates evaluated are those specified by the
  argument plus the point estimate given in \code{pars}.}

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
  fourth columns contain the time and the variables:
  \code{n}, \code{cn} and \code{tcn}. When *.range
  arguments are given, additional columns contain the
  variables \code{f}, \code{s} and \code{z}.
}
\description{
  System of ordinary differential equations to simulate the
  effect of reversible contraception in a population at
  equilibrium, where deaths are compensated by births and
  net immigration.
}
\note{
  The implemented model is part of an ongoing PhD thesis
  (student: Oswaldo Santos; adviser: Fernando Ferreira) to
  be finished on the next months.
}
\examples{
# Parameters and initial conditions.
pars.solvetc <- c(d = 1 / 6, fr = 0.5, s = 0.2,
                   z = 0.8, dr = 0.1)

init.solvetc <- c(n = 950, cn = 50)

# Solve for point estimates.
solvetc.pt <- SolveTC(pars = pars.solvetc,
                      init = init.solvetc,
                      time = 0:30, method = 'rk4')

# Solve for parameter ranges.
solvetc.rg <- SolveTC(pars = pars.solvetc,
                      init = init.solvetc,
                      time = 0:20,
                      fr.range = seq(0, 1, 0.01),
                      s.range = c(0.05, 0.4),
                      z.range = c(0.95, 0.6),
                      method = 'rk4')
}
\seealso{
  \link[deSolve]{ode}.
}

