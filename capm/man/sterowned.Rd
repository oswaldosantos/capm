\name{sterowned}
\alias{sterowned}
\title{Owned dog sterilization}
\usage{
  sterowned(pars = NULL, state = NULL, time = NULL,
    ster.range = NULL, ...)
}
\arguments{
  \item{pars}{\code{\link{vector}} of length 4. The values
  are point estimates of birth rate, death rate, carrying
  capacity and sterilization rate. The names of this values
  must be "b", "d", "k" and "s", respectively.}

  \item{state}{\code{\link{vector}} of length 2. The values
  are initial population size and initial proportion of
  sterilized animals. The names of this values must be "n"
  and "q", respectively.}

  \item{time}{time sequence for which output is wanted; the
  first value of times must be the initial time.}

  \item{ster.range}{optional sequence (between 0 and 1) of
  the sterilization rates to be simulated.}

  \item{...}{further arguments passed to
  \link[deSolve]{ode} function.}
}
\value{
  \code{\link{list}} of class \code{sterowned}. The first
  element, \code{*$model}, is the model function. The
  second, third and fourth elements are vectors
  (\code{*$pars}, \code{*$state}, \code{*$time},
  respectively) containing the \code{pars}, \code{state}
  and \code{time} arguments of the function. The fifth
  element \code{*$results} is a \code{\link{data.frame}}
  with up to as many rows as elements in time. First column
  contains the time, second column the population size and
  third column the proportion of sterilized animals. If
  \code{ster.range} is specified, fourth column contains
  the used sterilization rates.
}
\description{
  System of ordinary differential equations to simulate the
  effect of owned dogs sterilization on population
  dynamics.
}
\details{
  The implemented model is described by Amaku, et. al.,
  2009 and the function is a wrapper around the defaults of
  \link[deSolve]{ode} function, whose help page must be
  consulted for details.
}
\note{
  Logistic growth models are not intended for scenarios in
  which population size is greater than carrying capacity
  and growth rate is negative.
}
\examples{
# Parameters and initial conditions from estimates
# obtained in examples section from svysumm function.
pars.od <- c(b = 0.262, d = 0.075,
             k = 82531.831 * 1.1, s = .056)
state.od <- c(n = 82531.831, q = 0.207)

# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)

# Solve for a range of sterilization rates.
ster.range.od <- sterowned(pars = pars.od, state = state.od, time = 0:30, ster.range = seq(0, .4, length.out = 50))
}
\references{
  Amaku M, Dias R and Ferreira F (2009). Dinamica
  populacional canina: potenciais efeitos de campanhas de
  esterilizacao. Revista Panamericana de Salud Publica,
  25(4), pp. 300-304.

  Soetaert K, Cash J and Mazzia F (2012). Solving
  differential equations in R. Springer.
}
\seealso{
  \link[deSolve]{ode}.
}

