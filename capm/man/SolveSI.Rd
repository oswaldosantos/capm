\name{SolveSI}
\alias{SolveSI}
\title{Sterilization and immigration effects.}
\usage{
  SolveSI(pars = NULL, init = NULL, time = NULL, dd = "b",
    im = 0, ster.range = NULL, ...)
}
\arguments{
  \item{pars}{\code{\link{vector}} of length 4. The values
  are point estimates of birth rate, death rate, carrying
  capacity and sterilization rate. The names of this values
  must be "b", "d", "k" and "s", respectively.}

  \item{init}{\code{\link{vector}} of length 2. The values
  are initial population size and initial proportion of
  sterilized animals. The names of this values must be "n"
  and "q", respectively.}

  \item{time}{time sequence for which output is wanted; the
  first value of times must be the initial time.}

  \item{dd}{string equal to \code{b} or \code{d} to define
  if density-dependece act on birth or death rartes
  respectively.}

  \item{im}{a number representing the total of immigrants
  per time unit.}

  \item{ster.range}{optional sequence (between 0 and 1) of
  the sterilization rates to be simulated.}

  \item{...}{further arguments passed to
  \link[deSolve]{ode} function.}
}
\value{
  \code{\link{list}} of class \code{capmModels}. The first
  element, \code{*$model}, is the model function. The
  second, third and fourth elements are vectors
  (\code{*$pars}, \code{*$init}, \code{*$time},
  respectively) containing the \code{pars}, \code{init} and
  \code{time} arguments of the function. The fifth element
  \code{*$results} is a \code{\link{data.frame}} with up to
  as many rows as elements in time. First column contains
  the time, second column the population size and third
  column the proportion of sterilized animals. If
  \code{ster.range} is specified, fourth column contains
  the used sterilization rates.
}
\description{
  System of ordinary differential equations to simulate the
  effect of sterilization and immigration on population
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
# obtained in examples section from svysumm function but
# estimating a proportion insted of a total for births.
pars.SolveSI = c(b = 0.245, d = 0.101,
                     k = 98050.49, s = .048)
init.SolveSI = c(n = 89136.810, q = 0.198)

# Solve for a specific sterilization rate.
SolveSI.pt = SolveSI(pars = pars.SolveSI,
                             init = init.SolveSI,
                             time = 0:30, dd = 'b',
                             im = 100, method = 'rk4')

# Solve for a range of sterilization rates.
SolveSI.rg = SolveSI(pars = pars.SolveSI,
                             init = init.SolveSI,
                             time = 0:30, dd = 'b', im = 100,
                             ster.range = seq(0, .4, l = 50),
                             method = 'rk4')
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

