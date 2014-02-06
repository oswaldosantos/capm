\name{CalculatePopChange}
\alias{CalculatePopChange}
\title{Population change.}
\usage{
  CalculatePopChange(model.out = NULL, variable = NULL,
    t1 = NULL, t2 = NULL, ratio = TRUE)
}
\arguments{
  \item{model.out}{an object of class \code{capmModels}.}

  \item{variable}{string with the name of the the output
  variable for which the change needs to be calculated.}

  \item{t1}{value specifying the first time.}

  \item{t2}{value specifying the second time.}

  \item{ratio}{logical. When \code{TRUE}, the calculated
  change is based on poulation size at t2 / population size
  at t1. When \code{FALSE}, the calculated change is based
  on poulation size at t2 / population size at t1.}
}
\value{
  Value representing the ratio (if \code{ratio} is
  \code{TRUE}) or the difference (if \code{ratio} is
  \code{FALSE}) between population size at time t2 and t1.
  If only time time is specified, the value is the
  population size at that time.
}
\description{
  Calculate the change in population size between two
  times. When only one time is specified, the population
  size at moment is returned.
}
\examples{
# Parameters and initial conditions from estimates
# obtained in examples section from svysumm function but
# estimating a proportion insted of a total for births.
pars.solve.si = c(b = 0.245, d = 0.101,
                     k = 98050.49, s = .048)
init.solve.si = c(n = 89136.810, q = 0.198)

# Solve for a specific sterilization rate.
solve.si.pt = SolveSI(pars = pars.solve.si,
                             init = init.solve.si,
                             time = 0:30, dd = 'b',
                             im = 100, method = 'rk4')

# Calculate the population change (ratio) between times 0 and 30.
CalculatePopChange(solve.si.pt, variable = n, t2 = 30, t1 = 0)

# Calculate the population change (difference) between times 0 and 30.
CalculatePopChange(solve.si.pt, variable = n, t2 = 30, t1 = 0, ratio = F)

# Calculate the population zises at time 30.
CalculatePopChange(solve.si.pt, variable = n, t2 = 30)
}

