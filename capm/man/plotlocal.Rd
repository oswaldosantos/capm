\name{plotlocal}
\alias{plotlocal}
\title{Plot results of localsens function}
\usage{
  plotlocal(local.out = NULL, type = 1, sens.x = "Time",
    sens.y = "Sensitivity",
    ind.y = c("L1", "L2", "Mean", "Min", "Max"),
    size.ax = 10)
}
\arguments{
  \item{local.out}{output from \code{\link{localsens}}
  function.}

  \item{type}{a number to define the type of graphical
  output. When equal to \code{1}, both the sensitivity
  functions and all importance indices are ploted. If equal
  to \code{2}, the sensitivity function for each parameter
  and the importance indices L1 and L2 are ploted and if
  equal to \code{3}, just sensitivity functions are
  ploted.}

  \item{sens.x}{string with the name of x axis for
  sensitivity functions.}

  \item{sens.y}{string with the name of y axis for
  sensitivity functions.}

  \item{ind.y}{\code{\link{character}} \code{\link{vector}}
  with the name of y axis for the parameter importance
  indices.}

  \item{size.ax}{a number to specify the size of axes
  labels and text.}
}
\description{
  Plot results of the \code{\link{localsens}} function.
}
\details{
  Font size of saved plots is usually different to the font
  size seen in graphic browsers. Before changing font
  sizes, see the final result in saved (or preview) plots.
}
\examples{
#####################
## Example 1       ##
## sterowned model ##
#####################

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
state.od <- c(n = 125027.411, q = 0.188)

# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)

## Calculate local sensitivities for all parameters.
local.od = localsens(model.out = ster.od, sensv = 'n')

## Plot local sensitivities
plotlocal(local.od)

################
## Example 2  ##
## rasa model ##
################

## Parameters and intial conditions.
pars.rasa = c(
af1 = 0.219, am1 = 0.219, af2 = 0.241, am2 = 0.241,
bf1 = 0.091, bm1 = 0.091, bf2 = 0.1, bm2 = 0.1,
ef1 = 0.074, ef2 = 0.01, em1 = 0.047, em2 = 0.01,
k1 = 137176.8, k2 = 13854.86, z1 = 1, z2 = 1,
h = 0.051, j = 0.111, v = 0.1
)
state.rasa = c(
  f1 = 46181.12, sf1 = 13309.497,
  m1 = 49681.91, sm1 = 15533.682,
  f2 = 5949.062, sf2 = 59.491,
  m2 = 6521.56, sm2 = 65.216
)

# Solve for point estimates.
rasa.pt <- rasa(pars = pars.rasa,
                state = state.rasa,
                time = 0:30)

## Calculate local sensitivities for all parameters.
local.rasa2 = localsens(model.out = rasa.pt, sensv = 'n2')

## Plot local sensitivities
plotlocal(local.rasa2)
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.

  Soetaert K, Cash J and Mazzia F (2012). Solving
  differential equations in R. Springer.
}
\seealso{
  \link[FME]{plot.sensFun}.
}

