\name{plotglobal}
\alias{plotglobal}
\title{Plot results of GlobalSens function}
\usage{
  plotglobal(global.out = NULL,
    nam.leg = "Sensitivity range", mm.leg = "min - max",
    sd.leg = "mean +- sd   ")
}
\arguments{
  \item{global.out}{output from \code{\link{globalsens}}
  function.}

  \item{nam.leg}{string with name for the legend.}

  \item{mm.leg}{string with the name of the "envelope"
  calculated using the minimum and maximum ranges.}

  \item{sd.leg}{string with the name of the "envelope"
  calculated using the mean +- standard deviation ranges.}
}
\description{
  Plot results of of \code{\link{globalsens}} function.
}
\details{
  Font size of saved plots is usually different to the font
  size seen in graphic browsers. Before changing font
  sizes, see the final result in saved (or preview) plots.

  Other details of the plot can be modifyed using
  appropriate functions from \code{ggplot2} package.
}
\examples{
#### example 1 - sterowned function results ####

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
state.od <- c(n = 125027.411, q = 0.188)

# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.ranges.1 = setranges(pars = pars.od)

## Calculate golobal sensitivity of combined parameters.
glob.od.all = globalsens(model.out = ster.od, ranges = par.ranges.1, all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.od = globalsens(model.out = ster.od, ranges = par.ranges.1)

### Plot the sensitivities of combined parameters.
plotglobal(glob.od.all)

### Plot the sensitivities of individual parameters.
plotglobal(glob.od)
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
}
\seealso{
  \link[deSolve]{plot.deSolve}.
}

