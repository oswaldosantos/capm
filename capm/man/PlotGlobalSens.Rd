\name{PlotGlobalSens}
\alias{PlotGlobalSens}
\title{Plot results of GlobalSens function}
\usage{
  PlotGlobalSens(global.out = NULL, xlabel = "Time",
    ylabel = "Population", nam.leg = "Sensitivity range",
    mm.leg = "min - max", sd.leg = "mean +- sd   ")
}
\arguments{
  \item{global.out}{output from
  \code{\link{CalculateGlobalSens}} function.}

  \item{nam.leg}{string with name for the legend.}

  \item{xlabel}{string with the name of x axis.}

  \item{ylabel}{string with the name of y axis.}

  \item{mm.leg}{string with the name of the "envelope"
  calculated using the minimum and maximum ranges.}

  \item{sd.leg}{string with the name of the "envelope"
  calculated using the mean +- standard deviation ranges.}
}
\description{
  Plot results of of \code{\link{CalculateGlobalSens}}
  function.
}
\details{
  Font size of saved plots is usually different to the font
  size seen in graphic browsers. Before changing font
  sizes, see the final result in saved (or preview) plots.

  Other details of the plot can be modifyed using
  appropriate functions from \code{ggplot2} package.
}
\examples{
###################
## Example 1     ##
## SolveSI model ##
###################

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.solvesi <- c(b = 0.245, d = 0.101,
                      k = 98050.49, s = .048)
init.solvesi <- c(n = 89136.810, q = 0.198)

# Solve for a specific sterilization rate.
solvesi.pt <- SolveSI(pars = pars.solvesi,
                      init = init.solvesi,
                      time = 0:30, dd = 'b',
                      im = 100, , method = 'rk4')

## Set ranges 10 \% greater and lesser than the
## point estimates.
rg.solvesi <- SetRanges(pars = pars.solvesi)

## Calculate golobal sensitivity of combined parameters.
glob.all.solvesi <- CalculateGlobalSens(
  model.out = solvesi.pt,
  ranges = rg.solvesi,
  sensv = 'n', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.solvesi <- CalculateGlobalSens(
  model.out = solvesi.pt,
  ranges = rg.solvesi, sensv = 'n')

#' ### Plot the sensitivities of combined parameters.
PlotGlobalSens(glob.all.solvesi)

### Plot the sensitivities of individual parameters.
PlotGlobalSens(glob.solvesi)

#####################
## Example 2       ##
## SolveIASA model ##
#####################

## Parameters and intial conditions.
pars.solveiasa <- c(
   b1 = 21870.897, b2 = 4374.179,
   df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
   sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
   k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
   ab = 0.054, ad = 0.1, v = 0.2, vc = 0.1)

init.solveiasa <- c(
   f1 = 33425.19, cf1 = 10864.901,
   m1 = 38038.96, cm1 = 6807.759,
   f2 = 3342.519, cf2 = 108.64901,
   m2 = 3803.896, cm2 = 68.07759)

# Solve for point estimates.
solveiasa.pt <- SolveIASA(pars = pars.solveiasa,
                          init = init.solveiasa,
                          time = 0:30, method = 'rk4')

## Set ranges 10 \% greater and lesser than the
## point estimates.
rg.solveiasa <- SetRanges(pars = pars.solveiasa)

## Calculate golobal sensitivity of combined parameters.
glob.solveiasa <- CalculateGlobalSens(
  model.out = solveiasa.pt,
  ranges = rg.solveiasa,
  sensv = 'n2', all = TRUE)

## Calculate golobal sensitivity of an individual parameter.
glob.solveiasa <- CalculateGlobalSens(
  model.out = solveiasa.pt,
  ranges = rg.solveiasa,
  sensv = 'n2')

### Plot the sensitivities of combined parameters.
PlotGlobalSens(glob.all.solveiasa)

### Plot the sensitivities of individual parameters.
PlotGlobalSens(glob.solveiasa)
}
\seealso{
  \link[deSolve]{plot.deSolve}.
}

