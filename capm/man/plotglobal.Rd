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
#####################
## Example 1       ##
## sterowned model ##
#####################

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.od <- c(b = 0.262, d = 0.075,
             k = 82531.831 * 1.1, s = .056)
state.od <- c(n = 82531.831, q = 0.207)

# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od,
                     state = state.od, time = 0:30)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.od.1 = setranges(pars = pars.od)

## Calculate golobal sensitivity of combined parameters.
glob.od.all = globalsens(
  model.out = ster.od, ranges = par.rg.od.1,
  sensv = 'n', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.od = globalsens(model.out = ster.od,
ranges = par.rg.od.1, sensv = 'n')

### Plot the sensitivities of combined parameters.
plotglobal(glob.od.all)

### Plot the sensitivities of individual parameters.
plotglobal(glob.od)

################
## Example 2  ##
## rasa model ##
################

## Parameters and intial conditions.
pars.rasa = c(
   b1 = 21585.15, b2 = 2374.367,
   df1 = 0.081, dm1 = 0.069, df2 = 0.089, dm2 = 0.076,
   sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
   k1 = 90785.01, k2 = 9078.501, h1 = 1, h2 = 0.5,
   ab = 0.065, ad = 0.095, v = 0.111
)
state.rasa = c(
   f1 = 41641.785, cf1 = 8423.503,
   m1 = 40890.046, cm1 = 8647.503,
   f2 = 4164.179, cf2 = 208.209,
   m2 = 4089.005, cm2 = 204.45
)

# Solve for point estimates.
rasa.pt <- rasa(pars = pars.rasa,
                state = state.rasa,
                time = 0:30)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.rasa.1 = setranges(pars = pars.rasa)

## Calculate golobal sensitivity of combined parameters.
glob.rasa.all = globalsens(
  model.out = rasa.pt,
  ranges = par.rg.rasa.1, sensv = 'n2', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.rasa = globalsens(
  model.out = rasa.pt, ranges = par.rg.rasa.1,
  sensv = 'n2')

### Plot the sensitivities of combined parameters.
plotglobal(glob.rasa.all)

### Plot the sensitivities of individual parameters.
plotglobal(glob.rasa)
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
}
\seealso{
  \link[deSolve]{plot.deSolve}.
}

