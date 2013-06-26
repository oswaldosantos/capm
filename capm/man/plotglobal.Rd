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
##################
## Example 1    ##
## sterim model ##
##################

## Parameters and intial conditions from estimates
## obtained in examples section from the svysumm function.
pars.sterim = c(b = 0.245, d = 0.101,
             k = 98050.49, s = .048)
state.sterim = c(n = 89136.810, q = 0.198)

# Solve for a specific sterilization rate.
sterim.pt = sterim(pars = pars.sterim, state = state.sterim,
                  time = 0:30, dd = 'b', imm = 100)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.sterim.1 = setranges(pars = pars.sterim)

## Calculate golobal sensitivity of combined parameters.
glob.sterim.all = globalsens(
  model.out = sterim.pt, ranges = par.rg.sterim.1,
  sensv = 'n', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.sterim = globalsens(model.out = sterim.pt,
ranges = par.rg.sterim.1, sensv = 'n')

### Plot the sensitivities of combined parameters.
plotglobal(glob.sterim.all)

### Plot the sensitivities of individual parameters.
plotglobal(glob.sterim)

################
## Example 2  ##
## iasa model ##
################

## Parameters and intial conditions.
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

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.iasa.1 = setranges(pars = pars.iasa)

## Calculate golobal sensitivity of combined parameters.
glob.iasa.all = globalsens(
  model.out = iasa.pt,
  ranges = par.rg.iasa.1, sensv = 'n2', all = TRUE)

## Calculate golobal sensitivity of individual parameters.
glob.iasa = globalsens(
  model.out = iasa.pt, ranges = par.rg.iasa.1,
  sensv = 'n2')

### Plot the sensitivities of combined parameters.
plotglobal(glob.iasa.all)

### Plot the sensitivities of individual parameters.
plotglobal(glob.iasa)
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
}
\seealso{
  \link[deSolve]{plot.deSolve}.
}

