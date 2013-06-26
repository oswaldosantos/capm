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

## Calculate local sensitivities for all parameters.
local.sterim = localsens(model.out = sterim.pt, sensv = 'n')

## Plot local sensitivities
plotlocal(local.sterim)

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

## Calculate local sensitivities for all parameters.
local.iasa2 = localsens(model.out = iasa.pt, sensv = 'n2')

## Plot local sensitivities
plotlocal(local.iasa2)
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.

  Soetaert K, Cash J and Mazzia F (2012). Solving
  differential equations in R. Springer.
}
\seealso{
  \link[FME]{plot.sensFun}.
}

