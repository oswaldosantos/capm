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
pars.od <- c(b = 0.262, d = 0.075,
             k = 82531.831 * 1.1, s = .056)
state.od <- c(n = 82531.831, q = 0.207)

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
   bf1 = 0.262, bm1 = 0.262, bf2 = 0.288, bm2 = 0.288,
   df1 = 0.081, dm1 = 0.069, df2 = 0.089, dm2 = 0.076,
   sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
   k1 = 90785.01, k2 = 9078.501, h1 = 1, h2 = 1,
   ab = 0.065, ad = 0.095, r = 0.111
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

