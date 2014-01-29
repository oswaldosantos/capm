\name{PlotLocalSens}
\alias{PlotLocalSens}
\title{Plot results of CalculateLocalSens function}
\usage{
  PlotLocalSens(local.out = NULL, x.sens = "Time",
    y.sens = "Sensitivity",
    y.ind = c("L1", "L2", "Mean", "Min", "Max"),
    ax.size = 10, type = 6)
}
\arguments{
  \item{local.out}{output from
  \code{\link{CalculateLocalSens}} function.}

  \item{type}{a number to define the type of graphical
  output. \code{6}: sensitivity functions and all
  importance indices are ploted; \code{1}: importance index
  L1; \code{2}: importance index L2; \code{3}: mean of
  sensitivity functions; \code{5}: minimum of sensitivity
  functions; and \code{5}: maximum of sensitivity
  functions.}

  \item{x.sens}{string with the name of x axis for
  sensitivity functions.}

  \item{y.sens}{string with the name of y axis for
  sensitivity functions.}

  \item{y.ind}{\code{\link{character}} \code{\link{vector}}
  with the name of y axis for the parameter importance
  indices.}

  \item{ax.size}{a number to specify the size of axes
  labels and text.}
}
\description{
  Plot results of the \code{\link{CalculateLocalSens}}
  function.
}
\details{
  Font size of saved plots is usually different to the font
  size seen in graphic browsers. Before changing font
  sizes, see the final result in saved (or preview) plots.
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
                      im = 100, method = 'rk4')

## Calculate local sensitivities for all parameters.
local.solvesi <- CalculateLocalSens(
  model.out = solvesi.pt, sensv = 'n')

## Plot local sensitivities
PlotLocalSens(local.solvesi)

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
   ab = 0.054, ad = 0.1, v = 0.2, z = 0.1)

init.solveiasa <- c(
   f1 = 33425.19, cf1 = 10864.901,
   m1 = 38038.96, cm1 = 6807.759,
   f2 = 3342.519, cf2 = 108.64901,
   m2 = 3803.896, cm2 = 68.07759)

# Solve for point estimates.
solveiasa.pt <- SolveIASA(pars = pars.solveiasa,
                          init = init.solveiasa,
                          time = 0:30, method = 'rk4')

## Calculate local sensitivities for all parameters.
local.solveiasa2 <- CalculateLocalSens(
  model.out = solveiasa.pt, sensv = 'n2')
local.solveiasa1 <- CalculateLocalSens(
  model.out = solveiasa.pt, sensv = 'n1')

## Plot local sensitivities
PlotLocalSens(local.solveiasa2)
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.

  Soetaert K, Cash J and Mazzia F (2012). Solving
  differential equations in R. Springer.
}
\seealso{
  \link[FME]{plot.sensFun}.
}

