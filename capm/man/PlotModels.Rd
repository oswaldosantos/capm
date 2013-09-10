\name{PlotModels}
\alias{PlotModels}
\title{Plot results of capm model functions}
\usage{
  PlotModels(model.out = NULL, variable = NULL,
    col = "red", col1 = c("cadetblue1", "yellow", "red"),
    col2 = c("blue", "darkgreen", "darkred"),
    xlabel = "Years", ylabel = NULL,
    scenlabel = "Im = (__ * owned carrying capacity)",
    leglabel = NULL, pop = NULL)
}
\arguments{
  \item{model.out}{output of one of the function previously
  mentioned.}

  \item{variable}{string to specify the variable to be
  ploted.

  For \code{\link{SolveSterIm}} function:

  "n" (population size).

  "q" (proportion of sterilized animals).

  For \code{\link{SolveIASA}} function using with only
  point estimates:

  "f1" (owned intact females).

  "cf1" (owned sterilized females).

  "m1" (owned intact males).

  "cm1" (owned sterilized males).

  "f2" (stray intact females).

  "cf2" (stray sterilized females).

  "m2" (stray intact males).

  "cm2" (stray sterilized males).

  "n1" (owned intact animals).

  "cn1" (oned sterilized animals).

  "n2" (stray intact animals).

  "cn2" (stray sterilized animals).

  "N1" (owned animals).

  "N2" (stray animals).

  "N" (total population).

  For \code{\link{SolveIASA}} function using *.range
  arguments:

  "f" (intact females).

  "cf" (sterilized females).

  "m" (intact males).

  "cm" (sterilized males).

  "n" (intact animals).

  "cn" (sterilized animals).

  "N" (Total population stratified by reproductive
  status).}

  \item{col}{string indicating the color of ploted line,
  when \code{ster.range} is \code{NULL}.}

  \item{col1}{\code{\link{character}} \code{\link{vector}}
  indicating the color of lowest (highest) population sizes
  (proportion of sterilized animals), when
  \code{ster.range} is not \code{NULL}.}

  \item{col2}{\code{\link{character}} \code{\link{vector}}
  indicating the color of lowest (highest) population sizes
  (proportion of sterilized animals), when
  \code{ster.range} is not \code{NULL}.}

  \item{xlabel}{string with the name of x axis.}

  \item{ylabel}{string with the name of y axis.}

  \item{leglabel}{string with the name of legend, for plots
  of \code{\link{SolveIASA}} output.}

  \item{scenlabel}{string with the name of scenarios of
  \code{\link{SolveIASA}} output, determined by the
  immigartion rates. Within the string, use the expression
  __ in the location where you want to appear the value of
  the immigartion rate. For line breaking, use \code{\\n}
  (see examples).}

  \item{pop}{a value indicating the output of
  \code{\link{SolveIASA}} to be ploted. When \code{NULL}
  (default), plots for owned and stray populations under
  scenarios created by immigartion rate are created. If
  \code{1}, the plots of owned population for the minimum
  immigartion rate are ploted. When \code{2}, the plots of
  stray population for the minimum immigartion rate are
  ploted. If \code{3}, the plots of owned population for
  the maximum immigartion rate are ploted. When \code{4},
  the plots of owned population for the maximum immigartion
  rate are ploted.}
}
\description{
  Plot results of the following functions:
  \code{\link{SolveSterIm}} and \code{\link{SolveIASA}}.
}
\details{
  Font size of saved plots is usually different to the font
  size seen in graphic browsers. Before changing font
  sizes, see the final result in saved (or preview) plots.

  Other details of the plot can be modifyed using
  appropriate functions from \code{ggplot2} package.
}
\examples{
#######################
## Example 1         ##
## SolveSterIm model ##
#######################

## Parameters and initial conditions from estimates
## obtained in examples section from svysumm function.
pars.SolveSterIm = c(b = 0.245, d = 0.101,
                     k = 98050.49, s = .048)
init.SolveSterIm = c(n = 89136.810, q = 0.198)

# Solve for a specific sterilization rate.
SolveSterIm.pt = SolveSterIm(pars = pars.SolveSterIm,
                             init = init.SolveSterIm,
                             time = 0:30, dd = 'b',
                             im = 100, method = 'rk4')

# Solve for a range of sterilization rates.
SolveSterIm.rg = SolveSterIm(pars = pars.SolveSterIm,
                             init = init.SolveSterIm,
                             time = 0:30, dd = 'b', im = 100,
                             ster.range = seq(0, .4, l = 50))

## Plot the population size
PlotModels(SolveSterIm.pt, variable = 'n')
PlotModels(SolveSterIm.rg, variable = 'n')

## Plot the proportion of sterilized animals
PlotModels(SolveSterIm.pt, variable = 'q')
PlotModels(SolveSterIm.rg, variable = 'q')

#####################
## Example 2       ##
## SolveIASA model ##
#####################

## Parameters and intial conditions.
pars.SolveIASA = c(
   b1 = 21870.897, b2 = 4374.179,
   df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
   sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
   k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
   ab = 0.054, ad = 0.1, v = 0.2, vc = 0.1)

init.SolveIASA = c(
   f1 = 33425.19, cf1 = 10864.901,
   m1 = 38038.96, cm1 = 6807.759,
   f2 = 3342.519, cf2 = 108.64901,
   m2 = 3803.896, cm2 = 68.07759)


# Solve for point estimates.
SolveIASA.pt <- SolveIASA(pars = pars.SolveIASA,
                          init = init.SolveIASA,
                          time = 0:30)

# Solve for parameter ranges.
SolveIASA.rg <- SolveIASA(pars = pars.SolveIASA,
                          init = init.SolveIASA,
                          time = 0:20,
                          ster.range = seq(0, .4, l = 20),
                          aban.range = c(0, .2),
                          adop.range = c(0, .2),
                          im.range = c(0, .1),
                          method = 'rk4')

## Plot stray population sizes using point estimates
PlotModels(SolveIASA.pt, variable = "n2")

## Plot all scenarios and change the label for the scenarios.
PlotModels(SolveIASA.rg, variable = 'N')
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
}
\seealso{
  \link[deSolve]{plot.deSolve}.
}

