\name{plotmodel}
\alias{plotmodel}
\title{Plot results of capm model functions}
\usage{
  plotmodel(model.out = NULL, variable = NULL, col = "red",
    col1 = c("cadetblue1", "yellow", "red"),
    col2 = c("blue", "darkgreen", "darkred"),
    xlabel = "Years",
    scenlabel = "Imm = (__ * owned carrying capacity)",
    leglabel = NULL, ylabel = NULL, pop = NULL)
}
\arguments{
  \item{model.out}{output of one of the function previously
  mentioned.}

  \item{variable}{string to specify the variable to be
  ploted.

  For \code{\link{sterim}} function:

  "n" (population size).

  "q" (proportion of sterilized animals).

  For \code{\link{iasa}} function using with only point
  estimates:

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

  For \code{\link{iasa}} function using *.range arguments:

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
  of \code{\link{iasa}} output.}

  \item{scenlabel}{string with the name of scenarios of
  \code{\link{iasa}} output, determined by the immigartion
  rates. Within the string, use the expression __ in the
  location where you want to appear the value of the
  immigartion rate. For line breaking, use \code{\\n} (see
  examples).}

  \item{pop}{a value indicating the output of
  \code{\link{iasa}} to be ploted. When \code{NULL}
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
  \code{\link{sterim}} and \code{\link{iasa}}.
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
## sterim model ##
#####################

## Parameters and initial conditions from estimates
## obtained in examples section from svysumm function.
pars.sterim = c(b = 0.245, d = 0.101,
             k = 98050.49, s = .048)
state.sterim = c(n = 89136.810, q = 0.198)

# Solve for a specific sterilization rate.
sterim.pt = sterim(pars = pars.sterim, state = state.sterim,
                  time = 0:30, dd = 'b', imm = 100)

# Solve for a range of sterilization rates.
sterim.rg = sterim(
   pars = pars.sterim, state = state.sterim,
   time = 0:30, dd = 'b', imm = 100,
   ster.range = seq(0, .4, length.out = 50))

## Plot the population size
plotmodel(sterim.pt, variable = 'n')
plotmodel(sterim.rg, variable = 'n')

## Plot the proportion of sterilized animals
plotmodel(sterim.pt, variable = 'q')
plotmodel(sterim.rg, variable = 'q')

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
   ab = 0.054, ad = 0.1, v = 0.2, vc = 0.1
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

# Solve for parameter ranges.
iasa.rg <- iasa(pars = pars.iasa,
                state = state.iasa,
                time = 0:30,
                ster.range = seq(0, .5, length.out = 50),
                aban.range = c(0, .2),
                adop.range = c(0, .2),
                imm.range = c(0, .1))

## Plot stray population sizes using point estimates
plotmodel(iasa.pt, variable = "n2")

## Plot all scenarios and change the label for the scenarios.
plotmodel(iasa.rg, variable = 'N')
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
}
\seealso{
  \link[deSolve]{plot.deSolve}.
}

