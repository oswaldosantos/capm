\name{plotmodel}
\alias{plotmodel}
\title{Plot results of capm model functions}
\usage{
  plotmodel(model.out = NULL, variable = NULL, col = "red",
    col1 = c("cadetblue1", "yellow", "red"),
    col2 = c("blue", "darkgreen", "darkred"),
    xlabel = "Time",
    scenlabel = "Imm = (__ * owned carrying capacity)",
    leglabel = NULL, ylabel = NULL, pop = NULL)
}
\arguments{
  \item{model.out}{output of one of the function previously
  mentioned.}

  \item{variable}{string to specify the variable to be
  ploted.

  For \code{\link{sterowned}} function:

  "n" (population size).

  "q" (proportion of sterilized animals).

  For \code{\link{rasa}} function using with only point
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

  For \code{\link{rasa}} function using *.range arguments:

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
  of \code{\link{rasa}} output.}

  \item{scenlabel}{string with the name of scenarios of
  \code{\link{rasa}} output, determined by the immigartion
  rates. Within the string, use the expression __ in the
  location where you want to appear the value of the
  immigartion rate. For line breaking, use \code{\\n} (see
  examples).}

  \item{pop}{a value indicating the output of
  \code{\link{rasa}} to be ploted. When \code{NULL}
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
  \code{\link{sterowned}} and \code{\link{rasa}}.
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

## Parameters and initial conditions from estimates
## obtained in examples section from svysumm function.
pars.od <- c(b = 0.242, d = 0.093,
             k = 88113.544 * 1.1, s = .048)
state.od <- c(n = 88113.544, q = 0.203)

# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od,
                     state = state.od, time = 0:30)

# Solve for a range of sterilization rates.
ster.range.od <- sterowned(
 pars = pars.od,
 state = state.od,
 time = 0:30,
 ster.range = seq(0, .4, length.out = 50))

## Plot the population size
plotmodel(ster.od, variable = 'n')
plotmodel(ster.range.od, variable = 'n')

## Plot the proportion of sterilized animals
plotmodel(ster.od, variable = 'q')
plotmodel(ster.range.od, variable = 'q')

################
## Example 2  ##
## rasa model ##
################

## Parameters and intial conditions.
pars.rasa = c(
   b1 = 21351.829, b2 = 3202.774,
   df1 = 0.081, dm1 = 0.069, df2 = 0.093, dm2 = 0.079,
   sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
   k1 = 96924.9, k2 = 9692.49, h1 = 1, h2 = .5,
   ab = 0.066, ad = 0.05, v = 0.12
)
state.rasa = c(
   f1 = 44576.299, cf1 = 11059.615,
   m1 = 43537.245, cm1 = 6827.960,
   f2 = 4457.630, cf2 = 222.8815,
   m2 = 4353.724, cm2 = 217.6862
)

# Solve for point estimates.
rasa.pt <- rasa(pars = pars.rasa,
                state = state.rasa,
                time = 0:30)

# Solve for parameter ranges.
rasa.rg <- rasa(pars = pars.rasa,
                state = state.rasa,
                time = 0:30,
                ster.range = seq(0, .5, length.out = 50),
                aban.range = c(0, .2),
                adop.range = c(0, .2),
                imm.range = c(0, .1))

## Plot stray population sizes using point estimates
plotmodel(rasa.pt, variable = "n2")

## Plot all scenarios and change the label for the scenarios.
plotmodel(rasa.rg, scenlabel = "Rec  \\n(__ * k)")
}
\references{
  Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
}
\seealso{
  \link[deSolve]{plot.deSolve}.
}

