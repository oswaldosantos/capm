\name{plotmodel}
\alias{plotmodel}
\title{Plot results of capm model functions}
\usage{
  plotmodel(model.out = NULL, variable = NULL, col = "red",
    col1 = c("yellow", "blue"),
    col2 = c("green", "orangered1"), xlabel = "Time",
    scenlabel = "Recruitment rate =\n (__ * owned carrying capacity)",
    leglabel = c("Owned \npop size", "Stray \npop size"),
    ylabel = NULL, pop = NULL)
}
\arguments{
  \item{model.out}{output of one of the function previously
  mentioned.}

  \item{variable}{string to specify the variable to be
  ploted. If function is \code{\link{sterowned}}, possible
  values are: "n" (population size) or "q" (proportion of
  sterilized animals). For \code{\link{rasa}} function,
  possible values are: "n" (total population size), "n1"
  (owned population size), "n2" (stray population size),}

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
  \code{\link{rasa}} output, determined by the recruitment
  rates. Within the string, use the expression __ in the
  location where you want to appear the value of the
  recruitment rate. For line breaking, use \code{\\n} (see
  examples).}

  \item{pop}{a value indicating the output of
  \code{\link{rasa}} to be ploted. When \code{NULL}
  (default), plots for owned and stray populations under
  scenarios created by recruitment rate are created. If
  \code{1}, the plots of owned population for the minimum
  recruitment rate are ploted. When \code{2}, the plots of
  stray population for the minimum recruitment rate are
  ploted. If \code{3}, the plots of owned population for
  the maximum recruitment rate are ploted. When \code{4},
  the plots of owned population for the maximum recruitment
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
pars.od <- c(b = 0.262, d = 0.075,
             k = 82531.831 * 1.1, s = .056)
state.od <- c(n = 82531.831, q = 0.207)

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

# Solve for parameter ranges.
rasa.rg <- rasa(pars = pars.rasa,
                state = state.rasa,
                time = 0:30,
                ster.range = seq(0, .5, length.out = 50),
                aban.range = c(0, .2),
                adop.range = c(0, .2),
                recr.range = c(0, .1))

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

