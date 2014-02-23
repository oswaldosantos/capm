\name{PlotLocalSens}
\alias{PlotLocalSens}
\title{Plot results of CalculateLocalSens function}
\usage{
PlotLocalSens(local.out = NULL, x.sens = "Time", y.sens = "Sensitivity",
  y.ind = c("L1", "L2", "Mean", "Min", "Max"), ax.size = 10, type = 6)
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

  \item{y.ind}{string with the name of y axis for the
  parameter importance indices.}

  \item{ax.size}{a number to specify the size of axes
  labels and text.}
}
\description{
Plot results of the \code{\link{CalculateLocalSens}}
function.
}
\details{
Font size of saved plots is usually different to the font
size seen in graphic browsers. Before changing font sizes,
see the final result in saved (or preview) plots.
}
\examples{
#####################
## SolveIASA model ##
#####################

## Parameters and intial conditions.
pars.solve.iasa = c(
   b1 = 21870.897, b2 = 4374.179,
   df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
   sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
   k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
   ab = 0.054, ad = 0.1, v = 0.2, z = 0.1)

init.solve.iasa = c(
   f1 = 33425.19, fs1 = 10864.901,
   m1 = 38038.96, ms1 = 6807.759,
   f2 = 3342.519, fs2 = 108.64901,
   m2 = 3803.896, ms2 = 68.07759)


# Solve for point estimates.
solve.iasa.pt <- SolveIASA(pars = pars.solve.iasa,
                          init = init.solve.iasa,
                          time = 0:15, method = 'rk4')

## Calculate local sensitivities to all parameters.
local.solve.iasa2 <- CalculateLocalSens(
  model.out = solve.iasa.pt, sensv = 'n2')

## Plot local sensitivities
# Uncomment the following line:
# PlotLocalSens(local.solve.iasa2)
}
\references{
Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.

Soetaert K, Cash J and Mazzia F (2012). Solving
differential equations in R. Springer.

\url{http://oswaldosantos.github.io/capm}
}
\seealso{
\link[FME]{plot.sensFun}.
}

