\name{setranges}
\alias{setranges}
\title{Parameter ranges for global sensitivity analysis}
\usage{
  setranges(pars = NULL, range = 0.1, estimates = NULL,
    rowpars = NULL)
}
\arguments{
  \item{pars}{the same \code{pars} vector used in one of
  the following functions: \code{\link{sterowned}}.}

  \item{range}{scale factor to define the minimum and
  maximum for each parameter. The default is 0.1, which set
  the minimum and maximum as 10 percent lesser and greater
  that the \code{pars} values.}

  \item{estimates}{output from the \code{\link{svysumm}}
  function (see detials).}

  \item{rowpars}{\code{\link{vector}} with values
  corresponding to the row positions of \code{estimates},
  containing estimates for one or more parameters of
  \code{pars}. The name of the values must be equal to the
  name of the corresponding parameters in \code{pars} (see
  detials and examples).}
}
\description{
  Define the minimum and maximum values for parameters
  whose global sensitivities are to be assesses with
  \code{\link{globalsens}} or \code{\link{sensRange}}
  functions.
}
\details{
  \code{estimates} and \code{rowpars} are optional
  arguments to be used together. Ranges for all parameters
  are initially defined acording to \code{range}. If
  \code{estimates} is used, the confidence limits for the
  parameters specified in \code{rowpars} will replace the
  corresponding ranges initially dfined by \code{range}.
}
\examples{
##### exemplo 1 - SterOwned function ####

## Parameters and initial conditions from estimates
## obtained in examples section from svysumm function.
pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
state.od <- c(n = 125027.411, q = 0.188)

# Solve for a specific sterilization rate.
ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)

# Solve for a range of sterilization rates.
ster.range.od <- sterowned(pars = pars.od, state = state.od, time = 0:30, ster.range = seq(0, .4, length.out = 50))

### Set ranges 10 \% greater and lesser than the
## point estimates.
par.ranges.1 = setranges(pars = pars.od)

### Set ranges based on estimated confidence limits.

## First, estimate parameters.
# Load data with psu identifiers and sizes.
data(psu.ssu)

# Load data with sample data.
data(Sample)

# Specify the two-stage cluster design.
design <- svyd2(psu.ssu, Sample, psu.col = 2, ssu.col = 1)

# Look at the variables contained in the survey design
names(design$variables)

# Specify the type of estimate for each variable
variables <- c("", "", "total", "prop", "mean", rep("prop", 8), "", "")

# Make sure you specify the correct type of estimate for each variable
cbind(names(design$variables), variables)

# Calculate the summary statistics for the survey
estimates <- svysumm(design, variables = variables, rnd = 3)
## Then:
# See which rows contains the estimates of interest.
data.frame(rownames(estimates))

# Estimaes of birth, death and sterilization rates are
# in rows 13, 17 and 8.
par.ranges = setranges(pars = pars.od, range = .1, estimates = estimates, rowpars = c('b' = 13, 'd' = 17, 's' = 8))
}
\references{
  Soetaert K and Petzoldt T (2010). Inverse modelling,
  sensitivity and monte carlo analysis in R using package
  FME. Journal of Statistical Software, 33(3), pp. 1-28.

  Reichert P and Kfinsch HR (2001). Practical
  identifiability analysis of large environmental
  simulation models. Water Resources Research, 37(4), pp.
  1015-1030.
}
\seealso{
  \code{\link{sensRange}} and \code{\link{sterowned}}.
}

