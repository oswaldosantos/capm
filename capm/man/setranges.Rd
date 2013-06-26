\name{setranges}
\alias{setranges}
\title{Parameter ranges for global sensitivity analysis}
\usage{
  setranges(pars = NULL, range = 0.1, estimates = NULL,
    rowpars = NULL, verbose = FALSE)
}
\arguments{
  \item{pars}{the same \code{pars} vector used in one of
  the following functions: \code{\link{sterim}} or
  \code{\link{iasa}}.}

  \item{range}{scale factor to define the minimum and
  maximum for each parameter. The default is 0.1, which set
  the minimum and maximum as 10 percent lesser and greater
  than the \code{pars} values.}

  \item{estimates}{output from the \code{\link{svysumm}}
  function (see detials).}

  \item{rowpars}{\code{\link{vector}} with values
  corresponding to the row positions of \code{estimates},
  containing estimates for one or more parameters of
  \code{pars}. The name of the values must be equal to the
  name of the corresponding parameters in \code{pars} (see
  detials and examples).}

  \item{verbose}{logical idnicating if alteratinos must be
  informed.}
}
\value{
  a \code{\link{data.frame}} with the complete set of
  parameter ranges, when \code{verbose} is \code{FALSE} or
  rowpars is \code{NULL}. If \code{verbose} is \code{TRUE}
  and rowpars is not \code{NULL}, a \code{\link{list}} with
  information about the changed ranges (first element) and
  the complete set of parameter ranges.
}
\description{
  Define the minimum and maximum values for parameters
  whose global sensitivities are to be assesses with
  \code{\link{globalsens}} or \code{\link{sensRange}}
  functions.
}
\details{
  \code{estimates} and \code{rowpars} are optional
  arguments to be used together. When used, \code{pars}
  must be a \code{setrange} output. The confidence limits
  for the parameters specified in \code{rowpars} will
  replace the corresponding ranges initially dfined.
}
\note{
  Logistic growth models are not intended for scenarios in
  which population size is greater than carrying capacity
  and growth rate is negative.
}
\examples{
data(psu.ssu)
data(Sample)

##################
## Example 1    ##
## sterim model ##
##################

## Parameters and initial conditions from estimates
## obtained in examples section from svysumm function.
pars.sterim = c(b = 0.245, d = 0.101,
             k = 98050.49, s = .048)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.sterim.1 = setranges(pars = pars.sterim)

## Set ranges based on estimated confidence limits.

# Specify the two-stage cluster design.
design <- svyd2(psu.ssu, Sample, psu.col = 2,
                ssu.col = 1)

# Look at the variables contained in the survey design
names(design$variables)

# Specify the type of estimate for each variable
variables <- c("", "", "total", "prop", "mean",
               rep("prop", 7), "", "")

# Make sure you specify the correct type of
# estimate for each variable
cbind(names(design$variables), variables)

# Calculate the summary statistics for the survey
estimates <- svysumm(design, variables, rnd = 3)

## Then:
# See which rows contain the estimates of interest.
data.frame(rownames(estimates))

# Estimaes of birth, death and sterilization rates are
# in rows 11, 15 and 8.
par.rg.sterim.1 = setranges(pars = pars.sterim)
par.rg.sterim = setranges(
   pars = par.rg.sterim.1,
   estimates = estimates,
   rowpars = c('b' = 11, 'd' = 15, 's' = 8))

#####################
## Example 2       ##
## iasa model      ##
#####################

## Parameters and initial conditions.
pars.iasa = c(
   b1 = 21870.897, b2 = 4374.179,
   df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
   sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
   k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
   ab = 0.054, ad = 0.1, v = 0.1
)

## Set ranges 10 \% greater and lesser than the
## point estimates.
par.rg.iasa.1 = setranges(pars.iasa)

## Set ranges based on estimated confidence limits.

# Prepare the data (see svysumm examples).
sample1 = Sample[, c(1:4, 6:7, 12)]
sample1[, 5] = as.character(sample1[, 5])
sample1[which(sample1$castrated == "yes"), 5] = 1
sample1[which(sample1[, 5] == "no"), 5] = 0
sample1[, 5] = as.numeric(sample1[, 5])

# Specify the two-stage cluster designs.
design <- svyd2(psu.ssu, Sample, psu.col = 2,
                ssu.col = 1)
design.f = subset(design, sex == 'Female')
design.m = subset(design, sex == 'Male')

# Look at the variables contained in the survey design
# and specify the type of estimate for each variable.

names(design$variables)
variables <- c("", "", "total", "prop", "mean",
rep("prop", 3), "total", rep("prop", 3),
"", "")

names(design.f$variables)
variables.f <- c("", "", "total", "prop", "mean",
                 rep("prop", 3), "total", rep("prop", 3),
                 "", "")

names(design.m$variables)
variables.m <- c("", "", "total", "prop", "mean",
                 rep("prop", 3), "total", rep("prop", 3),
                 "", "")

# Make sure you specify the correct type of
# estimate for each variable
cbind(names(design$variables), variables)
cbind(names(design.f$variables), variables.f)
cbind(names(design.m$variables), variables.m)

# Calculate summary statistics.
estimates = svysumm(design, variables, rnd = 3)
estimates.f = svysumm(design.f, variables.f, rnd = 3)
estimates.m = svysumm(design.m, variables.m, rnd = 3)

## Then:
# See which rows contain the estimates of interest.
data.frame(rownames(estimates))
data.frame(rownames(estimates.f))
data.frame(rownames(estimates.m))

# Change the ranges.
par.rg.iasa.1 = setranges(pars.iasa)
new.est = c(b1 = 11, ab = 17, ad = 10)
par.rg.iasa = setranges(pars = par.rg.iasa.1,
                        estimates = estimates,
                        rowpars = new.est)
new.est.f = c(df1 = 15, sf1 = 8)
par.rg.iasa = setranges(pars = par.rg.iasa.1,
                        estimates = estimates.f,
                        rowpars = new.est.f)
new.est.m = c(dm1 = 15, sm1 = 8)
par.rg.iasa = setranges(pars = par.rg.iasa.1,
                        estimates = estimates.m,
                        rowpars = new.est.m)
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
  \code{\link{sensRange}} and \code{\link{sterim}}.
}

