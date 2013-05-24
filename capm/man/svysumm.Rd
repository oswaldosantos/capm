\name{svysumm}
\alias{svysumm}
\title{Summary statistics for sample surveys}
\usage{
  svysumm(design = NULL, variables = NULL, level = 0.95,
    rnd = 3)
}
\arguments{
  \item{design}{an object of class "survey.design2"
  "survey.design".}

  \item{variables}{\code{\link{character}}
  \code{\link{vector}} with the type of estimate for each
  variable contained in \code{design} (see details).}

  \item{level}{the confidence level required.}

  \item{rnd}{the number of decimal places (round) or
  significant digits (signif) to be used. If \code{NA},
  scientific notation is used.}
}
\value{
  Matrix with survey summaries.
}
\description{
  Wraps functions for summary statistics from survey
  package.
}
\details{
  The length of \code{variables} must be equal to the
  length of \code{names(design$variables)} (see examples).
}
\examples{
# Load data.
data(psu.ssu)
data(Sample)

#######################
## Example 1         ##
## General estimates ##
#######################

# Specify the two-stage cluster design.
design = svyd2(psu.ssu, Sample, psu.col = 2, ssu.col = 1)

# Look at the variables contained in the survey design
names(design$variables)

# Specify the type of estimate for each variable
variables = c("", "", "total", "prop", "mean", rep("prop", 4), "total", rep("prop", 3), "", "")

# Make sure you specify the correct type of estimate for each variable
cbind(names(design$variables), variables)

# Calculate the summary statistics for the survey.
estimates = svysumm(design, variables = variables, rnd = 3)

############################
## Example 2              ##
## Sex-specific estimates ##
############################

# Make a copy of the dataset and select some
# variables of interest.
sample1 = Sample[, c(1:4, 6:7, 12)]

# Transform to numeric the "castrated" variable in order
# to estimate its total.
sample1[, 5] = as.character(sample1[, 5])
sample1[which(sample1$castrated == "yes"), 5] = 1
sample1[which(sample1[, 5] == "no"), 5] = 0
sample1[, 5] = as.numeric(sample1[, 5])

# Define a survey design for each sex.
design.sex = svyd2(psu.ssu, sample1,
                   psu.col = 2,
                   ssu.col = 1)
design.f = subset(design.sex, sex == 'Female')
design.m = subset(design.sex, sex == 'Male')

# Look at the variables contained in the survey design
names(design.sex$variables)

# Specify the type of estimate for each variable
variables.sex = c("", "", "total", "", "total",
                  "prop", "prop", "", "")

# Make sure you specify the correct type of
# estimate for each variable
cbind(names(design.sex$variables), variables.sex)

# Calculate the summary statistics for the surveys.
estimates.f = svysumm(design.f, variables.sex, rnd = 3)
estimates.m = svysumm(design.m, variables.sex, rnd = 3)
}
\references{
  Lumley, T. (2011). Complex surveys: A guide to analysis
  using R (Vol. 565). Wiley.
}

