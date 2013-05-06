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
}
\references{
  Lumley, T. (2011). Complex surveys: A guide to analysis
  using R (Vol. 565). Wiley.
}

