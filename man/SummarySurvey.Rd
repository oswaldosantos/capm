\name{SummarySurvey}
\alias{SummarySurvey}
\title{Summary statistics for sample surveys}
\usage{
SummarySurvey(design = NULL, variables = NULL, level = 0.95, rnd = 3)
}
\arguments{
  \item{design}{an output form \code{\link{DesignSurvey}}
  function.}

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
Wraps functions for summary statistics from survey package.
}
\details{
The length of \code{variables} must be equal to the length
of \code{names(design$variables)} (see examples).
}
\examples{
# Load data.
data(psu.ssu)
data(survey.data)

#######################
## Example 1         ##
## General estimates ##
#######################

# Specify the two-stage cluster design.
design <- DesignSurvey(psu.ssu, survey.data, psu.col = 2, ssu.col = 1, psu.2cd = 20)

# Look at the variables contained in the survey design
names(design$variables)

# Specify the type of estimate for each variable
variables <- c("", "", "total", "prop", "mean", rep("prop", 3),
              "total", rep("prop", 3), "", "", "")

# Make sure you specify the correct type of estimate for each variable
cbind(names(design$variables), variables)

# Calculate the summary statistics for the survey.
# Uncomment the following two lines (will take some seconds).
# estimates <- SummarySurvey(design, variables = variables,
#                           rnd = 3)

############################
## Example 2              ##
## Sex-specific estimates ##
############################

# Make a copy of the dataset and select some
# variables of interest.
sample1 <- survey.data[, c(1:4, 6:7, 11)]

# Transform to numeric the "sterilized" variable in order
# to estimate its total.
sample1[, 5] <- as.character(sample1[, 5])
sample1[which(sample1$sterilized == "yes"), 5] <- 1
sample1[which(sample1[, 5] == "no"), 5] <- 0
sample1[, 5] <- as.numeric(sample1[, 5])

# Define a survey design for each sex.
design.sex <- DesignSurvey(psu.ssu, sample1,
                   psu.col = 2,
                   ssu.col = 1, psu.2cd = 20)
design.f <- subset(design.sex, sex == 'Female')
design.m <- subset(design.sex, sex == 'Male')

# Look at the variables contained in the survey design
names(design.sex$variables)

# Specify the type of estimate for each variable
variables.sex <- c("", "", "total", "", "total",
                  "prop", "prop", "", "", "")

# Make sure you specify the correct type of
# estimate for each variable
cbind(names(design.sex$variables), variables.sex)

# Calculate the summary statistics for the surveys.
# Uncomment the following two lines (will take some seconds).
# estimates.f <- SummarySurvey(design.f, variables.sex, rnd = 3)
# estimates.m <- SummarySurvey(design.m, variables.sex, rnd = 3)
}
\references{
Lumley, T. (2011). Complex surveys: A guide to analysis
using R (Vol. 565). Wiley.

\url{http://oswaldosantos.github.io/capm}
}

