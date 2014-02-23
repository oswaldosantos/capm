\name{Calculate2StageSampleSize}
\alias{Calculate2StageSampleSize}
\title{Two-stage cluster sampling size and composition}
\usage{
Calculate2StageSampleSize(psu.ssu = NULL, psu.x = NULL, level = 0.95,
  error = 0.1, cost = 4)
}
\arguments{
  \item{psu.ssu}{\code{\link{data.frame}} with all primary
  sampling units (PSU). First column contains PSU unique
  identifiers. Second column contains \code{\link{numeric}}
  PSU sizes.}

  \item{psu.x}{\code{\link{data.frame}}. Each row
  corresponds to a secondary sampling unit (SSU) surveyed
  in a pilot study. First column contains the PSU
  identifiers to which the ssu belongs to. Second column
  contains the totals observed in the ssu and must be
  \code{\link{numeric}}.}

  \item{level}{the confidence level required. It must be
  \code{\link{numeric}} between 0 and 1 inclusive.}

  \item{error}{the maximum relative difference between the
  estimate and the unknown population value. It must be
  \code{\link{numeric}} between 0 and 1 inclusive.}

  \item{cost}{the ratio of the cost of sampling a PSU to
  the cost of sampling a SSU.}
}
\value{
Matrix with the sample size and composition and with
variability estimates.
}
\description{
Calculates sample size and composition for a two-stage
cluster sampling design to estimate a total.
}
\details{
It is assumed that psu from the pilot are selected with
probability proportional to size (PPS) and with
replacement. ssu are assumed to be selected via simple
(systematic) random sampling.

PSU must have the same identifiers in \code{psu.ssu} and in
\code{psu.x}.
}
\examples{
# Load data with psu identifiers and sizes.
data(psu.ssu)

# Load data from a pilot sample.
data(pilot)

# Calculate sample size and composition.
(sample.sc <- Calculate2StageSampleSize(psu.ssu, pilot, level = 0.95, error = 0.1, cost = 4))
}
\references{
Levy P and Lemeshow S (2008). Sampling of populations:
methods and applications, Fourth edition. John Wiley and
Sons, Inc.

\url{http://oswaldosantos.github.io/capm}
}

