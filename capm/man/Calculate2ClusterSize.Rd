\name{Calculate2ClusterSize}
\alias{Calculate2ClusterSize}
\title{Two-stage cluster sampling size and composition}
\usage{
  Calculate2ClusterSize(psu.ssu = NULL, psu.x = NULL,
    level = 0.95, error = 0.1, cost = 12)
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
  numeric \code{\link{numeric}}.}

  \item{level}{the confidence level required.}

  \item{error}{the maximum relative difference between the
  estimate and the unknown population value.}

  \item{cost}{the ratio of the cost of sampling a psu to
  the cost of sampling a SSU.}
}
\value{
  Matrix with the sample size and composition and with
  estimates of variability.
}
\description{
  Calculates sample size and composition for a two-stage
  cluster sampling design to estimate a total.
}
\details{
  It is assumed that psu from the pilot are selected with
  probability proportional to size (PPS) and with
  replacement. ssu are assumed to be selected via simple
  random sampling.

  PSU must have the same identifiers in \code{psu.ssu} and
  in \code{psu.x}.
}
\examples{
# Load data with psu identifiers and sizes.
data(psu.ssu)

# Load data from a pilot sample.
data(pilot)

# Calculate sample size and composition.
sample.sc <- Calculate2ClusterSize(psu.ssu, pilot, level = 0.95, error = 0.1, cost = 12)
}
\references{
  Levy P and Lemeshow S (1999). Sampling of populations:
  methods and applications, Second edition. John Wiley and
  Sons, Inc.
}

