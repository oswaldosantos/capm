\name{svyd2}
\alias{svyd2}
\title{Two-stage cluster sampling analysis}
\usage{
  svyd2(psu.ssu = NULL, sample = NULL, psu.col = NULL,
    ssu.col = NULL, ...)
}
\arguments{
  \item{psu.ssu}{\code{\link{data.frame}} with all primary
  sampling units (psu). First column contains psu unique
  identifiers. Second column contains \code{\link{numeric}}
  psu sizes.}

  \item{sample}{a \code{\link{data.frame}} with sample
  observations. One of the columns must contain unique
  identifiers for psu. Another column must contain unique
  identifiers for ssu. The rest of the columns corresponds
  to the variables surveyed.}

  \item{psu.col}{the column of \code{sample} containing the
  psu identifiers.}

  \item{ssu.col}{the column of \code{sample} containing the
  ssu identifiers.}

  \item{...}{further arguments passed to
  \code{\link{svydesign}} function.}
}
\description{
  A wraper for \code{\link{svydesign}} function from the
  survey package, to specify a two-stage cluster sampling
  analysis. Weights are calculated considering a
  probability proportional to size sampling with
  replacement for the first stage and a simple random
  sampling for the second stage. Finite population
  correction is specified as the population size for each
  level of sampling.
}
\details{
  A psu appearing in both \code{psu.ssu} and in
  \code{sample} must have the same identifier. ssu
  identifiers must be unique but can appear more than once
  if there is more than one observations per ssu.
}
\examples{
# Load data with psu identifiers and sizes.
data(psu.ssu)

# Load data with sample data.
data(Sample)

# Specify the two-stage cluster design.
design <- svyd2(psu.ssu, Sample, psu.col = 2, ssu.col = 1)
}
\references{
  Lumley, T. (2011). Complex surveys: A guide to analysis
  using R (Vol. 565). Wiley.
}

