\name{SamplePPS}
\alias{SamplePPS}
\title{Probability proportional to size sampling with replacement}
\usage{
  SamplePPS(psu.ssu = NULL, psu = NULL, write = FALSE, ...)
}
\arguments{
  \item{psu.ssu}{\code{\link{data.frame}} with all primary
  sampling units (PSU). First column contains PSU unique
  identifiers. Second column contains \code{\link{numeric}}
  PSU sizes.}

  \item{psu}{the number of PSU to be selected.}

  \item{write}{logical. If \code{TRUE}, a *.csv file
  containing the PSU and their secondary sampling units
  (SSU) is writed in the current working directory.}

  \item{...}{further arguments passed to
  \code{\link{write.table}} function.}
}
\value{
  \code{\link{data.frame}}. First column contains the
  selected psu identifiers, coerced by
  \code{\link{as.character}}, to avoid scientific notation
  in case the identifiers be of \code{\link{class}}
  \code{\link{numeric}}. Second column contain psu sizes, a
  variable needed for second stage sampling with
  \code{\link{SampleSystematic}}.
}
\description{
  Select primary sampling units (PSU) with probability
  proportional to size with replacement.
}
\examples{
# Load data with PSU identifiers and sizes.
data(psu.ssu)

# Take a sample of 20 PSU with probability proportional to size with replacement.
(selected.psu <- SamplePPS(psu.ssu, 20, write = FALSE))
}
\references{
  Levy P and Lemeshow S (1999). Sampling of populations:
  methods and applications, Second edition. John Wiley and
  Sons, Inc.
}
\seealso{
  \code{\link{SampleSystematic}}.
}

