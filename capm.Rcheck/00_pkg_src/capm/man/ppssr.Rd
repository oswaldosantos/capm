\name{ppssr}
\alias{ppssr}
\title{Probability proportional to size sampling with replacement}
\usage{
  ppssr(psu.ssu = NULL, psu = NULL, write = FALSE, ...)
}
\arguments{
  \item{psu.ssu}{\code{\link{data.frame}} with all primary
  sampling units (psu). First column contains psu unique
  identifiers. Second column contains \code{\link{numeric}}
  psu sizes.}

  \item{psu}{the number of psu to be selected.}

  \item{write}{logical. If \code{TRUE}, a *.csv file
  containing the psu and their ssu is writed in the current
  working directory.}

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
  \code{\link{siss}}.
}
\description{
  Select primary sampling units (psu) with probability
  proportional to size with replacement.
}
\examples{
# Load data with psu identifiers and sizes.
data(psu.ssu)

# Take a sample of 20 psu with probability proportional to size with replacement.
selected.psu = ppssr(psu.ssu, 20, write = FALSE)
selected.psu
}
\references{
  Levy P and Lemeshow S (1999). Sampling of populations:
  methods and applications, Second edition. John Wiley and
  Sons, Inc.
}
\seealso{
  \link[pps]{pps1}, \link[pps]{ppss}, \code{\link{siss}}.
}

