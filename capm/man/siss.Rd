\name{siss}
\alias{siss}
\title{Systematic sampling}
\usage{
  siss(psu.ssu = NULL, ssu = NULL, write = FALSE, ...)
}
\arguments{
  \item{psu.ssu}{\code{\link{data.frame}} with all primary
  sampling units (psu). First column contains psu unique
  identifiers. Second column contains \code{\link{numeric}}
  psu sizes.}

  \item{ssu}{the number of ssu to be selected.}

  \item{write}{logical. If \code{TRUE}, a *.csv file
  containing the psu and their ssu is writed in the current
  working directory.}

  \item{...}{further arguments passed to
  \code{\link{write.table}} function.}
}
\value{
  A \code{matrix}. The names of columns are the identifiers
  of selected psu, coerced by \code{\link{as.character}} to
  avoid scientific notation in case the identifiers be of
  \code{\link{class}} \code{\link{numeric}}. The rows
  correspond to the selected ssu within each psu.
}
\description{
  Select secondary sampling units (ssu) in one or more
  primary sampling units (psu), using systematic sampling.
}
\examples{
# Load data with psu identifiers and sizes.
data(psu.ssu)

# Take a sistematic sampling of 5 ssu in the first five psu of psu.ssu.
selected.ssu <- siss(psu.ssu[1:5, ], 5, write = TRUE)
selected.ssu
}
\references{
  Levy P and Lemeshow S (1999). Sampling of populations:
  methods and applications, Second edition. John Wiley and
  Sons, Inc.
}
\seealso{
  \code{\link{ppssr}}.
}

