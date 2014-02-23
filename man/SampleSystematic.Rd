\name{SampleSystematic}
\alias{SampleSystematic}
\title{Systematic sampling}
\usage{
SampleSystematic(psu.ssu = NULL, ssu = NULL, total = NULL,
  write = FALSE, ...)
}
\arguments{
  \item{psu.ssu}{\code{\link{data.frame}} with all PSU.
  First column contains PSU unique identifiers. Second
  column contains \code{\link{numeric}} PSU sizes.}

  \item{ssu}{\code{\link{numeric}} indicating SSU to be
  selected.}

  \item{total}{\code{\link{numeric}} indicating the number
  of sampling units in the population. This is an
  alternative to \code{psu.ssu}, intended for simple
  sampling designs.}

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
Select sampling units using systematic samplin. In the
context of two-stage cluster sampling, select Secondary
Sampling Units (SSU) in one or more Primary Sampling Units
(PSU), using systematic sampling.
}
\examples{
# Load data with PSU identifiers and sizes.
data(psu.ssu)

# Take a sample of 10 PSU, with probability
# proportional to size and with replacement.
selected.psu <- SamplePPS(psu.ssu, 10, write = FALSE)

# Take a sistematic sampling of 5 SSU within each
# PSU of selected.psu.
(selected.ssu <- SampleSystematic(selected.psu, 5, write = FALSE))

# Simple systematic sampling
(simp.syst.sampling <- SampleSystematic(ssu = 5, total = 100))
}
\references{
Levy P and Lemeshow S (2008). Sampling of populations:
methods and applications, Fourth edition. John Wiley and
Sons, Inc.

\url{http://oswaldosantos.github.io/capm}
}
\seealso{
\code{\link{SamplePPS}}.
}

