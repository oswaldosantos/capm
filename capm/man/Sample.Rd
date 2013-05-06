\name{Sample}
\alias{Sample}
\docType{data}
\title{
Estimates of dog demographic variables
}
\description{
This data set list dog demographic variables obtained from simulations based on field data.
}
\usage{data(Sample)}
\format{
  A data frame with 1816 observations on the following 13 variables.
  \describe{
    \item{\code{interview_id}}{Interview Id.}
    \item{\code{psu}}{Primary sampling unit Id.}
    \item{\code{caes}}{Number of dogs per secondary sampling unit (ssu).}
    \item{\code{sex}}{Sex of the dog.}
    \item{\code{age}}{Age of the dog}
    \item{\code{castrated}}{Indicate if the dog is castrated}
    \item{\code{castrated1}}{Indicate if the dog was castrated during the last year.}
    \item{\code{adopted}}{Indicate if the dog was adopted.}
    \item{\code{recruitment}}{Indicate if during the year previous to the dog aquiscition, the owner had (and lost) another dog.}
    \item{\code{births}}{Indicate if the dog was was present in the ssu at the moment of the interview.}
    \item{\code{death}}{Indicate if the dog who lived in the ssu during the last year but was not present at the moment of the interview, died.}
    \item{\code{lost}}{Indicate if the dog who lived in the ssu during the last year but was not present at the moment of the interview, disapear or was abandoned.}
  }
}
\details{
Some model parameters used in other capm function are estimates of this variables, obtained with \code{\link{svysumm}} function. Variables refering to events occurred during the year previous to the interview are used as annual rates.

The data were simulated to approximate rates obtained in field studies to be published.
}
\examples{
data(Sample)
str(Sample)
head(Sample)
}
\keyword{datasets}
