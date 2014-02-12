\name{survey.data}
\alias{survey.data}
\docType{data}
\title{
Estimates of dog demographic variables
}
\description{
This data set list dog demographic variables obtained from simulations based on field data.
}
\usage{data(survey.data)}
\format{
  A data frame with 16622 observations on the following 13 variables.
  \describe{
    \item{\code{interview_id}}{Interview Id.}
    \item{\code{psu}}{Primary sampling unit Id.}
    \item{\code{dogs}}{Indicate if there is a dog in secondary sampling unit (SSU). For each interviewed ssu with more than one dog, there is as many rows as dogs in it.}
    \item{\code{sex}}{Sex of the dog.}
    \item{\code{age}}{Age of the dog}
    \item{\code{sterilized}}{Indicate if the dog is sterilized (spayed/neutered).}
    \item{\code{sterilized.ly}}{Indicate if the dog was sterilized during the last year.}
    \item{\code{adopted}}{Indicate if the dog was adopted.}
    \item{\code{births}}{Number of puppies per female.}
    \item{\code{present}}{Indicate if the dog was present in the ssu at the moment of the interview.}
    \item{\code{death}}{Indicate if the dog who lived in the ssu during the last year but was not present at the moment of the interview, died.}
    \item{\code{lost}}{Indicate if the dog who lived in the ssu during the last year but was not present at the moment of the interview, disapear or was abandoned.}
  }
}
\details{
Some model parameters used in other capm function are estimates of this variables, obtained with \code{\link{SummarySurvey}} function. Variables refering to events occurred during the year previous to the interview are used as annual rates.

The data were simulated to approximate rates obtained in field studies to be published.
}
\examples{
data(survey.data)
str(survey.data)
head(survey.data)
}
\keyword{datasets}
