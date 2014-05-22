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
  A data frame with 469 observations on the following 16 variables.
  \describe{
    \item{\code{interview_id}}{Interview Id.}
    \item{\code{psu}}{Primary sampling unit Id.}
    \item{\code{dogs}}{There is a dog in the secondary sampling unit (SSU). For each interviewed SSU with more than one dog, there is as many rows as dogs in it.}
    \item{\code{sex}}{Sex of the dog.}
    \item{\code{age}}{Age of the dog}
    \item{\code{sterilized}}{The dog is sterilized (spayed/neutered).}
    \item{\code{sterilized.ly}}{The dog was sterilized during the last year.}
    \item{\code{births}}{Number of puppies during the last year.}
    \item{\code{present}}{The dog was present in the SSU at the moment of the interview.}
    \item{\code{fate}}{Fate of dogs: died, given, in_home (remains in home), lost or sold.}
    \item{\code{acquired}}{Mode in which the dog was acquired: adopted, born_in_home, bought, gift (given by someone).}
    \item{\code{outside}}{The dog was acquired in another city.}
    \item{\code{acquired.ly}}{The dog was acquired during the last year}
    \item{\code{immigrant}}{"acquired"" equals to "bought" or "outside"" equales to "yes". Immigration is understood as the movements from outside populations (populations from other cities and populations which maintain the pet market of the city.). Movements from stray populations are parameterized under category "adopted" of the variable "acquired".}
    \item{\code{immigrant.ly}}{The dog immigrated during the last year.}
    \item{\code{immigrant.sterilized.ly}}{The dog who immigrated during the last year was already sterilized.}
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
