\name{pilot}
\alias{pilot}
\docType{data}
\title{
  Pilot study to calculate sample size and composition
}
\description{
  This data set has 5 secondary sampling units (SSU) selected with the \code{\link{SampleSystematic}} function, within 10 primary sampling units (PSU) from a sampling frame of Santos city, selected with the \code{\link{SamplePPS}} function. For each ssu, there is a number of dogs, simulated from a poisson ditribution, with lambda equal to the weighted mean (0.8) of number od fogs per ssu observed in field studies. The pilot is intended to calculate sample size and composition to estimate owned dog population size.
}
\usage{data(pilot)}
\format{
  A data frame with 50 observations on the following 2 variables.
  \describe{
    \item{\code{psu}}{PSU to which ssu belongs to.}
    \item{\code{dogs}}{Number of dogs per SSU.}
  }
}
\examples{
  data(pilot)
  str(pilot)
  head(pilot)
}
\keyword{datasets}