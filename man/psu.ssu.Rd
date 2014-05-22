\name{psu.ssu}
\alias{psu.ssu}
\docType{data}
\title{
Primary and secondary sampling frames from Santos, Brazil.
}
\description{
This data set list the primary (enumeration units/setores censitarios) and secondary (household/domicilios) sampling units from urban area of Santos city, according to the census of 2010, made by the Instituto Brasileiro de Geografia e Estatistica - IBGE.
}
\usage{data(psu.ssu)}
\format{
  A data frame with 649 observations on the following 2 variables.
  \describe{
    \item{\code{psu}}{Primary sampling units}
    \item{\code{ssu}}{Total number of secondary sampling units per primary sampling unit.}
  }
}
\source{
\url{www.ibge.gov.br}
}
\examples{
data(psu.ssu)
str(psu.ssu)
head(psu.ssu)
}
\keyword{datasets}
