\name{PlotPopPyramid}
\alias{PlotPopPyramid}
\title{Population PlotPopPyramid}
\usage{
  PlotPopPyramid(dat = NULL, col.age = NULL,
    col.sex = NULL, col.cas = NULL, x.label = "Total",
    stage.label = "Years", inner.color = "Gold2",
    outer.color = "DarkOliveGreen")
}
\arguments{
  \item{dat}{\code{\link{data.frame}}.}

  \item{col.age}{the number or name of \code{dat} column
  which have a \code{\link{numeric}} \code{\link{vector}}
  representing ages or stage categories.}

  \item{col.sex}{the number or name of \code{dat} column
  which have a \code{\link{factor}} with two levels
  representing the sex of individuals (see Details).}

  \item{col.cas}{the number of \code{dat} column which have
  a \code{\link{factor}} representing the reproductive
  status of individuals (see Details).}

  \item{x.label}{a string to be used as a label for x-axis.
  If non defined, \code{xlabel} is equal to "Total" (see
  Details).}

  \item{stage.label}{a string to be used as a label for
  ages or stages categories. If non defined,
  \code{stage.label} is equal to "Years" (see Details).}

  \item{inner.color}{any valid way to specify colors. When
  \code{col.cas} is \code{NULL}, \code{inner.color} is the
  color of bars. When \code{col.cas} is not \code{NULL},
  \code{innercolor} is the inner color of bars. If non
  defined, \code{inner.color} is equal to "Gold2".}

  \item{outer.color}{any valid way to specify colors. When
  \code{col.cas} is \code{NULL}, \code{outer.color} is
  ignored. When \code{col.cas} is not \code{NULL},
  \code{outer.color} is the outer color of bars. If non
  defined, \code{outercolor} is equal to "DarkOliveGreen".}
}
\description{
  Dysplays two opposed horizontal barplots (pyramid).
}
\details{
  \code{PlotPopPyramid} is mainly intended for companion
  animals population pyramids, although it can display
  other types of opposed bar charts with suitable
  modification of the arguments.

  The bars to the left of \code{0} on the x axis correspond
  to the minimum value of
  \code{\link{as.numeric}}(\code{dat[, col.sex]}). If
  \code{col.cas} is not \code{NULL}, bars will be stacked,
  with the minimum value of
  \code{\link{as.numeric}}(\code{dat[, col.cas]}) as their
  base.

  On the top of the plot, it is displayed the total number
  of observations of each level of \code{dat[, col.sex]}.
  The \code{\link{levels}} of \code{col.sex} are used as
  \code{\link{labels}}.

  The legend title is equal to \code{names(dat[, col.cas])}
  and the legend \code{\link{labels}} are equal to the
  \code{\link{levels}} of \code{dat[, col.cas]}.

  Font size of saved plots is usually different to the font
  size seen in graphic browsers. Before changing font
  sizes, see the final result in saved (or preview) plots.

  Other details of the plot can be modifyed using
  appropriate functions from \code{ggplot2} package (see
  examples).
}
\note{
  In companion animals population surveys, some age
  categories might be empty. One difference between
  \code{PlotPopPyramid} and \code{pryramid.plot} is that
  the first does not drop empty age categories.
}
\examples{
## Load data with information about age, sex and reproductive status of individuals.
data(Sample)
PlotPopPyramid(Sample, col.age = 5, col.sex = 4, col.cas = 6)
PlotPopPyramid(Sample, col.age = 5, col.sex = 4)
}
\seealso{
  \link[plotrix]{pyramid.plot}.
}

