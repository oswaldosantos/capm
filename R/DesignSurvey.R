#' Two-stage cluster sampling and systematic sampling analysis
#' @description A wraper for \code{\link{svydesign}} function from the survey package, to specify a two-stage cluster sampling analysis or a systematic sampling. In the first case, weights are calculated considering a probability proportional to size sampling with replacement for the first stage and a simple random sampling for the second stage. Finite population correction is specified as the population size for each level of sampling.
#' @param psu.ssu \code{\link{data.frame}} with all Primary Sampling Units (PSU). First column contains PSU unique identifiers. Second column contains \code{\link{numeric}} PSU sizes.
#' @param sample \code{\link{data.frame}} with sample observations. One of the columns must contain unique identifiers for PSU. Another column must contain unique identifiers for Secondary Sampling Units (SSU). The rest of the columns corresponds to the surveyed variables.
#' @param psu.col the column of \code{sample} containing the psu identifiers (for two-stage cluster designs).
#' @param ssu.col the column of \code{sample} containing the ssu identifiers (for two-stage cluster designs).
#' @param design string to define the type of design. "2clusterPPS" defines a two-stage cluster sampling design with selection of PSU with probability proportional to size. "simple" defines a simple (systematic) random sampling design.
#' @param psu.2cd value indicating the number of psu included in a design of type "2clusterPPS" (for psu included more than once, each must be counted).
#' @param total \code{\link{numeric}} value representing the total of sampling units in the population. If \code{design} is equal to "2clusterPPS", it is not necessary to define \code{total}.
#' @param ... further arguments passed to \code{\link{svydesign}} function. 
#' @details A PSU appearing in both \code{psu.ssu} and in \code{sample} must have the same identifier. SSU identifiers must be unique but can appear more than once if there is more than one observation per SSU.
#' @references Lumley, T. (2011). Complex surveys: A guide to analysis using R (Vol. 565). Wiley.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples 
#' # Load data with PSU identifiers and sizes.
#' data(psu.ssu)
#' 
#' # Load data with sample data.
#' data(survey.data)
#' 
#' # Specify the two-stage cluster design that included 20 PSU.
#' (design <- DesignSurvey(psu.ssu, survey.data, psu.col = 2, ssu.col = 1, psu.2cd = 20))

DesignSurvey <- function(psu.ssu = NULL, sample = NULL, psu.col = NULL, ssu.col = NULL, design = '2clusterPPS', psu.2cd = NULL, total = NULL, ...) {
  if (design == '2clusterPPS') {
    if (length(which(!is.na(match(psu.ssu[, 1], 
                                  sample[, psu.col])))) == 0) {
      stop('There is no matches between PSU identifiers\nfrom psu.ssu and sample. See details section from the help page.')
    }
    names(sample)[c(psu.col, ssu.col)] <- c('psu.id', 'ssu.id')
    pop.size <- nrow(psu.ssu)
    sample <- cbind(sample, pop.size)
    sample <- merge(sample, psu.ssu, by.x = psu.col, by.y = 1)
    names(sample)[ncol(sample)] <- 'psu.size'
    psu.sample.size <- tapply(sample$psu.size, sample$psu.id, length)
    psu.sample.size <- rep(psu.sample.size, psu.sample.size)
    f.1 <- psu.2cd * sample$psu.size / sum(psu.ssu[, 2])
    f.2 <- psu.sample.size / sample$psu.size
    sample$weights <- 1 / (f.1 * f.2)
    return(svydesign(ids = ~ psu.id + ssu.id, fpc = ~ pop.size +
                       psu.size, weights = ~ weights, data = sample, ...))
  } else {
    if (design == 'simple') {
      sample$total <- total
      return(svydesign(ids = ~ 1, fpc = ~ total, data = sample))
    }
  }
}
