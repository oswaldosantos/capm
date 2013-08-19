#' Two-stage cluster sampling analysis
#' @description A wraper for \code{\link{svydesign}} function from the survey package, to specify a two-stage cluster sampling analysis. Weights are calculated considering a probability proportional to size sampling with replacement for the first stage and a simple random sampling for the second stage. Finite population correction is specified as the population size for each level of sampling.
#' @param psu.ssu \code{\link{data.frame}} with all primary sampling units (psu). First column contains psu unique identifiers. Second column contains \code{\link{numeric}} psu sizes.
#' @param sample a \code{\link{data.frame}} with sample observations. One of the columns must contain unique identifiers for psu. Another column must contain unique identifiers for ssu. The rest of the columns corresponds to the variables surveyed.
#' @param psu.col the column of \code{sample} containing the psu identifiers.
#' @param ssu.col the column of \code{sample} containing the ssu identifiers.
#' @param design string to define the type of design. "2clusterPPS" defines a two-stage cluster sampling design with selection of PSU with probability proportional to size. "simple" defines a simple (systematic) random sampling design.
#' @param total value corresponding to population size. If \code{design} is equal to "2clusterPPS", it is not necessary to define \code{total}.
#' @param ... further arguments passed to \code{\link{svydesign}} function. 
#' @details A psu appearing in both \code{psu.ssu} and in \code{sample} must have the same identifier. ssu identifiers must be unique but can appear more than once if there is more than one observations per ssu.
#' @references Lumley, T. (2011). Complex surveys: A guide to analysis using R (Vol. 565). Wiley.
#' @export
#' @examples 
#' # Load data with PSU identifiers and sizes.
#' data(psu.ssu)
#' 
#' # Load data with sample data.
#' data(Sample)
#' 
#' # Specify the two-stage cluster design.
#' design = DesignSurvey(psu.ssu, Sample, psu.col = 2, ssu.col = 1)

DesignSurvey <- function(psu.ssu = NULL, sample = NULL, psu.col = NULL, ssu.col = NULL, design = '2clusterPPS', total = NULL, ...) {
  if (design == '2clusterPPS') {
    if (length(which(!is.na(match(psu.ssu[, 1], 
                                  sample[, psu.col])))) == 0) {
      stop('There is no matches between PSU identifiers\nfrom psu.ssu and sample. See details section from the help page.')
    }
    names(sample)[c(psu.col, ssu.col)] <- c('psu.id', 'ssu.id')
    pop.size <- sum(psu.ssu[, 2])
    sample <- cbind(sample, pop.size)
    sample <- merge(sample, psu.ssu, by.x = psu.col, by.y = 1)
    names(sample)[ncol(sample)] <- 'psu.size'
    return(svydesign(ids = ~ psu.id + ssu.id, fpc = ~ pop.size +
                       psu.size, data = sample, ...))
  } else {
    if (design == 'simple') {
      sample$total <- total
      return(svydesign(ids = ~ 1, fpc = ~ total, data = sample))
    }
  }
}
