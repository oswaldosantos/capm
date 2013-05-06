#' Summary statistics for sample surveys
#' @description Wraps functions for summary statistics from survey package.
#' @param design an object of class "survey.design2" "survey.design".
#' @param variables \code{\link{character}} \code{\link{vector}} with the type of estimate for each variable contained in \code{design} (see details).
#' @param level the confidence level required.
#' @param rnd the number of decimal places (round) or significant digits (signif) to be used. If \code{NA}, scientific notation is used.
#' @return Matrix with survey summaries.
#' @details The length of \code{variables} must be equal to the length of \code{names(design$variables)} (see examples).
#' @references Lumley, T. (2011). Complex surveys: A guide to analysis using R (Vol. 565). Wiley.
#' @export
#' @examples
#' # Load data with psu identifiers and sizes.
#' data(psu.ssu)
#' 
#' # Load data with sample data.
#' data(Sample)
#' 
#' # Specify the two-stage cluster design.
#' design <- svyd2(psu.ssu, Sample, psu.col = 2, ssu.col = 1)
#' 
#' # Look at the variables contained in the survey design
#' names(design$variables)
#' 
#' # Specify the type of estimate for each variable
#' variables <- c("", "", "total", "prop", "mean", rep("prop", 8), "", "")
#' 
#' # Make sure you specify the correct type of estimate for each variable
#' cbind(names(design$variables), variables)
#' 
#' # Calculate the summary statistics for the survey
#' estimates <- svysumm(design, variables = variables, rnd = 3)
svysumm <- function(design = NULL, variables = NULL, level = 0.95, rnd = 3) {
  if (length(variables) != length(names(design$variables))) {
    stop('The length of variables argument must be equal to the length of names(design$variables)')
  }
  match1 <- names(design$variables)
  match2 <- c('psu.id', 'ssu.id', 'pop.size', 'psu.size')
  matches <- which(!is.na(match(match1, match2)))
  variables[matches] <- ''
  z <- abs(round(qnorm((1 - level) / 2, 0, 1), 2))
  vrs <- design$variables
  out <- NULL
  for (i in 1:length(variables)) {
    if (variables[i] == 'total') {
      tmp <- svytotal(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Total.', names(vrs)[i])
      out <- rbind(out, tmp1)
    }
    if (variables[i] == 'mean') {
      tmp <- svymean(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Mean.', names(vrs)[i])
      out <- rbind(out, tmp1)
    }
    if (variables[i] == 'prop') {
      tmp <- svymean(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Prop.', rownames(tmp1))
      rownames(tmp1) <- gsub('vrs\\[, i\\]', paste0(names(vrs)[i], '.'), rownames(tmp1))
      out <- rbind(out, tmp1)
    }
  }
  colnames(out) <- c('Estimate', 'SE', ci[1], ci[2], 'Deff', 'Error (%)')
  ifelse (is.na(rnd), return(out), return(round(out, rnd)))
}