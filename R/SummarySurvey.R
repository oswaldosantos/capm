#' Summary statistics for sample surveys
#' @description Wraps functions for summary statistics from survey package.
#' @param design an output form \code{\link{DesignSurvey}} function.
#' @param variables \code{\link{character}} \code{\link{vector}} with the type of estimate for each variable contained in \code{design} (see details).
#' @param conf.level the confidence level required.
#' @param rnd the number of decimal places (round) or significant digits (signif) to be used. If \code{NA}, scientific notation is used.
#' @return Matrix with survey summaries.
#' @details The length of \code{variables} must be equal to the length of \code{names(design$variables)} (see examples).
#' @references Lumley, T. (2011). Complex surveys: A guide to analysis using R (Vol. 565). Wiley.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples
#' data(city)
#' data(hh)
#' ## Two-stage cluster design that included 65 PSU.
#' data(cluster_sample)
#' cluster_sample2 <- cluster_sample[complete.cases(cluster_sample), c(1:2, 8:10)]
#' design <- DesignSurvey(sample = cluster_sample2,
#'              psu.ssu = city[, c("track_id", "hh")],
#'              psu.col = "track_id", ssu.col = "hh_id", psu.2cd = 65,
#'              cal.col = "persons", cal.N = sum(hh$persons))
#' vars <- rep("total", 3)
#' cbind(names(design$variables), vars)
#' SummarySurvey(design = design, variables = vars)
#' 
#' ## Systematic sampling
#' data(sys_sample)
#' sys_sample2 <- sys_sample[complete.cases(sys_sample), 7:9]
#' design <- DesignSurvey(sample = sys_sample2, N = sum(city$hh),
#'                        cal.col = "persons", cal.N = sum(hh$persons))
#' vars <- rep("total", 3)
#' cbind(names(design$variables), vars)
#' #SummarySurvey(design = design, variables = vars)
#'
SummarySurvey <- function(design = NULL, variables = NULL, conf.level = 0.95, rnd = 3) {
  if (length(variables) != length(names(design$variables))) {
    stop('The length of variables argument must be equal to the length of names(design$variables)')
  }
  match1 <- names(design$variables)
  match2 <- c('psu.id', 'ssu.id', 'pop.size', 'psu.size')
  matches <- which(!is.na(match(match1, match2)))
  variables[matches] <- ''
  z <- abs(round(qnorm((1 - conf.level) / 2, 0, 1), 2))
  vrs <- design$variables
  out <- NULL
  for (i in 1:length(variables)) {
    if (variables[i] == 'total') {
      tmp <- svytotal(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = conf.level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Total.', names(vrs)[i])
      out <- rbind(out, tmp1)
    }
    if (variables[i] == 'mean') {
      tmp <- svymean(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = conf.level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Mean.', names(vrs)[i])
      out <- rbind(out, tmp1)
    }
    if (variables[i] == 'prop') {
      tmp <- svymean(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = conf.level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Prop.', rownames(tmp1))
      rownames(tmp1) <- gsub('vrs\\[, i\\]', paste0(names(vrs)[i], '.'), rownames(tmp1))
      out <- rbind(out, tmp1)
    }
  }
  colnames(out) <- c('Estimate', 'SE', ci[1], ci[2], 'Deff', 'Error (%)')
  if ('simple' %in% names(design)) {
    out <- out[ , -5]
  }
  ifelse (is.na(rnd), return(out), return(round(out, rnd)))
}
