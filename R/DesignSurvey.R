#' Survey design
#' @description A wraper for \code{\link{svydesign}} function from the survey package, to define one of the following survey designs: two-stage cluster, simple (systematic) or stratified. In the first case, weights are calculated considering a sample with probability proportional to size and with replacement for the first stage and a simple random sampling for the second stage. Finite population correction is specified as the population size for each level of sampling.
#' @param sample \code{\link{data.frame}} with sample observations. for two-stage cluster designs, one of the columns must contain unique identifiers for PSU and another column must contain unique identifiers for Secondary Sampling Units (SSU).
#' @param psu.ssu \code{\link{data.frame}} with all Primary Sampling Units (PSU). First column contains PSU unique identifiers. Second column contains \code{\link{numeric}} PSU sizes. It is used only for two-stage cluster designs.
#' @param psu.col the column of \code{sample} containing the psu identifiers (for two-stage cluster designs). It is used only for two-stage cluster designs.
#' @param ssu.col the column of \code{sample} containing the ssu identifiers (for two-stage cluster designs). It is used only for two-stage cluster designs.
#' @param cal.col the column of \code{sample} with the variable to calibrate estimates. It must be used together with \code{cal.N}.
#' @param psu.2cd for two-stage cluster designs, value indicating the number of psu included (for PSU included more than once, each must be counted).
#' @param N for simple designs, a \code{\link{numeric}} value representing the total of sampling units in the population. for a stratified design, it is a column of \code{sample} indicating, for each observation, the total of sampling units in its respective strata. \code{N} is ignored in two-stage cluster designs.
#' @param strata for stratified designs, a column of \code{sample} indicating the strata memebership of each observation.
#' @param cal.N population total for the variable to calibrate the estimates. It must be used togheter with \code{cal.col}.
#' @param ... further arguments passed to \code{\link{svydesign}} function. 
#' @return An object of class survey.design.
#' @details For two-stage cluster designs, a PSU appearing in both \code{psu.ssu} and in \code{sample} must have the same identifier. SSU identifiers must be unique but can appear more than once if there is more than one observation per SSU. \code{sample} argument must have just the varibles to be estimated plus the variables required to define the design (two-stage cluster or stratified). \code{cal.col} and \code{cal.N} are needed only if estimates will be calibrated. The calibration is based on a population total.
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
#' DesignSurvey(sample = cluster_sample2,
#'              psu.ssu = city[, c("track_id", "hh")],
#'              psu.col = 1, ssu.col = 2, psu.2cd = 65,
#'              cal.col = 3, cal.N = sum(hh$persons))
#' #'
#' ## Simple design.
#' data(sys_sample)
#' sys_sample2 <- sys_sample[complete.cases(sys_sample), 7:9]
#' DesignSurvey(sample = sys_sample[, -c(1:2)], N = sum(city$hh))
#' 
#' ## Assuming that systematic_sample is a stratified design.
#' # Hypothetical strata
#' strat <- sys_sample2
#' strat$strat <- sample(c("urban", "rural"), nrow(strat), prob = c(.95, .05),
#'                       replace = TRUE)
#' strat$strat_size <- round(sum(city$hh) * .95)
#' strat$strat_size[strat$strat == "rural"] <- round(sum(city$hh) * .05)
#' DesignSurvey(strat, N = "strat_size", strata = "strat")
#' 
DesignSurvey <- function (sample = NULL, psu.ssu = NULL, psu.col = NULL,
                          ssu.col = NULL, cal.col = NULL, psu.2cd = NULL,
                          N = NULL, strata = NULL, cal.N = NULL, ...) 
{
  if (is.numeric(psu.2cd)) {
    if (length(which(!is.na(match(psu.ssu[, 1], sample[, psu.col])))) == 0) {
      stop("There is no matches between PSU identifiers\nfrom psu.ssu and sample. See details section from the help page.")
    }
    if (is.numeric(psu.col)) {
      names(sample)[psu.col] <- "psu.id"
    } else {
      names(sample)[names(sample) == psu.col] <- "psu.id"
    }
    if (is.numeric(ssu.col)) {
      names(sample)[ssu.col] <- "ssu.id"
    } else {
      names(sample)[names(sample) == ssu.col] <- "ssu.id"
    }
    pop.size <- nrow(psu.ssu)
    sample <- cbind(sample, pop.size)
    sample <- merge(sample, psu.ssu, by.x = "psu.id", by.y = 1)
    names(sample)[ncol(sample)] <- "psu.size"
    psu.sample.size <- tapply(sample$psu.size, sample$psu.id, length)
    psu.sample.size <- rep(psu.sample.size, psu.sample.size)
    f.1 <- psu.2cd * sample$psu.size/sum(psu.ssu[, 2])
    f.2 <- psu.sample.size/sample$psu.size
    sample$weights <- 1/(f.1 * f.2)
    dsn <- svydesign(ids = ~psu.id + ssu.id, fpc = ~pop.size + 
                       psu.size, weights = ~weights, data = sample, ...)
    for (i in c('psu.id', 'ssu.id', 'pop.size', 'psu.size', 'weights')) {
      dsn$variables <- dsn$variables[-which(names(dsn$variables) == i)]
    }
    if (!is.null(cal.col) & !is.null(cal.N)) {
      dsn <- calibrate(dsn, formula = ~ sample[, cal.col]-1, population = cal.N)
    }
    return(dsn)
  }
  if (!is.null(N) & is.null(strata)) {
    sample$N <- N
    dsn <- svydesign(ids = ~1, fpc = ~N, data = sample)
    if (!is.null(cal.col) & !is.null(cal.N)) {
      dsn <- calibrate(dsn, formula = ~ sample[, cal.col]-1, population = cal.N)
    }
    dsn$variables <- dsn$variables[-which(names(dsn$variables) == 'N')]
    dsn$simple <- 'yes'
    return(dsn)
  }
  if (!is.null(N) & !is.null(strata)) {
    dsn <- svydesign(ids = ~1, fpc = ~sample[, N],
                     strata = ~sample[, strata], data = sample)
    if (!is.null(cal.col) & !is.null(cal.N)) {
      dsn <- calibrate(dsn, formula = ~ sample[, cal.col]-1, population = cal.N)
    }
    if (is.numeric(N)) {
      N <- names(sample)[N]
    }
    if (is.numeric(strata)) {
      strata <- names(sample)[strata]
    }
    for (i in c(N, strata)) {
      dsn$variables <- dsn$variables[-which(names(dsn$variables) == i)]
    }
    return(dsn)
  }
}