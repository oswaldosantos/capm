#' Parameter ranges for global sensitivity analysis
#' @description Define the minimum and maximum values for parameters whose global sensitivities are to be assesses with \code{\link{globalsens}} or \code{\link{sensRange}} functions.
#' @param pars the same \code{pars} vector used in one of the following functions: \code{\link{sterowned}} or \code{\link{rasa}}.
#' @param range scale factor to define the minimum and maximum for each parameter. The default is 0.1, which set the minimum and maximum as 10 percent lesser and greater than the \code{pars} values.
#' @param estimates output from the \code{\link{svysumm}} function (see detials).
#' @param rowpars \code{\link{vector}} with values corresponding to the row positions of \code{estimates}, containing estimates for one or more parameters of \code{pars}. The name of the values must be equal to the name of the corresponding parameters in \code{pars} (see detials and examples).
#' @param verbose logical idnicating if alteratinos must be informed.
#' @details \code{estimates} and \code{rowpars} are optional arguments to be used together. Ranges for all parameters are initially defined acording to \code{range}. If \code{estimates} is used, the confidence limits for the parameters specified in \code{rowpars} will replace the corresponding ranges initially dfined by \code{range}.
#' @return a \code{\link{data.frame}} with the complete set of parameter ranges, when \code{verbose} is \code{FALSE} or rowpars is \code{NULL}. If \code{verbose} is \code{TRUE} and rowpars is not \code{NULL}, a \code{\link{list}} with information about the changed ranges (first element) and the complete set of parameter ranges.
#' @note Logistic growth models are not intended for scenarios in which population size is greater than carrying capacity and growth rate is negative.
#' @references Soetaert K and Petzoldt T (2010). Inverse modelling, sensitivity and monte carlo analysis in R using package FME. Journal of Statistical Software, 33(3), pp. 1-28.
#' 
#' Reichert P and Kfinsch HR (2001). Practical identifiability analysis of large environmental simulation models. Water Resources Research, 37(4), pp. 1015-1030.
#' @seealso \code{\link{sensRange}} and \code{\link{sterowned}}.
#' @export
#' @examples
#' 
#' data(psu.ssu)
#' data(Sample)
#' 
#' #####################
#' ## Example 1       ##
#' ## sterowned model ##
#' #####################
#' 
#' ## Parameters and initial conditions from estimates   
#' ## obtained in examples section from svysumm function.
#' pars.od <- c(b = 0.262, d = 0.075, 
#'              k = 82531.831 * 1.1, s = .056)
#' 
#' ## Set ranges 10 % greater and lesser than the 
#' ## point estimates.
#' par.rg.od10 = setranges(pars.od)
#' 
#' ## Set ranges based on estimated confidence limits.
#' 
#' # Specify the two-stage cluster design.
#' design <- svyd2(psu.ssu, Sample, psu.col = 2, 
#'                 ssu.col = 1)
#' 
#' # Look at the variables contained in the survey design
#' names(design$variables)
#' 
#' # Specify the type of estimate for each variable
#' variables <- c("", "", "total", "prop", "mean", 
#'                rep("prop", 8), "", "")
#' 
#' # Make sure you specify the correct type of 
#' # estimate for each variable
#' cbind(names(design$variables), variables)
#' 
#' # Calculate the summary statistics for the survey
#' estimates <- svysumm(design, variables, rnd = 3)
#' 
#' ## Then: 
#' # See which rows contain the estimates of interest.
#' data.frame(rownames(estimates))
#' 
#' # Estimaes of birth, death and sterilization rates are
#' # in rows 13, 17 and 8.
#' par.rg.od = setranges(
#'  pars.od, 
#'  range = .1, 
#'  estimates, 
#'  rowpars = c('b' = 13, 'd' = 17, 's' = 8)
#'  )
#'  
#' #####################
#' ## Example 2       ##
#' ## rasa model      ##
#' #####################
#' 
#' ## Parameters and initial conditions from estimates   
#' ## obtained in examples section from svysumm function.
#' pars.rasa = c(
#'    af1 = 0.262, am1 = 0.262, af2 = 0.288, am2 = 0.288,
#'    bf1 = 0.081, bm1 = 0.069, bf2 = 0.089, bm2 = 0.076,
#'    ef1 = 0.064, ef2 = 0.05, em1 = 0.048, em2 = 0.05,
#'    k1 = 90785.01, k2 = 9078.501, z1 = 1, z2 = 1, 
#'    h = 0.065, j = 0.095, v = 0.111
#' )
#' 
#' ## Set ranges 10 % greater and lesser than the 
#' ## point estimates.
#' par.rg.rasa10 = setranges(pars.rasa)
#' 
#' ## Set ranges based on estimated confidence limits.
#' 
#' # Prepare the data (see svysumm examples).
#' sample1 = Sample[, c(1:4, 6:7, 12)]
#' sample1[, 5] = as.character(sample1[, 5])
#' sample1[which(sample1$castrated == "yes"), 5] = 1
#' sample1[which(sample1[, 5] == "no"), 5] = 0
#' sample1[, 5] = as.numeric(sample1[, 5])
#' 
#' # Specify the two-stage cluster designs.
#' design <- svyd2(psu.ssu, Sample, psu.col = 2, 
#'                 ssu.col = 1)
#' design.f = subset(design, sex == 'Female')
#' design.m = subset(design, sex == 'Male')
#' 
#' # Look at the variables contained in the survey design
#' # and specify the type of estimate for each variable.
#' 
#' names(design$variables)
#' variables <- c("", "", "total", "prop", "mean", 
#'                rep("prop", 8), "", "")
#'                
#' names(design.f$variables)
#' variables.f <- c("", "", "total", "prop", "mean", 
#'                rep("prop", 8), "", "")
#'
#' names(design.m$variables)
#' variables.m <- c("", "", "total", "prop", "mean", 
#'                rep("prop", 8), "", "")
#' 
#' # Make sure you specify the correct type of 
#' # estimate for each variable
#' cbind(names(design$variables), variables)
#' cbind(names(design.f$variables), variables.f)
#' cbind(names(design.m$variables), variables.m)
#' 
#' # Calculate summary statistics.
#' estimates = svysumm(design, variables, rnd = 3)
#' estimates.f = svysumm(design.f, variables.f, rnd = 3)
#' estimates.m = svysumm(design.m, variables.m, rnd = 3)
#' 
#' ## Then: 
#' # See which rows contain the estimates of interest.
#' data.frame(rownames(estimates))
#' data.frame(rownames(estimates.f))
#' data.frame(rownames(estimates.m))
#' 
#' # Change the ranges.
#' par.rg.rasa = setranges(pars.rasa)
#' rep.est = c(af1 = 13, am1 = 13, h = 19, j = 10,v = 12)
#' par.rg.rasa = setranges(pars = pars.rasa, estimates = estimates, 
#'                         rowpars = rep.est)
#' rep.est.f = c(bf1 = 6, ef1 = 4)
#' par.rg.rasa = setranges(pars = pars.rasa, estimates = estimates.f, 
#'                        rowpars = rep.est.f)
#' rep.est.m = c(bm1 = 6, em1 = 4)
#' par.rg.rasa = setranges(pars = pars.rasa, estimates = estimates.m, 
#' rowpars = rep.est.m)
#' 
setranges = function(pars = NULL, range = 0.1, estimates = NULL, rowpars = NULL, verbose = FALSE) {
  par.ranges = data.frame(min = c(pars) * (1 - range),
                          max = c(pars) * (1 + range))
  rownames(par.ranges) = names(pars)
  cumu = NULL
  for (i in 1:length(rowpars)) {
    if (!is.null(rowpars[i])) {
      par.ranges[names(rowpars[i]), ] = estimates[rowpars[i], 3:4]
      cumu = c(cumu, names(rowpars)[i])
    }
  }
  if (verbose == TRUE) {
    if (length(cumu) != 0) {
      return(list('Ranges for the following parameters were changed:' = c(cumu), Ranges = par.ranges))
    } else {
      return(par.ranges)
    }
  } else {
    return(par.ranges)
  }
}