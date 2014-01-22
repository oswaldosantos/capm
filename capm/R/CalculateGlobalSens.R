#' Global sensitivity analysis
#' @description Wraper for \code{\link{sensRange}} function which calculate sensitivities of population size, to parameters used in one of the following functions: \code{\link{SolveIASA}}, \code{\link{SolveSI}} or \code{\link{SolveTC}} .
#' @param model.out an object of class \code{capmModels}.
#' @param ranges output from the \code{\link{SetRanges}} function, applied to the \code{pars} argument used in the function specified previously in \code{model.out}.
#' @param sensv string or \code{\link{character}} \code{\link{vector}} with the name of the the output variables for which the sensitivity needs to be estimated.
#' @param all logical. If \code{\link{FALSE}}, sensitivity ranges are calculated for each parameter. If \code{TRUE}, sensitivity ranges are calculated for the combination of all aparameters.
#' @details When \code{all} is equal to TRUE, \code{dist} argument in \code{\link{sensRange}} is defined as "latin" and when equal to \code{\link{FALSE}}, as "grid". The \code{num} argument in \code{\link{sensRange}} is defined as 100.
#' @return A \code{data.frame} (extended by \code{summary.sensRange} when \code{all == TRUE}) containing the parameter set and the corresponding values of the sensitivity output variables.
#' @references Soetaert K and Petzoldt T (2010). Inverse modelling, sensitivity and monte carlo analysis in R using package FME. Journal of Statistical Software, 33(3), pp. 1-28.
#' 
#' Reichert P and Kfinsch HR (2001). Practical identifiability analysis of large environmental simulation models. Water Resources Research, 37(4), pp.1015-1030.
#' @seealso \code{\link{sensRange}}.
#' @export
#' @examples
#' ###################
#' ## Example 1     ##
#' ## SolveSI model ##
#' ###################
#' 
#' ## Parameters and intial conditions from estimates
#' ## obtained in examples section from the svysumm function.
#' pars.solvesi <- c(b = 0.245, d = 0.101, 
#'                       k = 98050.49, s = .048)
#' init.solvesi <- c(n = 89136.810, q = 0.198)
#' 
#' # Solve for a specific sterilization rate.
#' solvesi.pt <- SolveSI(pars = pars.solvesi, 
#'                       init = init.solvesi, 
#'                       time = 0:30, dd = 'b',
#'                       im = 100, , method = 'rk4')
#' 
#' ## Set ranges 10 % greater and lesser than the
#' ## point estimates.
#' rg.solvesi <- SetRanges(pars = pars.solvesi)
#' 
#' ## Calculate golobal sensitivity of combined parameters.
#' glob.all.solvesi <- CalculateGlobalSens(
#'   model.out = solvesi.pt, 
#'   ranges = rg.solvesi,
#'   sensv = 'n', all = TRUE)
#' 
#' ## Calculate golobal sensitivity of individual parameters.
#' glob.solvesi <- CalculateGlobalSens(
#'   model.out = solvesi.pt,
#'   ranges = rg.solvesi, sensv = 'n')
#' 
#' #####################
#' ## Example 2       ##
#' ## SolveIASA model ##
#' #####################
#' 
#' ## Parameters and intial conditions.
#' pars.solveiasa <- c(
#'    b1 = 21870.897, b2 = 4374.179,
#'    df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
#'    sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
#'    k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
#'    ab = 0.054, ad = 0.1, v = 0.2, vc = 0.1)
#'    
#' init.solveiasa <- c(
#'    f1 = 33425.19, cf1 = 10864.901,
#'    m1 = 38038.96, cm1 = 6807.759,
#'    f2 = 3342.519, cf2 = 108.64901,
#'    m2 = 3803.896, cm2 = 68.07759)
#' 
#' # Solve for point estimates.
#' solveiasa.pt <- SolveIASA(pars = pars.solveiasa, 
#'                           init = init.solveiasa, 
#'                           time = 0:30, method = 'rk4')
#' 
#' ## Set ranges 10 % greater and lesser than the
#' ## point estimates.
#' rg.solveiasa <- SetRanges(pars = pars.solveiasa)
#' 
#' ## Calculate golobal sensitivity of combined parameters.
#' glob.allSolveIASA <- CalculateGlobalSens(
#'   model.out = solveiasa.pt,
#'   ranges = rg.solveiasa, 
#'   sensv = 'n2', all = TRUE)
#' 
#' ## Calculate golobal sensitivity of an individual parameter.
#' glob.SolveIASA <- CalculateGlobalSens(
#'   model.out = solveiasa.pt, 
#'   ranges = rg.solveiasa,
#'   sensv = 'n2')
#' 
CalculateGlobalSens <- function(model.out = NULL, ranges = NULL, sensv = NULL, all = FALSE) {
  if (!setequal(rownames(ranges), names(model.out$pars))) {
    stop('All parameters in ranges must be\ncontained in "pars" argument  of model.out.')
  }
  if (length(intersect(sensv, names(model.out$init))) == 0) {
    stop('All variables in sensv must be\ncontained in "init" argument  of model.out.')
  }
  if (all) {
    sens <- sensRange(func = model.out$model, 
                      parms = model.out$pars, 
                      init = model.out$init,
                      time = model.out$time,
                      parRange = ranges,
                      dist = "latin", 
                      sensvar = sensv, num = 100)
    return(summary(sens))
  } else {
    sens <- NULL
    for (i in 1:length(model.out$pars)) {
      tmp <- sensRange(func = model.out$model, 
                       parms = model.out$pars, 
                       init = model.out$init,
                       time = model.out$time,
                       parRange = ranges[i, ],
                       dist = "grid", 
                       sensvar = sensv, num = 100)
      sens <- rbind(sens, summary(tmp))
    }
    param <- rep(names(model.out$pars), 
                 each = length(tmp[-1]))
    sens <- cbind(sens, param)
    return(sens)
  }
}