#' Local sensitivity analysis
#' @description Wraper for \code{\link{sensFun}} function which estimates local effect of all model parameters on population size, applying the so-called sensitivity functions. The set of parameters used in any of the following functions can be assessed: \code{\link{sterowned}} or \code{\link{rasa}}.
#' @param model.out output of one of the following functions: \code{\link{sterowned}} or \code{\link{rasa}}.
#' @param sensv string or \code{\link{character}} \code{\link{vector}} with the name of the the output variables for which the sensitivity needs to be estimated.
#' @details For further arguments of \code{\link{sensFun}}, defaults are used. See the help page of this function for details. Methods for class "sensFun" can be used.
#' @return a \code{\link{data.frame}} of class \code{\link{sensFun}} containing the sensitivity functions. There is one row for each sensitivity variable at each independent time. The first column \code{x}, contains the time value; the second column \code{var}, the name of the observed variable; and remaining columns have the sensitivity parameters.

#' @references Soetaert K and Petzoldt T (2010). Inverse modelling, sensitivity and monte carlo analysis in R using package FME. Journal of Statistical Software, 33(3), pp. 1-28.
#' 
#' Reichert P and Kfinsch HR (2001). Practical identifiability analysis of large environmental simulation models. Water Resources Research, 37(4), pp.1015-1030.
#' @seealso \code{\link{sensRange}}.
#' @export
#' @examples 
#' #####################
#' ## Example 1       ##
#' ## sterowned model ##
#' #####################
#' 
#' ## Parameters and intial conditions from estimates 
#' ## obtained in examples section from the svysumm function.
#' pars.od <- c(b = 0.262, d = 0.075, 
#'              k = 82531.831 * 1.1, s = .056)
#' state.od <- c(n = 82531.831, q = 0.207)
#' 
#' # Solve for a specific sterilization rate.
#' ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)
#' 
#' ## Calculate local sensitivities for all parameters.
#' local.od = localsens(model.out = ster.od, sensv = 'n')
#' 
#' ## Bivariate sensitivity
#' pairs(local.od, col = "green")
#' 
#' ################
#' ## Example 2  ##
#' ## rasa model ##
#' ################
#' 
#' ## Parameters and intial conditions.
#' pars.rasa = c(
#'    bf1 = 0.262, bm1 = 0.262, bf2 = 0.288, bm2 = 0.288,
#'    df1 = 0.081, dm1 = 0.069, df2 = 0.089, dm2 = 0.076,
#'    sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
#'    k1 = 90785.01, k2 = 9078.501, h1 = 1, h2 = 1, 
#'    ab = 0.065, ad = 0.095, r = 0.111
#' )
#' state.rasa = c(
#'    f1 = 41641.785, cf1 = 8423.503, 
#'    m1 = 40890.046, cm1 = 8647.503, 
#'    f2 = 4164.179, cf2 = 208.209, 
#'    m2 = 4089.005, cm2 = 204.45
#' )
#' 
#' # Solve for point estimates.
#' rasa.pt <- rasa(pars = pars.rasa,
#'                 state = state.rasa,
#'                 time = 0:30)
#' 
#' ## Calculate local sensitivities for all parameters.
#' local.rasa2 = localsens(model.out = rasa.pt, sensv = 'n2')
#' local.rasa1 = localsens(model.out = rasa.pt, sensv = 'n1')
#' 
#' ## Bivariate sensitivity
#' pairs(local.rasa2, col = "green")
#' 
localsens = function(model.out = NULL, sensv = 'n') {
  sensFun(func = model.out$model, 
          parms = model.out$pars, 
          state = model.out$state,
          time = model.out$time,
          sensvar = sensv,
          varscale = 1)
}