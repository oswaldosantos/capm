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
#' pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
#' state.od <- c(n = 125027.411, q = 0.188)
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
#' af1 = 0.219, am1 = 0.219, af2 = 0.241, am2 = 0.241,
#' bf1 = 0.091, bm1 = 0.091, bf2 = 0.1, bm2 = 0.1,
#' ef1 = 0.074, ef2 = 0.01, em1 = 0.047, em2 = 0.01,
#' k1 = 137176.8, k2 = 13854.86, z1 = 1, z2 = 1,
#' h = 0.051, j = 0.111, v = 0.1
#' )
#' state.rasa = c(
#'   f1 = 46181.12, sf1 = 13309.497,
#'   m1 = 49681.91, sm1 = 15533.682,
#'   f2 = 5949.062, sf2 = 59.491,
#'   m2 = 6521.56, sm2 = 65.216
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