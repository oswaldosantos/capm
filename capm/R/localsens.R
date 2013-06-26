#' Local sensitivity analysis
#' @description Wraper for \code{\link{sensFun}} function which estimates local effect of all model parameters on population size, applying the so-called sensitivity functions. The set of parameters used in any of the following functions can be assessed: \code{\link{sterim}} or \code{\link{iasa}}.
#' @param model.out output of one of the following functions: \code{\link{sterim}} or \code{\link{iasa}}.
#' @param sensv string or \code{\link{character}} \code{\link{vector}} with the name of the the output variables for which the sensitivity needs to be estimated.
#' @details For further arguments of \code{\link{sensFun}}, defaults are used. See the help page of this function for details. Methods for class "sensFun" can be used.
#' @return a \code{\link{data.frame}} of class \code{\link{sensFun}} containing the sensitivity functions. There is one row for each sensitivity variable at each independent time. The first column \code{x}, contains the time value; the second column \code{var}, the name of the observed variable; and remaining columns have the sensitivity parameters.

#' @references Soetaert K and Petzoldt T (2010). Inverse modelling, sensitivity and monte carlo analysis in R using package FME. Journal of Statistical Software, 33(3), pp. 1-28.
#' 
#' Reichert P and Kfinsch HR (2001). Practical identifiability analysis of large environmental simulation models. Water Resources Research, 37(4), pp.1015-1030.
#' @seealso \code{\link{sensRange}}.
#' @export
#' @examples 
#' ##################
#' ## Example 1    ##
#' ## sterim model ##
#' ##################
#' 
#' ## Parameters and intial conditions from estimates 
#' ## obtained in examples section from the svysumm function.
#' pars.sterim = c(b = 0.245, d = 0.101, 
#'              k = 98050.49, s = .048)
#' state.sterim = c(n = 89136.810, q = 0.198)
#' 
#' # Solve for a specific sterilization rate.
#' sterim.pt = sterim(pars = pars.sterim, state = state.sterim, 
#'                   time = 0:30, dd = 'b', imm = 100)
#' 
#' ## Calculate local sensitivities for all parameters.
#' local.sterim = localsens(model.out = sterim.pt, sensv = 'n')
#' 
#' ## Bivariate sensitivity
#' pairs(local.sterim, col = "green")
#' 
#' ################
#' ## Example 2  ##
#' ## iasa model ##
#' ################
#' 
#' ## Parameters and intial conditions.
#' pars.iasa = c(
#'    b1 = 21870.897, b2 = 4374.179,
#'    df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
#'    sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
#'    k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
#'    ab = 0.054, ad = 0.1, v = 0.1
#' )
#' state.iasa = c(
#'    f1 = 33425.19, cf1 = 10864.901,
#'    m1 = 38038.96, cm1 = 6807.759,
#'    f2 = 3342.519, cf2 = 108.64901,
#'    m2 = 3803.896, cm2 = 68.07759
#' )
#' 
#' # Solve for point estimates.
#' iasa.pt <- iasa(pars = pars.iasa,
#'                 state = state.iasa,
#'                 time = 0:30)
#' 
#' ## Calculate local sensitivities for all parameters.
#' local.iasa2 = localsens(model.out = iasa.pt, sensv = 'n2')
#' local.iasa1 = localsens(model.out = iasa.pt, sensv = 'n1')
#' 
#' ## Bivariate sensitivity
#' pairs(local.iasa2, col = "green")
#' 
localsens = function(model.out = NULL, sensv = 'n') {
  sensFun(func = model.out$model, 
          parms = model.out$pars, 
          state = model.out$state,
          time = model.out$time,
          sensvar = sensv,
          varscale = 1)
}