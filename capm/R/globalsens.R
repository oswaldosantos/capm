#' Global sensitivity analysis
#' @description Wraper for \code{\link{sensRange}} function which calculate sensitivities of population size, to parameters used in one of the following functions: \code{\link{sterowned}}.
#' @param model.out results of one of the following functions: \code{\link{sterowned}}
#' @param ranges output from the \code{\link{setranges}} function, applied to the \code{pars} argument used in the function specified previously in \code{model.out}.
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
#' #####################
#' ## Example 1       ##
#' ## sterowned model ##
#' #####################
#' 
#' ## Parameters and intial conditions from estimates
#' ## obtained in examples section from the svysumm function.
#' pars.od <- c(b = 0.242, d = 0.093, 
#'              k = 88113.544 * 1.1, s = .048)
#' state.od <- c(n = 88113.544, q = 0.203)
#'
#' 
#' # Solve for a specific sterilization rate.
#' ster.od <- sterowned(pars = pars.od,
#'                      state = state.od, time = 0:30)
#' 
#' ## Set ranges 10 % greater and lesser than the
#' ## point estimates.
#' par.rg.od.1 = setranges(pars = pars.od)
#' 
#' ## Calculate golobal sensitivity of combined parameters.
#' glob.od.all = globalsens(
#'   model.out = ster.od, ranges = par.rg.od.1,
#'   sensv = 'n', all = TRUE)
#' 
#' ## Calculate golobal sensitivity of individual parameters.
#' glob.od = globalsens(model.out = ster.od,
#' ranges = par.rg.od.1, sensv = 'n')
#' 
#' ################
#' ## Example 2  ##
#' ## rasa model ##
#' ################
#' 
#' ## Parameters and intial conditions.
#' pars.rasa = c(
#'    b1 = 21351.829, b2 = 3202.774,
#'    df1 = 0.081, dm1 = 0.069, df2 = 0.093, dm2 = 0.079,
#'    sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
#'    k1 = 96924.9, k2 = 9692.49, h1 = 1, h2 = .5,
#'    ab = 0.066, ad = 0.05, v = 0.12
#' )
#' state.rasa = c(
#'    f1 = 44576.299, cf1 = 11059.615,
#'    m1 = 43537.245, cm1 = 6827.960,
#'    f2 = 4457.630, cf2 = 222.8815,
#'    m2 = 4353.724, cm2 = 217.6862
#' )
#' 
#' # Solve for point estimates.
#' rasa.pt <- rasa(pars = pars.rasa, 
#'                 state = state.rasa, 
#'                 time = 0:30)
#' 
#' ## Set ranges 10 % greater and lesser than the
#' ## point estimates.
#' par.rg.rasa.1 = setranges(pars = pars.rasa)
#' 
#' ## Calculate golobal sensitivity of combined parameters.
#' glob.rasa.all = globalsens(
#'   model.out = rasa.pt,
#'   ranges = par.rg.rasa.1, sensv = 'n2', all = TRUE)
#' 
#' ## Calculate golobal sensitivity of individual parameters.
#' glob.rasa = globalsens(
#'   model.out = rasa.pt, ranges = par.rg.rasa.1,
#'   sensv = 'n2')
#' 
globalsens = function(model.out = NULL, ranges = NULL, sensv = NULL, all = FALSE) {
  if (class(model.out) == 'sterowned' |
        class(model.out) == 'rasa') {
    if (all == T) {
      sens = sensRange(func = model.out$model, 
                       parms = model.out$pars, 
                       state = model.out$state,
                       time = model.out$time,
                       parRange = ranges,
                       dist = "latin", 
                       sensvar = sensv, num = 100)
      return(summary(sens))
    } else {
        sens = NULL
        for (i in 1:length(model.out$pars)) {
          tmp = sensRange(func = model.out$model, 
                           parms = model.out$pars, 
                           state = model.out$state,
                           time = model.out$time,
                           parRange = ranges[i, ],
                           dist = "grid", 
                           sensvar = sensv, num = 100)
          sens = rbind(sens, summary(tmp))
        }
        param = rep(names(model.out$pars), 
                     each = length(tmp[-1]))
        sens = cbind(sens, param)
        return(sens)
    }
  }
}