#' Population change.
#' @description Calculate the change in population size between two times. When only one time is specified, the population size at that time is returned.
#' @param model.out an output from one of the following functions or a \code{\link{list}} with equivalent structure: \code{\link{SolveIASA}}, \code{\link{SolveSI}} or \code{\link{SolveTC}}.
#' @param variable string with the name of the the output variable for which the change needs to be calculated (see the variable argument for \code{\link{PlotModels}}.
#' @param t1 value specifying the first time.
#' @param t2 value specifying the second time.
#' @param ratio logical. When \code{TRUE}, the calculated change is based on poulation size at t2 divided by population size at t1. When \code{FALSE}, the calculated change is based on poulation size at t2 minus population size at t1.
#' @return Value representing the ratio (if \code{ratio} is \code{TRUE}) or the difference (if \code{ratio} is \code{FALSE}) between population size at time t2 and t1. If only one time is specified, the value is the population size at that time.
#' @references \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples
#' ###################
#' ## SolveSI model ##
#' ###################
#' 
#' # Parameters and initial conditions.
#' pars.solve.si = c(b = 0.245, d = 0.101, 
#'                      k = 98050.49, s = .048)
#' init.solve.si = c(n = 89136.810, q = 0.198)
#' 
#' # Solve for a specific sterilization rate.
#' solve.si.pt = SolveSI(pars = pars.solve.si, 
#'                              init = init.solve.si, 
#'                              time = 0:15, dd = 'b',
#'                              im = 100, method = 'rk4')
#' 
#' # Calculate the population change (ratio) between times 0 and 15.
#' CalculatePopChange(solve.si.pt, variable = 'n', t2 = 15, t1 = 0)
#' 
#' # Calculate the population change (difference) between times 0 and 15.
#' CalculatePopChange(solve.si.pt, variable = 'n', t2 = 15, t1 = 0, ratio = FALSE)
#' 
#' # Calculate the population zises at time 15.
#' CalculatePopChange(solve.si.pt, variable = 'n', t2 = 15)
#' 
CalculatePopChange <- function(model.out = NULL, variable = NULL, t1 = NULL, t2 = NULL, ratio = TRUE) {
  if (is.null(t1) & !is.null(t2)) {
    change <- model.out$results[model.out$results$time == t2, variable]
  }
  if (!is.null(t1) & is.null(t2)) {
    change <- model.out$results[model.out$results$time == t1, variable]
  }
  if (!is.null(t1) & !is.null(t2)) {
    if (ratio) {
      change <- model.out$results[model.out$results$time == t2, variable] /
        model.out$results[model.out$results$time == t1, variable]
    } else {
      change <- model.out$results[model.out$results$time == t2, variable] -
        model.out$results[model.out$results$time == t1, variable]
    }
  }  
  return(change)
}
