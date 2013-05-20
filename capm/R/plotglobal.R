#' Plot results of GlobalSens function
#' @description Plot results of of \code{\link{globalsens}} function.
#' @param global.out output from \code{\link{globalsens}} function.
#' @param nam.leg string with name for the legend.
#' @param mm.leg string with the name of the "envelope" calculated using the minimum and maximum ranges.
#' @param sd.leg string with the name of the "envelope" calculated using the mean +- standard deviation ranges.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' @seealso \link[deSolve]{plot.deSolve}.
#' @export
#' @examples 
#' #####################
#' ## Example 1       ##
#' ## sterowned model ##
#' #####################
#' 
#' ## Parameters and intial conditions from estimates
#' ## obtained in examples section from the svysumm function.
#' pars.od <- c(b = 0.167, d = 0.094,
#'              k = 125027.411 * 1.1, s = .059)
#' state.od <- c(n = 125027.411, q = 0.188)
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
#' ### Plot the sensitivities of combined parameters.
#' plotglobal(glob.od.all)
#' 
#' ### Plot the sensitivities of individual parameters.
#' plotglobal(glob.od)
#' 
#' ################
#' ## Example 2  ##
#' ## rasa model ##
#' ################
#' 
#' ## Parameters and intial conditions.
#' pars.rasa = c(
#'   af1 = 0.219, am1 = 0.219, af2 = 0.241, am2 = 0.241,
#'   bf1 = 0.091, bm1 = 0.091, bf2 = 0.1, bm2 = 0.1,
#'   ef1 = 0.074, ef2 = 0.01, em1 = 0.047, em2 = 0.01,
#'   k1 = 137176.8, k2 = 13854.86, z1 = 1, z2 = 1,
#'   h = 0.051, j = 0.111, v = 0.1
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
#' ### Plot the sensitivities of combined parameters.
#' plotglobal(glob.rasa.all)
#' 
#' ### Plot the sensitivities of individual parameters.
#' plotglobal(glob.rasa)

plotglobal <- function(global.out = NULL, nam.leg = 'Sensitivity range', mm.leg = 'min - max', sd.leg = 'mean +- sd   ') {
  x=Mean=Min=Max=Sd=NULL
  if (colnames(global.out)[length(global.out)] == 'param') {
    ggplot(global.out, aes(x = x, y = Mean)) +
      geom_ribbon(aes(ymin = Min, ymax = Max, fill = 'red'), 
                  alpha = .6) +
      geom_ribbon(aes(ymin = Mean - Sd, ymax = Mean + Sd, fill = 'blue'), 
                  alpha = .6) +
      geom_line() + facet_wrap( ~ param) +
      xlab('Time') + ylab('Population size')  +
      scale_fill_manual(
        name = nam.leg,
        values = c('blue', 'red'),
        labels = c(sd.leg, mm.leg)) +
      theme(legend.position = 'top') +
      guides(fill = guide_legend(
        override.aes = list(alpha = 0.5)))
  } else {
    ggplot(global.out, aes(x = x, y = Mean)) +
      geom_ribbon(aes(ymin = Min, ymax = Max, fill = 'red'), 
                  alpha = .6) +
      geom_ribbon(aes(ymin = Mean - Sd, ymax = Mean + Sd, fill = 'blue'), 
                  alpha = .6) +
      geom_line() +
      xlab('Time') + ylab('Population size')  +
      scale_fill_manual(
        name = nam.leg,
        values = c('blue', 'red'),
        labels = c(sd.leg, mm.leg)) +
      theme(legend.position = 'top') +
      guides(fill = guide_legend(
        override.aes = list(alpha = 0.4)))    
  }
}