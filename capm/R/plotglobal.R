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
#' pars.od <- c(b = 0.242, d = 0.093, 
#'              k = 88113.544 * 1.1, s = .048)
#' state.od <- c(n = 88113.544, q = 0.203)
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