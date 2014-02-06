#' Plot results of GlobalSens function
#' @description Plot results of of \code{\link{CalculateGlobalSens}} function.
#' @param global.out output from \code{\link{CalculateGlobalSens}} function.
#' @param nam.leg string with name for the legend.
#' @param xlabel string with the name of x axis.
#' @param ylabel string with the name of y axis.
#' @param mm.leg string with the name of the "envelope" calculated using the minimum and maximum ranges.
#' @param sd.leg string with the name of the "envelope" calculated using the mean +- standard deviation ranges.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package.
#' @seealso \link[deSolve]{plot.deSolve}.
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
#' #' ### Plot the sensitivities of combined parameters.
#' PlotGlobalSens(glob.all.solvesi)
#' 
#' ### Plot the sensitivities of individual parameters.
#' PlotGlobalSens(glob.solvesi)
#' 
#' #####################
#' ## Example 2       ##
#' ## SolveIASA model ##
#' #####################
#' 
#' ## Parameters and intial conditions.
#' pars.solve.iasa = c(
#'    b1 = 21870.897, b2 = 4374.179,
#'    df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
#'    sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
#'    k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
#'    ab = 0.054, ad = 0.1, v = 0.2, z = 0.1)
#'    
#' init.solve.iasa = c(
#'    f1 = 33425.19, fs1 = 10864.901,
#'    m1 = 38038.96, ms1 = 6807.759,
#'    f2 = 3342.519, fs2 = 108.64901,
#'    m2 = 3803.896, ms2 = 68.07759)
#'    
#' 
#' # Solve for point estimates.
#' solve.iasa.pt <- SolveIASA(pars = pars.solve.iasa, 
#'                           init = init.solve.iasa, 
#'                           time = 0:30, method = 'rk4')
#' 
#' ## Set ranges 10 % greater and lesser than the
#' ## point estimates.
#' rg.solve.iasa <- SetRanges(pars = pars.solve.iasa)
#' 
#' ## Calculate golobal sensitivity of combined parameters.
#' glob.all.solve.iasa <- CalculateGlobalSens(
#'   model.out = solve.iasa.pt,
#'   ranges = rg.solve.iasa, 
#'   sensv = 'n2', all = TRUE)
#' 
#' ## Calculate golobal sensitivity of an individual parameter.
#' glob.solve.iasa <- CalculateGlobalSens(
#'   model.out = solve.iasa.pt, 
#'   ranges = rg.solve.iasa,
#'   sensv = 'n2')
#' 
#' ### Plot the sensitivities of combined parameters.
#' PlotGlobalSens(glob.all.solve.iasa)
#' 
#' ### Plot the sensitivities of individual parameters.
#' PlotGlobalSens(glob.solve.iasa)
#'
PlotGlobalSens <- function(global.out = NULL, xlabel = 'Time', ylabel = 'Population', nam.leg = 'Sensitivity range', mm.leg = 'min - max', sd.leg = 'mean +- sd   ') {
  x <- Mean <- Min <- Max <- Sd <- NULL
  if (colnames(global.out)[length(global.out)] == 'param') {
    ggplot(global.out, aes(x = x, y = Mean)) +
      geom_ribbon(aes(ymin = Min, ymax = Max, fill = 'red'), 
                  alpha = .6) +
      geom_ribbon(aes(ymin = Mean - Sd, ymax = Mean + Sd, fill = 'blue'), 
                  alpha = .6) +
      geom_line() + facet_wrap( ~ param) +
      xlab(xlabel) + ylab(ylabel) +
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
      xlab(xlabel) + ylab(ylabel) +
      ylim(0, max(global.out$Max)) +
      scale_fill_manual(
        name = nam.leg,
        values = c('blue', 'red'),
        labels = c(sd.leg, mm.leg)) +
      theme(legend.position = 'top') +
      guides(fill = guide_legend(
        override.aes = list(alpha = 0.4)))    
  }
}
