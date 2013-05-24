#' Plot results of localsens function
#' @description Plot results of the \code{\link{localsens}} function.
#' @param local.out output from \code{\link{localsens}} function.
#' @param type a number to define the type of graphical output. When equal to \code{1}, both the sensitivity functions and all importance indices are ploted. If equal to \code{2}, the sensitivity function for each parameter and the importance indices L1 and L2 are ploted and if equal to \code{3}, just sensitivity functions are ploted.
#' @param sens.x string with the name of x axis for sensitivity functions.
#' @param sens.y string with the name of y axis for sensitivity functions.
#' @param ind.y \code{\link{character}} \code{\link{vector}} with the name of y axis for the parameter importance indices.
#' @param size.ax a number to specify the size of axes labels and text.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' 
#' Soetaert K, Cash J and Mazzia F (2012). Solving differential equations in R. Springer.
#' @seealso \link[FME]{plot.sensFun}.
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
#' ## Plot local sensitivities
#' plotlocal(local.od)
#' 
#' ################
#' ## Example 2  ##
#' ## rasa model ##
#' ################
#' 
#' ## Parameters and intial conditions.
#' pars.rasa = c(
#'    b1 = 21585.15, b2 = 2374.367,
#'    df1 = 0.081, dm1 = 0.069, df2 = 0.089, dm2 = 0.076,
#'    sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
#'    k1 = 90785.01, k2 = 9078.501, h1 = 1, h2 = 0.5, 
#'    ab = 0.065, ad = 0.095, v = 0.111
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
#' 
#' ## Plot local sensitivities
#' plotlocal(local.rasa2)
#' 
plotlocal = function(local.out = NULL, type = 1, sens.x = "Time", sens.y = "Sensitivity", ind.y = c("L1", "L2", "Mean", "Min", "Max"), size.ax = 10) {
  if (length(ind.y) != 5) {
    stop('The length of ind.y must be equal to 5.')
  }
  x=value=variable=loc1=loc2=loc3=loc4=loc5=NULL
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  } 
  tmp = melt(local.out[, -2], id.vars = 'x')
  loc = ggplot(
    tmp, aes(x = x, y = value, colour = variable)) + 
    geom_line(size = 1) +
    theme(legend.position="none") +
    xlab(sens.x) + ylab(sens.y)
  
  tmp1 = cbind(w = factor(
    rownames(summary(local.out)),
    levels = (rownames(summary(local.out)))),
    summary(local.out)
    )
  coln = colnames(summary(local.out))[-c(1:2, 8)]
  for (i in 1:length(coln)) {
    assign(
      paste0('loc', i),
      ggplot(tmp1, aes_string(x = 'w', y = coln[i], 
                              fill = 'w', colour = 'w')) +
        geom_bar(stat = 'identity') +
        theme(
          legend.position="none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = size.ax),
          axis.title.y = element_text(size = size.ax + 2), 
          axis.text.y = element_text(size = size.ax)) +
        ylab(ind.y[i]))
  }
  if (type == 1) {
    options(warn = -1)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(3, 2)))
    print(loc + theme(legend.position="none"),
          vp = vplayout(1, 1))
    print(loc1, vp = vplayout(1, 2))
    print(loc2, vp = vplayout(2, 1))
    print(loc3, vp = vplayout(2, 2))
    print(loc4, vp = vplayout(3, 1))
    print(loc5, vp = vplayout(3, 2))
    options(warn = 0)
  }
  if (type == 2) {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 2)))
    print(loc, vp = vplayout(1, 1:2))
    print(loc1, vp = vplayout(2, 1))
    print(loc2, vp = vplayout(2, 2))
  }
  if (type == 3) {
    print(loc + theme(legend.position="right"))
  }
}