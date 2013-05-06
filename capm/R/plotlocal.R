#' Plot results of localsens function
#' @description Plot results of of \code{\link{localsens}} function.
#' @param local.out output from \code{\link{localsens}} function.
#' @param type a number to define the type of graphical output. When equal to \code{1} (default), the sensitivity function for each parameter and the importance indices L1 and L2 are ploted. If equal to \code{2}, just sensitivity functions are ploted and if equal to \code{3}, both the sensitivity functions and all importance indices are ploted.
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
#' #### example 1 - sterowned function results ####
#' 
#' #' ## Parameters and intial conditions from estimates 
#' ## obtained in examples section from the svysumm function.
#' pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
#' state.od <- c(n = 125027.411, q = 0.188)
#' 
#' # Solve for a specific sterilization rate.
#' ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)
#' 
#' ## Calculate local sensitivities for all parameters.
#' local.od = localsens(model.out = ster.od)
#' 
#' ## Plot local sensitivities
#' plotlocal(local.od, type = 1)
#' @export
plotlocal = function(local.out = NULL, type = 1, sens.x = "Time", sens.y = "Sensitivity", ind.y = c("L1", "L2", "Mean", "Min", "Max"), size.ax = 10) {
  if (length(ind.y) != 5) {
    stop('The length of del.y must be equal to 5.')
  }
  x=value=variable=loc1=loc2=loc3=loc4=loc5=NULL
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  } 
  tmp = melt(local.out[, -2], id.vars = 'x')
  loc = ggplot(
    tmp, aes(x = x, y = value, colour = variable)) + 
    geom_line(size = 1) + 
    theme(legend.position="top", 
          legend.title = element_blank(),
          axis.title.x = element_text(size = size.ax + 2), 
          axis.text.x = element_text(size = size.ax),
          axis.title.y = element_text(size = size.ax + 2), 
          axis.text.y = element_text(size = size.ax)) +
    xlab(sens.x) + ylab(sens.y)
  
  tmp1 = cbind(w = rownames(summary(local.out)), 
               summary(local.out))
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
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 2)))
    print(loc, vp = vplayout(1, 1:2))
    print(loc1, vp = vplayout(2, 1))
    print(loc2, vp = vplayout(2, 2))
  }
  if (type == 2) {
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
  if (type == 3) {
    print(loc)
  }
}