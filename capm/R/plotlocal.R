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
#' ## Plot local sensitivities
#' plotlocal(local.sterim)
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
#' 
#' ## Plot local sensitivities
#' plotlocal(local.iasa2)
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
  scl = nchar(as.character(
    round(max(tmp[, 3])))) - 2
  if (min(tmp[, 3]) == 0 | max(tmp[, 3]) == 0) {
    yy = round(seq(min(tmp[, 3]), 
                   max(tmp[, 3]), length.out = 4))
  } else {
    yy = round(
      sort(c(seq(min(tmp[, 3]), max(tmp[, 3]), 
                 length.out = 4), 0)))
  }
  loc = ggplot(
    tmp, aes(x = x, y = value, colour = variable)) + 
    geom_line(size = .5) +
    theme(legend.position="none") +
    xlab(sens.x) + 
    ylab(paste0(sens.y, ' (x ', 10 ^ scl, ')')) +
    scale_y_continuous(breaks = yy, 
                       labels = round(yy / (10 ^ scl), 1))
  
  tmp1 = cbind(w = factor(
    rownames(summary(local.out)),
    levels = (rownames(summary(local.out)))),
    summary(local.out)
    )
  coln = colnames(summary(local.out))[-c(1:2, 8)]
  for (i in 1:length(coln)) {
    scl = nchar(as.character(
      round(max(abs(tmp1[, coln[i]]))))) - 2
    if (min(tmp1[, coln[i]]) == 0 | max(tmp1[, coln[i]]) == 0) {
      yy = round(seq(min(tmp1[, coln[i]]), 
                     max(tmp1[, coln[i]]), length.out = 4))
    } else {
      yy = round(
        sort(c(seq(min(tmp1[, coln[i]]), max(tmp1[, coln[i]]), 
                   length.out = 4), 0)))
    }
    assign(
      paste0('loc', i),
      ggplot(tmp1, aes_string(x = 'w', y = coln[i], 
                              fill = 'w', colour = 'w')) +
        geom_bar(stat = 'identity') +
        theme(
          legend.position="none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = size.ax - 1, 
                                     angle = 90, vjust = .5),
          axis.title.y = element_text(size = size.ax + 2), 
          axis.text.y = element_text(size = size.ax)) +
        ylab(paste0(ind.y[i], ' (x ', 10 ^ scl, ')')) +
        scale_y_continuous(breaks = yy, 
                         labels = round(yy / (10 ^ scl), 1)))
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