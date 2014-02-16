#' Plot results of CalculateLocalSens function
#' @description Plot results of the \code{\link{CalculateLocalSens}} function.
#' @param local.out output from \code{\link{CalculateLocalSens}} function.
#' @param type a number to define the type of graphical output. \code{6}: sensitivity functions and all importance indices are ploted; \code{1}: importance index L1; \code{2}:  importance index L2; \code{3}: mean of sensitivity functions; \code{5}: minimum of sensitivity functions; and \code{5}: maximum of sensitivity functions. 
#' @param x.sens string with the name of x axis for sensitivity functions.
#' @param y.sens string with the name of y axis for sensitivity functions.
#' @param y.ind string with the name of y axis for the parameter importance indices.
#' @param ax.size a number to specify the size of axes labels and text.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' 
#' Soetaert K, Cash J and Mazzia F (2012). Solving differential equations in R. Springer.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \link[FME]{plot.sensFun}.
#' @export
#' @examples 
#' #####################
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
#'                           time = 0:15, method = 'rk4')
#' 
#' ## Calculate local sensitivities to all parameters.
#' local.solve.iasa2 <- CalculateLocalSens(
#'   model.out = solve.iasa.pt, sensv = 'n2')
#' 
#' ## Plot local sensitivities
#' # Uncomment the following line:
#' # PlotLocalSens(local.solve.iasa2)
#' 
PlotLocalSens <- function(local.out = NULL, x.sens = "Time", y.sens = "Sensitivity", y.ind = c("L1", "L2", "Mean", "Min", "Max"), ax.size = 10, type = 6) {
  if (length(y.ind) != 5) {
    stop('The length of y.ind must be equal to 5.')
  }
  x <- value <- variable <- loc1 <- NULL
  loc2 <- loc3 <- loc4 <- loc5 <- NULL
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  } 
  tmp <- melt(local.out[, -2], id.vars = 'x')
  loc <- ggplot(
    tmp, aes(x = x, y = value, colour = variable)) + 
    geom_line(size = .5) +
    theme(legend.position="none") +
    xlab(x.sens) + 
    ylab(y.sens)
  
  tmp1 <- cbind(w = factor(
    rownames(summary(local.out)),
    levels = (rownames(summary(local.out)))),
                summary(local.out))
  coln <- colnames(summary(local.out))[-c(1:2, 8)]
  for (i in 1:length(coln)) {
    assign(
      paste0('loc', i),
      ggplot(tmp1, aes_string(x = 'w', y = coln[i], 
                              fill = 'w', colour = 'w')) +
        geom_bar(stat = 'identity') +
        theme(
          legend.position="none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = ax.size - 1, 
                                     angle = 90, vjust = .5),
          axis.title.y = element_text(size = ax.size + 2), 
          axis.text.y = element_text(size = ax.size)) +
        ylab(y.ind[i]))
  }
  if (type == 6) {
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
  if (type == 0) {
    print(loc + theme(legend.position="right"))
  }
  if (type == 1) {
    print(loc1)
  }
  if (type == 2) {
    print(loc1)
  }
  if (type == 3) {
    print(loc3)
  }
  if (type == 4) {
    print(loc4)
  }
  if (type == 5) {
    print(loc5)
  }
}