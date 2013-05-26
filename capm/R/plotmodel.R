#' Plot results of capm model functions
#' @description Plot results of the following functions: \code{\link{sterowned}} and \code{\link{rasa}}.
#' @param model.out output of one of the function previously mentioned.
#' @param variable string to specify the variable to be ploted. If function is \code{\link{sterowned}}, possible values are: "n" (population size) or "q" (proportion of sterilized animals). For \code{\link{rasa}} function, possible values are: "n" (total population size), "n1" (owned population size), "n2" (stray population size),
#' @param col string indicating the color of ploted line, when \code{ster.range} is \code{NULL}.
#' @param col1 \code{\link{character}} \code{\link{vector}} indicating the color of lowest (highest) population sizes (proportion of sterilized animals), when \code{ster.range} is not \code{NULL}.
#' @param col2 \code{\link{character}} \code{\link{vector}} indicating the color of lowest (highest) population sizes (proportion of sterilized animals), when \code{ster.range} is not \code{NULL}.
#' @param xlabel string with the name of x axis.
#' @param ylabel string with the name of y axis.
#' @param leglabel string with the name of legend, for plots of \code{\link{rasa}} output.
#' @param scenlabel string with the name of scenarios of \code{\link{rasa}} output, determined by the replacement rates. Within the string, use the expression __ in the location where you want to appear the value of the replacement rate. For line breaking, use \code{\\n} (see examples).
#' @param pop a value indicating the output of \code{\link{rasa}} to be ploted. When \code{NULL} (default), plots for owned and stray populations under scenarios created by replacement rate are created. If \code{1}, the plots of owned population for the minimum replacement rate are ploted. When \code{2}, the plots of stray population for the minimum replacement rate are ploted. If \code{3}, the plots of owned population for the maximum replacement rate are ploted. When \code{4}, the plots of owned population for the maximum replacement rate are ploted.
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
#' ## Parameters and initial conditions from estimates   
#' ## obtained in examples section from svysumm function.
#' pars.od <- c(b = 0.262, d = 0.075, 
#'              k = 82531.831 * 1.1, s = .056)
#' state.od <- c(n = 82531.831, q = 0.207)
#' 
#' # Solve for a specific sterilization rate.
#' ster.od <- sterowned(pars = pars.od, 
#'                      state = state.od, time = 0:30)
#' 
#' # Solve for a range of sterilization rates.
#' ster.range.od <- sterowned(
#'  pars = pars.od, 
#'  state = state.od, 
#'  time = 0:30, 
#'  ster.range = seq(0, .4, length.out = 50))
#'  
#' ## Plot the population size
#' plotmodel(ster.od, variable = 'n')
#' plotmodel(ster.range.od, variable = 'n')
#' 
#' ## Plot the proportion of sterilized animals
#' plotmodel(ster.od, variable = 'q')
#' plotmodel(ster.range.od, variable = 'q')
#' 
#' ################
#' ## Example 2  ##
#' ## rasa model ##
#' ################
#' 
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
#' # Solve for parameter ranges.
#' rasa.rg <- rasa(pars = pars.rasa, 
#'                 state = state.rasa, 
#'                 time = 0:30,
#'                 ster.range = seq(0, .5, length.out = 50), 
#'                 aban.range = c(0, .2), 
#'                 adop.range = c(0, .2),
#'                 repl.range = c(0, .1))
#'                 
#' ## Plot stray population sizes using point estimates
#' plotmodel(rasa.pt, variable = "n2")
#' 
#' ## Plot all scenarios and change the label for the scenarios.
#' plotmodel(rasa.rg, scenlabel = "Rec  \n(__ * k)")
#'
plotmodel = function(model.out = NULL, variable = NULL, col = 'red', col1 = c('yellow','blue'), col2 = c('green', 'orangered1'), xlabel = 'Time', scenlabel = 'Rep = (__ * owned carrying capacity)', leglabel = c('Owned \npop size', 'Stray \npop size'), ylabel = NULL, pop = NULL) {
  if (class(model.out) == 'sterowned') {
    if (ncol(model.out$results) == 3) {
      tmp = ggplot(model.out$results, 
                    aes_string(x = 'time', y = variable)) + 
        geom_line(colour = col) +
        xlab(xlabel)
      if (variable == 'n') {
        if (is.null(ylabel)) {
          ylabel = 'Population size'
        }
        tmp + ylab(ylabel)
      } else {
        if (is.null(ylabel)) {
          ylabel = 'Proportion of sterilized animals'
        }
        tmp + ylab(ylabel)
      }
    } else {
      if (is.null(ylabel)) {
        ylabel = 'Population size'
      }
      if (variable == 'n') {
        ggplot(
          model.out$results, 
          aes_string(x = 'time', y = 'ster.rate',
                     fill = variable)) +
          xlab(xlabel) + 
          ylab(ylabel) +
          geom_raster() + 
          scale_fill_continuous(
            name = 'Population size',
            limits = c(0, max(model.out$results[, variable])), 
            breaks = round(
              seq(0 , max(model.out$results[, variable]),
                  length.out = 5)),
            low = col1,
            high = col2) +
          theme(legend.position = 'top',
                legend.title = element_text(size = 12))
      } else {
        ggplot(
          model.out$results, 
          aes_string(x = 'time', y = 'ster.rate',
                     fill = variable)) +
          xlab(xlabel) + 
          ylab(ylabel) +
          geom_raster() +
          scale_fill_continuous(
            name = 'Proportion of sterilized animals',
            limits = c(0, 1), breaks=seq(0 , 1, .2),
            low = rev(col2), 
            high = rev(col1)) +
          theme(legend.position = 'top', 
                legend.title = element_text(size = 12))
      }
    }
  } else {
    if (class(model.out) == 'rasa') {
      if (ncol(model.out$results) == 12) {
        tmp = ggplot(model.out$results, 
                     aes_string(x = 'time', y = variable)) + 
          geom_line(colour = col) +
          xlab(xlabel)
        if (is.null(variable)) {
          stop('"variable" argument can not be NULL.')
        }
        if (variable == 'n') {
          yla = 'Total pulation size' 
          yli = max(model.out$results['n'])
        }
        if (variable == 'n2') {
          yla = 'Stray pulation size' 
          yli = max(model.out$results['n2'])
        }
        if (variable == 'n1') {
          yla = 'Owned pulation size' 
          yli = max(model.out$results['n1'])
        }
        if (variable == 'sm2') {
          yla = 'Stray sterilized male pulation size' 
          yli = max(model.out$results['sm2'])
        }
        if (variable == 'm2') {
          yla = 'Stray male pulation size' 
          yli = max(model.out$results['m2'])
        }
        if (variable == 'sf2') {
          yla = 'Stray sterilized female pulation size' 
          yli = max(model.out$results['sf2'])
        }
        if (variable == 'f2') {
          yla = 'Stray female pulation size' 
          yli = max(model.out$results['f2'])
        }
        if (variable == 'sm1') {
          yla = 'Owned sterilized male pulation size' 
          yli = max(model.out$results['sm1'])
        }
        if (variable == 'm1') {
          yla = 'Owned male pulation size' 
          yli = max(model.out$results['m1'])
        }
        if (variable == 'sf1') {
          yla = 'Owned sterilized female pulation size' 
          yli = max(model.out$results['sf1'])
        }
        if (variable == 'f1') {
          yla = 'Owned female pulation size' 
          yli = max(model.out$results['f1'])
        }
        if (is.null(ylabel)) {
          tmp + ylab(yla)
        } else {
          tmp + ylab(ylabel)
        }
        
      } else {
        model.out$results[, 'aban'] = 
          paste('Ab =', round(model.out$results[, 'aban'], 2))
        model.out$results[, 'adop'] = 
          paste('Ad =', round(model.out$results[, 'adop'], 2))
        if (is.null(ylabel)) {
          ylabel = 'Sterilization rate'
        }
        s11 = s12 = s21 = s22 = NULL
        for (i in 1:2) {
          for (j in 1:2) {
            dat = model.out$results
            dat = dat[
              dat[, 'group'] == unique(dat[, 'group'])[j] &
                dat[, 'repl'] == unique(dat[, 'repl'])[i], ]
            scl = nchar(as.character(
              round(max(dat[, 'n'])))) - 2
            assign(paste0('s', i, j), 
                   ggplot(
                     dat,
                     aes_string(x = 't', y = 'ster',
                                fill = 'n')) +
                     xlab(xlabel) + 
                     ylab(ylabel) +
                     ggtitle(
                       gsub('__', unique(
                         model.out$results[, 'repl'])[i],
                            scenlabel)) +
                     geom_raster() + 
                     scale_fill_continuous(
                       name = paste0(leglabel[j], '\n',
                                     '(x ', 10 ^ scl, ')\n'),
                       limits = c(0, max(dat[, 'n'])), 
                       breaks = round(
                         seq(0 , max(dat[, 'n']),
                             length.out = 5)),
                       labels = round(round(
                         seq(0 , max(dat[, 'n']),
                             length.out = 5)) / (10 ^ scl), 1),
                       low = col1,
                       high = col2) +
                     theme(legend.position = 'right',
                           legend.title = 
                             element_text(size = 10, face = 'plain'),
                           plot.margin = 
                             unit(c(.5, 0, 0, 0), 'lines'),
                           plot.title = 
                             element_text(size = 12)) +
                     facet_grid(adop ~ aban)
            )
          }
        }
        if (!is.null(pop)) {
          if (pop == 1) {
            print(s11)
          }
          if (pop == 2) {
            print(s12)
          }
          if (pop == 3) {
            print(s21)
          }
          if (pop == 4) {
            print(s22)
          }
        } else {
          
        }
        if (is.null(pop)) {
          vplayout <- function(x, y) {
            viewport(layout.pos.row = x, layout.pos.col = y)
          } 
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(2, 2)))
          print(s11, vp = vplayout(1, 1))
          print(s21, vp = vplayout(1, 2))
          print(s12, vp = vplayout(2, 1))
          print(s22, vp = vplayout(2, 2))
        }
      }
    }
  }
}