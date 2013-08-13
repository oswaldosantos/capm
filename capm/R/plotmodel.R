#' Plot results of capm model functions
#' @description Plot results of the following functions: \code{\link{sterim}} and \code{\link{iasa}}.
#' @param model.out output of one of the function previously mentioned.
#' @param variable string to specify the variable to be ploted. 
#' 
#' For \code{\link{sterim}} function: 
#' 
#' "n" (population size).
#' 
#' "q" (proportion of sterilized animals). 
#' 
#' For \code{\link{iasa}} function using with only point estimates:
#' 
#' "f1" (owned intact females).
#' 
#' "cf1" (owned sterilized females).
#' 
#' "m1" (owned intact males).
#' 
#' "cm1" (owned sterilized males).
#'
#' "f2" (stray intact females).
#' 
#' "cf2" (stray sterilized females).
#' 
#' "m2" (stray intact males).
#' 
#' "cm2" (stray sterilized males). 
#' 
#' "n1" (owned intact animals).
#' 
#' "cn1" (oned sterilized animals).
#' 
#' "n2" (stray intact animals).
#' 
#' "cn2" (stray sterilized animals).
#' 
#' "N1" (owned animals).
#' 
#' "N2" (stray animals).
#' 
#' "N" (total population).
#' 
#' For \code{\link{iasa}} function using *.range arguments:
#' 
#' "f" (intact females).
#' 
#' "cf" (sterilized females).
#' 
#' "m" (intact males).
#' 
#' "cm" (sterilized males).
#' 
#' "n" (intact animals).
#' 
#' "cn" (sterilized animals).
#' 
#' "N" (Total population stratified by reproductive status).
#' 
#' @param col string indicating the color of ploted line, when \code{ster.range} is \code{NULL}.
#' @param col1 \code{\link{character}} \code{\link{vector}} indicating the color of lowest (highest) population sizes (proportion of sterilized animals), when \code{ster.range} is not \code{NULL}.
#' @param col2 \code{\link{character}} \code{\link{vector}} indicating the color of lowest (highest) population sizes (proportion of sterilized animals), when \code{ster.range} is not \code{NULL}.
#' @param xlabel string with the name of x axis.
#' @param ylabel string with the name of y axis.
#' @param leglabel string with the name of legend, for plots of \code{\link{iasa}} output.
#' @param scenlabel string with the name of scenarios of \code{\link{iasa}} output, determined by the immigartion rates. Within the string, use the expression __ in the location where you want to appear the value of the immigartion rate. For line breaking, use \code{\\n} (see examples).
#' @param pop a value indicating the output of \code{\link{iasa}} to be ploted. When \code{NULL} (default), plots for owned and stray populations under scenarios created by immigartion rate are created. If \code{1}, the plots of owned population for the minimum immigartion rate are ploted. When \code{2}, the plots of stray population for the minimum immigartion rate are ploted. If \code{3}, the plots of owned population for the maximum immigartion rate are ploted. When \code{4}, the plots of owned population for the maximum immigartion rate are ploted.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' @seealso \link[deSolve]{plot.deSolve}.
#' @export
#' @examples
#' #####################
#' ## Example 1       ##
#' ## sterim model ##
#' #####################
#' 
#' ## Parameters and initial conditions from estimates   
#' ## obtained in examples section from svysumm function.
#' pars.sterim = c(b = 0.245, d = 0.101, 
#'              k = 98050.49, s = .048)
#' state.sterim = c(n = 89136.810, q = 0.198)
#' 
#' # Solve for a specific sterilization rate.
#' sterim.pt = sterim(pars = pars.sterim, state = state.sterim, 
#'                   time = 0:30, dd = 'b', imm = 100)
#' 
#' # Solve for a range of sterilization rates.
#' sterim.rg = sterim(
#'    pars = pars.sterim, state = state.sterim,
#'    time = 0:30, dd = 'b', imm = 100, 
#'    ster.range = seq(0, .4, length.out = 50))
#'  
#' ## Plot the population size
#' plotmodel(sterim.pt, variable = 'n')
#' plotmodel(sterim.rg, variable = 'n')
#' 
#' ## Plot the proportion of sterilized animals
#' plotmodel(sterim.pt, variable = 'q')
#' plotmodel(sterim.rg, variable = 'q')
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
#'    ab = 0.054, ad = 0.1, v = 0.2, vc = 0.1
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
#' # Solve for parameter ranges.
#' iasa.rg <- iasa(pars = pars.iasa, 
#'                 state = state.iasa, 
#'                 time = 0:30,
#'                 ster.range = seq(0, .5, length.out = 50), 
#'                 aban.range = c(0, .2), 
#'                 adop.range = c(0, .2),
#'                 imm.range = c(0, .1))
#'                 
#' ## Plot stray population sizes using point estimates
#' plotmodel(iasa.pt, variable = "n2")
#' 
#' ## Plot all scenarios and change the label for the scenarios.
#' plotmodel(iasa.rg, variable = 'N')
#'
plotmodel = function(model.out = NULL, variable = NULL, col = 'red', col1 = c('cadetblue1', 'yellow', 'red'), col2 = c('blue', 'darkgreen', 'darkred'), xlabel = 'Years', scenlabel = 'Imm = (__ * owned carrying capacity)', leglabel = NULL, ylabel = NULL, pop = NULL) {
  if (class(model.out) == 'sterim') {
    if (ncol(model.out$results) == 3) {
      tmp = ggplot(model.out$results, 
                    aes_string(x = 'time', y = variable)) + 
        geom_line(colour = col) +
        xlab(xlabel)
      if (is.null(variable)) {
        stop('"variable" argument can not be NULL.')
      }
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
    if (class(model.out) == 'iasa') {
      if (is.null(variable)) {
        stop('"variable" argument can not be NULL.')
      }
      if (ncol(model.out$results) == 16) {
        tmp = ggplot(model.out$results, 
                     aes_string(x = 'time', y = variable)) + 
          geom_line(colour = col) +
          xlab(xlabel)
        if (variable == 'f1') {
          yla = 'Owned intact females' 
          yli = max(model.out$results['f1'])
        }
        if (variable == 'cf1') {
          yla = 'Owned sterilized females' 
          yli = max(model.out$results['cf1'])
        }
        if (variable == 'm1') {
          yla = 'Owned intact males' 
          yli = max(model.out$results['m1'])
        }
        if (variable == 'cm1') {
          yla = 'Owned sterilized males' 
          yli = max(model.out$results['cm1'])
        }
        if (variable == 'f2') {
          yla = 'Stray intact females' 
          yli = max(model.out$results['f2'])
        }
        if (variable == 'cf2') {
          yla = 'Stray sterilized females' 
          yli = max(model.out$results['cf2'])
        }
        if (variable == 'm2') {
          yla = 'Stray intact males' 
          yli = max(model.out$results['m2'])
        }
        if (variable == 'cm2') {
          yla = 'Stray sterilized males' 
          yli = max(model.out$results['cm2'])
        }
        if (variable == 'n1') {
          yla = 'Owned intact animals' 
          yli = max(model.out$results['n1'])
        }
        if (variable == 'cn1') {
          yla = 'Owned sterilized animals' 
          yli = max(model.out$results['cn1'])
        }
        if (variable == 'n2') {
          yla = 'Stray intact animals' 
          yli = max(model.out$results['n2'])
        }
        if (variable == 'cn2') {
          yla = 'Stray sterilized animals' 
          yli = max(model.out$results['cn2'])
        }
        if (variable == 'N1') {
          yla = 'Owned animals' 
          yli = max(model.out$results['N1'])
        }
        if (variable == 'N2') {
          yla = 'Stray animals' 
          yli = max(model.out$results['N2'])
        }
        if (variable == 'N') {
          yla = 'Total pulation size' 
          yli = max(model.out$results['N'])
        }
        if (is.null(ylabel)) {
          tmp + ylab(yla)
        } else {
          tmp + ylab(ylabel)
        }
      } else {
        if (variable == 'f') {
          leglabel = c('Owned\nintact\nfemales', 
                       'Stray\nintact\nfemales')
        }
        if (variable == 'cf') {
          leglabel = c('Owned\nsterilized\nfemales', 
                       'Stray\nsterilized\nfemales')
        }
        if (variable == 'm') {
          leglabel = c('Owned\nintact\nmales', 
                       'Stray\nintact\nmales')
        }
        if (variable == 'cm') {
          leglabel = c('Owned\nsterilized\nmales', 
                       'Stray\nsterilized\nmales')
        }
        if (variable == 'n') {
          leglabel = c('Owned\nintact\nanimals', 
                       'Stray\nintact\nanimals')
        }
        if (variable == 'cn') {
          leglabel = c('Owned\nsterilized\nanimals', 
                       'Stray\nsterilized\nanimals')
        }
        if (variable == 'N') {
          leglabel = c('Owned\nanimals', 
                       'Stray\nanimals')
        }
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
                dat[, 'imm'] == unique(dat[, 'imm'])[i], ]
            scl = nchar(as.character(
              round(max(dat[, 'n'])))) - 2
            assign(paste0('s', i, j), 
                   ggplot(
                     dat,
                     aes_string(x = 't', y = 'ster',
                                fill = variable)) +
                     xlab(xlabel) + 
                     ylab(ylabel) +
                     ggtitle(
                       gsub('__', unique(
                         model.out$results[, 'imm'])[i],
                            scenlabel)) +
                     geom_raster() + 
                     scale_fill_continuous(
                       name = paste0(leglabel[j], '\n',
                                     '(x ', 10 ^ scl, ')\n'),
                       limits = c(0, max(dat[, variable])), 
                       breaks = round(
                         seq(0 , max(dat[, variable]),
                             length.out = 5)),
                       labels = round(round(
                         seq(0 , max(dat[, variable]),
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