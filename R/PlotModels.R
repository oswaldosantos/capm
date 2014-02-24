#' Plot results of capm model functions
#' @description Plot results of one of the following functions: \code{\link{SolveIASA}}, \code{\link{SolveSI}} or \code{\link{SolveTC}}.
#' @param model.out output of one of the function previously mentioned.
#' @param variable string to specify the variable to be ploted. 
#' 
#' For \code{\link{SolveSI}} function: 
#' 
#' "n" (population size).
#' 
#' "q" (proportion of sterilized animals). 
#' 
#' For \code{\link{SolveIASA}} function using only point estimates:
#' 
#' "f1" (owned intact females).
#' 
#' "fs1" (owned sterilized females).
#' 
#' "m1" (owned intact males).
#' 
#' "ms1" (owned sterilized males).
#'
#' "f2" (stray intact females).
#' 
#' "fs2" (stray sterilized females).
#' 
#' "m2" (stray intact males).
#' 
#' "ms2" (stray sterilized males). 
#' 
#' "n1" (owned intact animals).
#' 
#' "ns1" (owned sterilized animals).
#' 
#' "n2" (stray intact animals).
#' 
#' "ns2" (stray sterilized animals).
#' 
#' "N1" (owned animals).
#' 
#' "N2" (stray animals).
#' 
#' "N" (total population).
#' 
#' For \code{\link{SolveIASA}} function using *.range arguments:
#' 
#' "f" (intact females).
#' 
#' "fs" (sterilized females).
#' 
#' "m" (intact males).
#' 
#' "ms" (sterilized males).
#' 
#' "n" (intact animals).
#' 
#' "ns" (sterilized animals).
#' 
#' "N" (Total population stratified by reproductive status).
#' 
#' For \code{\link{SolveTC}} function: 
#' 
#' "n" (fertile animals).
#' 
#' "ns" (sterilized animals).
#' 
#' "tns" (cumulative of sterilized animals)
#' 
#' @param col string indicating the color of ploted line, when \code{s.range} is \code{NULL}.
#' @param col1 \code{\link{character}} \code{\link{vector}} indicating the color of lowest (highest) population sizes (proportion of sterilized animals), when \code{s.range} is not \code{NULL}.
#' @param col2 \code{\link{character}} \code{\link{vector}} indicating the color of highest (lowest) population sizes (proportion of sterilized animals), when \code{s.range} is not \code{NULL}.
#' @param x.label string with the name of x axis.
#' @param y.label string with the name of y axis.
#' @param leg.label string with the name of legend, for plots of \code{\link{SolveIASA}} output.
#' @param scen.label string with the name of scenarios of \code{\link{SolveIASA}} output, determined by the immigartion rates. Within the string, use the expression __ in the location where you want to appear the value of the immigartion rate. For line breaking, use \code{\\n} (see examples).
#' @param pop value indicating the output of \code{\link{SolveIASA}} to be ploted. When \code{NULL} (default), plots for owned and stray populations under scenarios created by immigartion rate are created. If \code{1}, the plots of owned population for the minimum immigartion rate are ploted. When \code{2}, the plots of stray population for the minimum immigartion rate are ploted. If \code{3}, the plots of owned population for the maximum immigartion rate are ploted. When \code{4}, the plots of owned population for the maximum immigartion rate are ploted.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \link[deSolve]{plot.deSolve}.
#' @export
#' @examples
#' #####################
#' ## SolveIASA model ##
#' #####################
#' 
#' ## Parameters and intial conditions.
#' pars.solveiasa = c(
#'    b1 = 21870.897, b2 = 4374.179,
#'    df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
#'    sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
#'    k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
#'    ab = 0.054, ad = 0.1, v = 0.2, z = 0.1)
#'    
#' init.solveiasa = c(
#'    f1 = 33425.19, fs1 = 10864.901,
#'    m1 = 38038.96, ms1 = 6807.759,
#'    f2 = 3342.519, fs2 = 108.64901,
#'    m2 = 3803.896, ms2 = 68.07759)
#'    
#' 
#' # Solve for point estimates.
#' solveiasa.pt <- SolveIASA(pars = pars.solveiasa, 
#'                           init = init.solveiasa, 
#'                           time = 0:10, method = 'rk4')
#' 
#' # Solve for parameter ranges.
#' solveiasa.rg <- SolveIASA(pars = pars.solveiasa, 
#'                           init = init.solveiasa, 
#'                           time = 0:10,
#'                           s.range = seq(0, .4, l = 15), 
#'                           ab.range = c(0, .2), 
#'                           ad.range = c(0, .2),
#'                           im.range = c(0, .1),
#'                           method = 'rk4')
#'                 
#' ## Plot stray population sizes using point estimates
#' # Uncomment the following line:
#' # PlotModels(solveiasa.pt, variable = "ns2")
#' 
#' ## Plot all scenarios and change the label for the scenarios.
#' # Uncomment the following line:
#' # PlotModels(solveiasa.rg, variable = 'ns', scen.label = 'Im = (__ * de la capacidad de carga)')
#'
PlotModels <- function(model.out = NULL, variable = NULL, col = 'red', col1 = c('cadetblue1', 'yellow', 'red'), col2 = c('blue', 'darkgreen', 'darkred'), x.label = 'Years', y.label = NULL, scen.label = 'Im = (__ * owned carrying capacity)', leg.label = NULL, pop = NULL) {
  if (class(model.out) != 'capmModels') {
    stop('model.out must be of class "capmModels".')
  }
  if (model.out$name == 'SolveSI') {
    if (length(intersect(variable, c('n', 'q'))) == 0) {
      stop(paste0('Invalid variable: "', variable,
                  '". See the help page of PlotModels.'))
    }
    if (ncol(model.out$results) == 3) {
      tmp <- ggplot(model.out$results, 
                    aes_string(x = 'time', y = variable)) + 
        geom_line(colour = col) +
        xlab(x.label)
      if (!is.null(y.label)) {
        tmp + ylab(y.label) +
          ylim(0, max(model.out$results[ , variable]))
      } else {
        if (variable == 'n') {
          y.label <- 'Population size'
        }
        if (variable == 'q') {
          y.label <- 'Proportion of sterilized animals'
        }
        tmp + ylab(y.label) +
          ylim(0, max(model.out$results[ , variable]))
      }
    }  else {
      if (is.null(y.label)) {
        y.label <- 'Sterilization rate'
      }
      scl <- nchar(as.character(
        round(max(model.out$results[, variable])))) - 2
      if (variable == 'n') {
        if (is.null(leg.label)) {
          leg.label = 'Population size'
        }
        ggplot(
          model.out$results, 
          aes_string(x = 'time', y = 's.rate',
                     fill = variable)) +
          xlab(x.label) + 
          ylab(y.label) +
          geom_raster() + 
          scale_fill_continuous(
            name = paste0(leg.label,' (x ', 10 ^ scl, ')'),
            limits = c(0, max(model.out$results[, variable])), 
            breaks = round(
              seq(0 , max(model.out$results[, variable]),
                  length.out = 5) - 0.5),
            labels = round(
              seq(0 , max(model.out$results[, variable]),
                  length.out = 5) / (10 ^ scl), 1),
            low = col1,
            high = col2) +
          theme(legend.position = 'top',
                legend.title = element_text(size = 12))
      } else {
        if (is.null(leg.label)) {
          leg.label == 'Proportion of sterilized animals'
        }
        ggplot(
          model.out$results, 
          aes_string(x = 'time', y = 's.rate',
                     fill = variable)) +
          xlab(x.label) + 
          ylab(y.label) +
          geom_raster() +
          scale_fill_continuous(
            name = leg.label,
            limits = c(0, 1), breaks = seq(0 , 1, .2),
            low = rev(col2), 
            high = rev(col1)) +
          theme(legend.position = 'top', 
                legend.title = element_text(size = 12),
                legend.text = element_text(angle = 90))
      }
    }
  } else { 
    if (model.out$name == 'SolveIASA') {
      if (ncol(model.out$results) == 16) {
        if (length(intersect(variable, 
                             c('f1', 'fs1', 'm1', 'ms1',
                               'f2', 'fs2', 'm2', 'ms2',
                               'n1', 'ns1', 'n2', 'ns2',
                               'N1', 'N2', 'N'))) == 0) {
          stop(paste0('Invalid variable: "', variable,
                      '". See the help page of PlotModels.'))
        }
        tmp <- ggplot(model.out$results, 
                      aes_string(x = 'time', y = variable)) + 
          geom_line(colour = col) +
          xlab(x.label)
        if (!is.null(y.label)) {
          tmp + ylab(y.label) +
            ylim(0, max(model.out$results[ , variable]))
        } else {
          if (variable == 'f1') {
            yla <- 'Owned intact females'
          }
          if (variable == 'fs1') {
            yla <- 'Owned sterilized females'
          }
          if (variable == 'm1') {
            yla <- 'Owned intact males'
          }
          if (variable == 'ms1') {
            yla <- 'Owned sterilized males'
          }
          if (variable == 'f2') {
            yla <- 'Stray intact females'
          }
          if (variable == 'fs2') {
            yla <- 'Stray sterilized females'
          }
          if (variable == 'm2') {
            yla <- 'Stray intact males'
          }
          if (variable == 'ms2') {
            yla <- 'Stray sterilized males'
          }
          if (variable == 'n1') {
            yla <- 'Owned intact animals'
          }
          if (variable == 'ns1') {
            yla <- 'Owned sterilized animals'
          }
          if (variable == 'n2') {
            yla <- 'Stray intact animals'
          }
          if (variable == 'ns2') {
            yla <- 'Stray sterilized animals'
          }
          if (variable == 'N1') {
            yla <- 'Owned animals'
          }
          if (variable == 'N2') {
            yla <- 'Stray animals'
          }
          if (variable == 'N') {
            yla <- 'Total pulation size'
          }
          tmp + ylab(yla) +
            ylim(0, max(model.out$results[ , variable]))
        }
      } else {
        if (length(intersect(variable, 
                             c('f', 'fs', 'm', 'ms',
                               'n', 'ns', 'N'))) == 0) {
          stop(paste('Invalid variable: "', variable, 
                     '". See the help page of PlotModels.'))
        }
        if (is.null(leg.label)) {
          if (variable == 'f') {
            leg.label <- c('Owned\nintact\nfemales', 
                          'Stray\nintact\nfemales')
          }
          if (variable == 'fs') {
            leg.label <- c('Owned\nsterilized\nfemales', 
                          'Stray\nsterilized\nfemales')
          }
          if (variable == 'm') {
            leg.label <- c('Owned\nintact\nmales', 
                          'Stray\nintact\nmales')
          }
          if (variable == 'ms') {
            leg.label <- c('Owned\nsterilized\nmales', 
                          'Stray\nsterilized\nmales')
          }
          if (variable == 'n') {
            leg.label <- c('Owned\nintact\nanimals', 
                          'Stray\nintact\nanimals')
          }
          if (variable == 'ns') {
            leg.label <- c('Owned\nsterilized\nanimals', 
                          'Stray\nsterilized\nanimals')
          }
          if (variable == 'N') {
            leg.label <- c('Owned\nanimals', 
                          'Stray\nanimals')
          }
        }
        model.out$results[, 'ab'] = 
          paste('Ab =', round(model.out$results[, 'ab'], 2))
        model.out$results[, 'ad'] = 
          paste('Ad =', round(model.out$results[, 'ad'], 2))
        if (is.null(y.label)) {
          y.label <- 'Sterilization rate'
        }
        s11 <- s12 <- s21 <- s22 <- NULL
        for (i in 1:2) {
          for (j in 1:2) {
            dat <- model.out$results
            dat <- dat[
              dat[, 'group'] == unique(dat[, 'group'])[j] &
                dat[, 'im'] == unique(dat[, 'im'])[i], ]
            scl <- nchar(as.character(
              round(max(dat[, variable])))) - 2
            assign(paste0('s', i, j), 
                   ggplot(
                     dat,
                     aes_string(x = 't', y = 's',
                                fill = variable)) +
                     xlab(x.label) + 
                     ylab(y.label) +
                     ggtitle(
                       gsub('__', unique(
                         model.out$results[, 'im'])[i],
                            scen.label)) +
                     geom_raster() + 
                     scale_fill_continuous(
                       name = paste0(leg.label[j], '\n',
                                     '(x ', 10 ^ scl, ')\n'),
                       limits = c(0, max(model.out$results[
                         model.out$results$group == j, variable])), 
                       breaks = 
                         seq(0 , max(model.out$results[
                           model.out$results$group == j, variable]),
                             length.out = 5),
                       labels = round(
                         seq(0 , max(model.out$results[
                           model.out$results$group == j, variable]),
                             length.out = 5) / (10 ^ scl), 1),
                       low = col1,
                       high = col2) +
                     theme(legend.position = 'right',
                           legend.title = 
                             element_text(size = 10, face = 'plain'),
                           plot.margin = 
                             unit(c(.5, 0, 0, 0), 'lines'),
                           plot.title = 
                             element_text(size = 12)) +
                     facet_grid(ad ~ ab)
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
    } else {
      if (model.out$name == 'SolveTC') {
        if (length(intersect(variable, c('n', 'cn', 'tcn'))) == 0) {
          stop(paste0('Invalid variable: "', variable,
                      '". See the help page of PlotModels.'))
        }
        if (ncol(model.out$results) == 4) {
          tmp <- ggplot(model.out$results, 
                        aes_string(x = 'time', y = variable)) + 
            geom_line(colour = col) +
            xlab(x.label)
          if (!is.null(y.label)) {
            tmp + ylab(y.label) +
              ylim(0, max(model.out$results[ , variable]))
          } else {
            if (variable == 'n') {
              y.label <- 'Fertile animals'
            }
            if (variable == 'cn') {
              y.label <- 'Infertile animals'
            }
            if (variable == 'tcn') {
              y.label <- 'Sterilized animals (cumulative)'
            }
            tmp + ylab(y.label) +
              ylim(0, max(model.out$results[ , variable]))
          }
        } else {
          if (is.null(y.label)) {
            y.label <- 'Fertility recovery rate'
          }
          scl <- nchar(as.character(
            round(max(model.out$results[, variable])))) - 2
          if (is.null(leg.label)) {
            if (variable == 'n') {
              leg.label = 'Fertile animals'
            }
            if (variable == 'cn') {
              leg.label = 'Inertile animals'
            }
            if (variable == 'tcn') {
              leg.label = 'Sterilized animals (cumulative)'
            }
          }
          model.out$results[, 's'] <- 
            paste('S =', round(model.out$results[, 's'], 2))
          model.out$results[, 'z'] <- 
            paste('Z =', round(model.out$results[, 'z'], 2))
          ggplot(
            model.out$results,
            aes_string(x = 'time', y = 'f',
                       fill = variable)) +
            xlab(x.label) + 
            ylab(y.label) +
            geom_raster() + 
            scale_fill_continuous(
              name = paste0(leg.label,' (x ', 10 ^ scl, ')'),
              limits = c(0, max(model.out$results[, variable])), 
              breaks = round(
                seq(0 , max(model.out$results[, variable]),
                    length.out = 5) - 0.5),
              labels = round(
                seq(0 , max(model.out$results[, variable]),
                    length.out = 5) / (10 ^ scl), 1),
              low = col1,
              high = col2) +
            theme(legend.position = 'top',
                  legend.title = element_text(size = 12),
                  legend.text = element_text(angle = 90),
                  plot.margin = unit(c(.5, 0, 0, 0), 'lines')) +
            facet_grid(z ~ s)
        }
      }
    }
  }
}