#' Plot results of capm model functions
#' @description Plot results of the following functions: \code{\link{sterowned}}.
#' @param model.out output of one of the function previously mentioned.
#' @param variable string to specify the variable to be ploted. If function is \code{\link{sterowned}}, possible values are: "n" (population size) or "q" (proportion of sterilized animals).
#' @param col string indicating the color of ploted line, when \code{ster.range} is not \code{NULL}.
#' @param col1 \code{\link{character}} \code{\link{vector}} indicating the color of lowest (highest) population sizes (proportion of sterilized animals), when \code{ster.range} is not \code{NULL}.
#' #' @param col2 \code{\link{character}} \code{\link{vector}} indicating the color of lowest (highest) population sizes (proportion of sterilized animals), when \code{ster.range} is not \code{NULL}.
#' @details Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' @seealso \link[deSolve]{plot.deSolve}.
#' @export
#' @examples 
#' #### example 1 - Sterowned function results ####
#' ## Parameters and initial conditions from estimates   
#' ## obtained in examples section from svysumm function.
#' pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
#' state.od <- c(n = 125027.411, q = 0.188)
#' 
#' # Solve for a specific sterilization rate.
#' ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)
#' 
#' # Solve for a range of sterilization rates.
#' ster.range.od <- sterowned(pars = pars.od, state = state.od, time = 0:30, ster.range = seq(0, .4, length.out = 50))
#'  
#' ## Plot the population size
#' plotmodel(ster.od, variable = 'n')
#' plotmodel(ster.range.od, variable = 'n')
#' 
#' ## Plot the proportion of sterilized animals
#' plotmodel(ster.od, variable = 'q')
#' plotmodel(ster.range.od, variable = 'q')
plotmodel = function(model.out = NULL, variable = NULL, col = 'red', col1 = c('yellow','blue'), col2 = c('green', 'orangered1')) {
  if (class(model.out) == 'sterowned') {
    if (ncol(model.out$results) == 3) {
      tmp = ggplot(model.out$results, 
                    aes_string(x = 'time', y = variable)) + 
        geom_line(colour = col) +
        xlab('Time')
      if (variable == 'n') {
        tmp + ylab('Population size') + 
          ylim(0, max(model.out$results['n']))
      } else {
        tmp + ylab('Proportion of sterilized animals') +
          ylim(0, 1)
      }
    } else {
      if (variable == 'n') {
        ggplot(
          model.out$results, 
          aes_string(x = 'time', y = 'ster.rate',
                     fill = variable)) +
          xlab('Time') + 
          ylab('Sterilization rate') +
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
          xlab('Time') + 
          ylab('Sterilization rate') +
          geom_raster() +
          scale_fill_continuous(name = 'Proportion of sterilized animals',
                                limits = c(0, 1), breaks=seq(0 , 1, .2),
                                low = rev(col2), 
                                high = rev(col1)) +
          theme(legend.position = 'top', 
                legend.title = element_text(size = 12))
      }
    }
  }
}