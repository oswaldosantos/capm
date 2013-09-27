#' Summary for class \code{capmModels}.
#' @usage summary(model.out, ...)
#' @method method for class 'capmModels'.
#' @rdname SolveStreIm
#' 
summary.capmModels <- function(model.out) {
  cat('\nModel function: ', model.out$name)
  cat('\nParameters:\n\n')
  print(model.out$pars)
  cat('\nInitial conditions:\n\n')
  print(model.out$init)
  cat('\nInitial time: ', model.out$time[1])
  cat('\n\nFinal time: ', model.out$time[length(model.out$time)])
}