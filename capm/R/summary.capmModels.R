#' Summary for class 'capmModels'
#' @usage summary(model.out, ...)
#' @s3method summary.capmModels
#' @export
print.capmModels <- function(model.out) {
  cat('\nModel function: ', model.out$name)
  cat('\n\nParameters:\n\n')
  print(model.out$pars)
  cat('\nInitial conditions:\n\n')
  print(model.out$init)
  cat('\nInitial time: ', model.out$time[1])
  cat('\n\nFinal time: ', model.out$time[length(model.out$time)])
}