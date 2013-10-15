print.capmModels <- function(x, ...) {
  cat('\nModel function: ', x$name)
  cat('\n\nParameters:\n\n')
  print(x$pars)
  cat('\nInitial conditions:\n\n')
  print(x$init)
  cat('\nInitial time: ', x$time[1])
  cat('\n\nFinal time: ', x$time[length(x$time)])
}