print.capmModels <- function(x, ...) {
  cat('\nModel function: ', model.out$name)
  cat('\n\nParameters:\n\n')
  model.out$pars
  cat('\nInitial conditions:\n\n')
  model.out$init
  cat('\nInitial time: ', model.out$time[1])
  cat('\n\nFinal time: ', model.out$time[length(model.out$time)])
  cat('Head:\n\n')
  head(model.out$results)
  cat('Tail:\n\n')
  tail(model.out$results)
}