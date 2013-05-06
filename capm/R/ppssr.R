#' Probability proportional to size sampling with replacement
#' @description Select primary sampling units (psu) with probability proportional to size with replacement.
#' @param psu.ssu \code{\link{data.frame}} with all primary sampling units (psu). First column contains psu unique identifiers. Second column contains \code{\link{numeric}} psu sizes.
#' @param psu the number of psu to be selected.
#' @param write logical. If \code{TRUE}, a *.csv file containing the psu and their ssu is writed in the current working directory.
#' @param ... further arguments passed to \code{\link{write.table}} function.
#' @return \code{\link{data.frame}}. First column contains the selected psu identifiers, coerced by \code{\link{as.character}}, to avoid scientific notation in case the identifiers be of \code{\link{class}} \code{\link{numeric}}. Second column contain psu sizes, a variable needed for second stage sampling with \code{\link{siss}}.
#' @references Levy P and Lemeshow S (1999). Sampling of populations: methods and applications, Second edition. John Wiley and Sons, Inc.
#' @seealso \link[pps]{pps1}, \link[pps]{ppss}, \code{\link{siss}}.
#' @export
#' @examples 
#' # Load data with psu identifiers and sizes.
#' data(psu.ssu)
#' 
#' # Take a sample of 20 psu with probability proportional to size with replacement.
#' selected.psu = ppssr(psu.ssu, 20, write = FALSE)
#' selected.psu
ppssr = function (psu.ssu = NULL, psu = NULL, write = FALSE, ...) {
  inv = c(which(!is.finite(psu.ssu[, 2])), which(psu.ssu[, 2] <= 0))
  if (length(inv) > 0) {
    stop('The size of the following sampling unit(s) is(are) invalid:', 
         '\n', paste('   ', inv))
  }
  if (psu > nrow(psu.ssu)) {
    stop('The number of selected sampling units (', psu, ') is greater 
         than the total number of sampling units in the population (', 
         nrow(psu.ssu), ').')
  }
  inv2 = which(psu.ssu[, 1] == psu.ssu[, 1][duplicated(psu.ssu[, 1])])
  if (length(inv2) > 1) {
    stop('The following psu are repeated:', '\n', 
         paste('   ', psu.ssu[, 1][duplicated(psu.ssu[, 1])]), '\n', 
         'It appears in positions:', '\n', paste('   ', inv2))
  }
  M = nrow(psu.ssu) 
  cum = cumsum(psu.ssu[ , 2]) 
  N = cum[M] 
  Psu = data.frame('selected psu' = rep(NA, psu), size = rep(NA, psu)) 
  for (i in 1:psu) { 
    a = runif(1, 0, N) 
    j = 1
    while (cum[j] < a) { 
      j = j + 1
    }
    Psu[i, ] = psu.ssu[j, ] 
  }
  Psu[, 1] = as.character(Psu[,1])
  if (write == T) {
    write.table(Psu, file = 'selected_psu.csv', sep = ',', dec = '.', 
                qmethod = 'double', row.names = FALSE, ...)
    cat('\n', 'The \"selected_psu.csv\" file contains the selected', '\n', 'psu and their sizes (ssu). It is in the directory:', '\n\n', getwd(), '\n', '\n')
  }
  return(Psu)
}