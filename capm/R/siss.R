#' Systematic sampling
#' @description Select secondary sampling units (ssu) in one or more primary sampling units (psu), using systematic sampling.
#' @param psu.ssu \code{\link{data.frame}} with all primary sampling units (psu). First column contains psu unique identifiers. Second column contains \code{\link{numeric}} psu sizes.
#' @param ssu the number of ssu to be selected.
#' @param write logical. If \code{TRUE}, a *.csv file containing the psu and their ssu is writed in the current working directory.
#' @param ... further arguments passed to \code{\link{write.table}} function.
#' @return A \code{matrix}. The names of columns are the identifiers of selected psu, coerced by \code{\link{as.character}} to avoid scientific notation in case the identifiers be of \code{\link{class}} \code{\link{numeric}}. The rows correspond to the selected ssu within each psu.
#' @references Levy P and Lemeshow S (1999). Sampling of populations: methods and applications, Second edition. John Wiley and Sons, Inc.
#' @seealso \code{\link{ppssr}}.
#' @export
#' @examples 
#' # Load data with psu identifiers and sizes.
#' data(psu.ssu)
#' 
#' # Take a sistematic sampling of 5 ssu in the first five psu of psu.ssu.
#' selected.ssu <- siss(psu.ssu[1:5, ], 5, write = TRUE)
#' selected.ssu
siss <- function(psu.ssu = NULL, ssu = NULL, write = FALSE, ...) {
  inv <- c(which(!is.finite(psu.ssu[, 2])), which(psu.ssu[, 2] <= 0))
  if (length(inv) > 0) {
    stop('The size of the following sampling unit(s) is(are) invalid:', 
         '\n', paste('   ', inv))
  }
  inv1 <- which(psu.ssu[, 2] < ssu)
  if (length(inv1) > 1) {
    stop('The number of secondary sampling units (', ssu, ') to be 
         selected is greater than the size of the following primary 
         sampling units:', '\n', paste('   ', inv1))
  }
  ssus <- matrix(rep(NA, nrow(psu.ssu) * ssu), ncol = nrow(psu.ssu))
  int <- psu.ssu[, 2] / ssu
  for(i in 1:length(int)) {
    k = sample(int[i], 1)
    Ssu = rep(NA, ssu)
    for(j in 1:ssu) {
      Ssu[j] = k
      k = k + int[i]
    }
    ssus[ , i] = floor(Ssu)
  }
  colnames(ssus) = as.character(psu.ssu[ , 1])
  if (write == TRUE) {
    colnames(ssus) = as.character(psu.ssu[ , 1])
    write.table(ssus, file = 'ssus.csv', sep = ',', dec = '.', 
                qmethod = 'double', col.names = NA, ...)
    cat('\n', 'The \"ssus.csv\" file contains the selected psu', '\n',    
        'and the their selected ssu. It is in the directory:',
        '\n\n', getwd(), '\n', '\n')
  }
  return(ssus)
}