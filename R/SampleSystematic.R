#' Systematic sampling
#' @description Select sampling units using systematic samplin. In the context of two-stage cluster sampling, select Secondary Sampling Units (SSU) in one or more Primary Sampling Units (PSU), using systematic sampling.
#' @param psu.ssu \code{\link{data.frame}} with all PSU. First column contains PSU unique identifiers. Second column contains \code{\link{numeric}} PSU sizes.
#' @param ssu \code{\link{numeric}} indicating SSU to be selected.
#' @param total \code{\link{numeric}} indicating the number of sampling units in the population. This is an alternative to \code{psu.ssu}, intended for simple sampling designs.
#' @param write logical. If \code{TRUE}, a *.csv file containing the psu and their ssu is writed in the current working directory.
#' @param ... further arguments passed to \code{\link{write.table}} function.
#' @return A \code{matrix}. The names of columns are the identifiers of selected psu, coerced by \code{\link{as.character}} to avoid scientific notation in case the identifiers be of \code{\link{class}} \code{\link{numeric}}. The rows correspond to the selected ssu within each psu.
#' @references Levy P and Lemeshow S (2008). Sampling of populations: methods and applications, Fourth edition. John Wiley and Sons, Inc.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @seealso \code{\link{SamplePPS}}.
#' @export
#' @examples 
#' # Load data with PSU identifiers and sizes.
#' data(psu.ssu)
#' 
#' # Take a sample of 10 PSU, with probability 
#' # proportional to size and with replacement.
#' selected.psu <- SamplePPS(psu.ssu, 10, write = FALSE)
#' 
#' # Take a sistematic sampling of 5 SSU within each 
#' # PSU of selected.psu.
#' (selected.ssu <- SampleSystematic(selected.psu, 5, write = FALSE))
#' 
#' # Simple systematic sampling
#' (simp.syst.sampling <- SampleSystematic(ssu = 5, total = 100))
SampleSystematic <- function(psu.ssu = NULL, ssu = NULL, total = NULL, write = FALSE, ...) {
  if (!is.null(total)) {
    int <- total / ssu
    k <- sample(int, 1)
    Ssu <- rep(NA, ssu)
    for(j in 1:ssu) {
      Ssu[j] <- k
      k <- k + int
    }
    ssus <- floor(Ssu)
    if (write == TRUE) {
      colnames(ssus) <- as.character(psu.ssu[ , 1])
      write.table(ssus, file = 'sampling_units.csv', sep = ',', dec = '.', 
                  qmethod = 'double', col.names = NA, ...)
      cat('\n', 'The \"sampling_units.csv\" file contains the selected SSU. \nIt is in the directory:', '\n\n', getwd(), '\n', '\n')
    }
    return(ssus)
  } else {
    inv <- c(which(!is.finite(psu.ssu[, 2])), which(psu.ssu[, 2] <= 0))
    if (length(inv) > 0) {
      stop('The size of the following sampling unit(s) is(are) invalid:', '\n', paste('   ', inv))
    }
    inv1 <- which(psu.ssu[, 2] < ssu)
    if (length(inv1) > 1) {
      stop('The number of secondary sampling units to be selected (', ssu, ') is greater than the size of the following primary sampling units:', '\n', paste('   ', inv1))
    }
    ssus <- matrix(rep(NA, nrow(psu.ssu) * ssu), ncol = nrow(psu.ssu))
    int <- psu.ssu[, 2] / ssu
    for(i in 1:length(int)) {
      k <- sample(int[i], 1)
      Ssu <- rep(NA, ssu)
      for(j in 1:ssu) {
        Ssu[j] <- k
        k <- k + int[i]
      }
      ssus[ , i] <- floor(Ssu)
    }
    colnames(ssus) <- as.character(psu.ssu[ , 1])
    if (write == TRUE) {
      colnames(ssus) <- as.character(psu.ssu[ , 1])
      write.table(ssus, file = 'ssus.csv', sep = ',', dec = '.', 
                  qmethod = 'double', col.names = NA, ...)
      cat('\n', 'The \"ssus.csv\" file contains the selected psu', '\n', 'and the their selected SSU. It is in the directory:', '\n\n', getwd(), '\n', '\n')
    }
    return(ssus)
  }
}
