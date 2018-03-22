#' Frequency table of categorical variables
#' @description Calculates and sort the count and relative frequency of categories.
#' @param variable a categorical variable.
#' @param rnd the number of decimal places (round) or significant digits (signif) to be used.
#' @param decreasing \code{\link{logical}} (default). If \code{TRUE}, frequencies will be sorted in decreasing order, if \code{FALSE}, they will be sorted in increasing order.
#' @seealso \link[base]{table} and \link[base]{sort}.
#' @return \code{\link{data.frame}}.
#' @export
#' @examples
#' data(hh)
#' FreqTab(hh$common_fates)
#' 
FreqTab <- function(variable = NULL, rnd = 3, decreasing = TRUE, ...) {
  tmp <- as.data.frame(sort(round(table(variable, ...), 3),
                            decreasing = decreasing))
  names(tmp) <- c("Category", "Count")
  tmp$Proportion <- round(tmp$Count / sum(tmp$Count), rnd)
  tmp
}