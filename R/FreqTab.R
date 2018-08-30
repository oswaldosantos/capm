#' Frequency table of categorical variables
#' @description Calculates and sort the count and relative frequency of categories.
#' @param \code{\link{data.frame}} with categorical variables.
#' @param variables name or position of categorical variables. If more than one variable is provided, contingency frequencies are calculated.
#' @param rnd the number of decimal places (round) or significant digits (signif) to be used.
#' @param decreasing \code{\link{logical}} (default). If \code{TRUE}, frequencies will be sorted in decreasing order, if \code{FALSE}, they will be sorted in increasing order.
#' @seealso \link[base]{table} and \link[base]{sort}.
#' @return \code{\link{data.frame}}.
#' @export
#' @examples
#' data(hh)
#' FreqTab(hh, c("common_fates", "tolerance"))
#' 
FreqTab <- function(data = NULL, variables = NULL, rnd = 3, decreasing = TRUE, ...) {
  if (is.null(dim(data)) & is.null(variables)) {
    # This "if" is for compatibility with versions older than v0.12.14.
    variables <- data
    tmp <- as.data.frame(sort(round(table(variables, ...), 3),
                              decreasing = decreasing))
    names(tmp) <- c("Category", "Count")
    tmp$Proportion <- round(tmp$Count / sum(tmp$Count), rnd)
    return(tmp)
  }
  if (is.numeric(variables)) {
    variable = names(data)[variables]
  }
  data <- data %>%
    group_by_(.dots = variables) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    mutate(Proportion = round(Count / sum(Count), rnd))
  if (decreasing) {
    return(arrange(data, desc(Count)))
  } else {
    arrange(data, Count)
  }
}