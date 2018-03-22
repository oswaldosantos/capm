#' Distribution of households according to the number of inhabitants one or more species
#' @description Dodged bar plot of the distribution of households according to the number of inhabitants of one or more species.
#' @param dat \code{\link{data.frame}} with households as observation unit and columns with the number of individuals of the species of interest.
#' @param  species names or positions of columns with species data.
#' @param proportion \code{\link{logical}}. If \code{TRUE} (default), the y axis will represent proportions, if \code{FALSE}, it would represent raw counts.
#' @param x.label title for x axis.
#' @param y.label title for y axis.
#' @param legend \code{\link{logical}}. If \code{TRUE} (default), the legend will be showed, if \code{FALSE}, it will be removed.
#' @seealso \link[ggplot2]{geom_bar}.
#' @export
#' @examples
#' data(hh)
#' PlotHHxSpecies(hh, c("persons", "dogs", "cats"))
#' 
PlotHHxSpecies <- function(dat = NULL, species = NULL, proportion = TRUE, x.label = "Individuals per household", y.label = "Proportion of households", legend = TRUE) {
  freqs <- data.frame(Category = integer(),
                      Count = integer(),
                      Proportion = integer())
  rows <- c()
  dat <- as.data.frame(dat)
  for (i in 1:length(species)) {
    tmp <- FreqTab(dat[, i])
    freqs <- rbind.data.frame(freqs, tmp)
    rows <- c(rows, nrow(tmp))
  }
  freqs$Species <- rep(species, rows)
  freqs
  if (proportion) {
    freqs <- freqs[, -2]
  } else {
    freqs <- freqs[, -3]
  }
  names(freqs)[2] <- "y"
  plt <- ggplot(freqs, aes(Category, y, fill = Species)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab(x.label) +
    ylab(y.label)
  if (legend) {
    return(plt)
  } else {
    plt +
      theme(legend.position = "none")
  }
}