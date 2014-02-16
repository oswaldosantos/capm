#' Population PlotPopPyramid
#' @description Dysplays two opposed horizontal barplots (pyramid).
#' @param dat \code{\link{data.frame}}.
#' @param age.col the number or name of \code{dat} column which have a \code{\link{numeric}} \code{\link{vector}} representing ages or stage categories.
#' @param sex.col the number or name of \code{dat} column which have a \code{\link{factor}} with two levels representing the sex of individuals (see Details).
#' @param str.col the number of \code{dat} column which have a \code{\link{factor}} representing the reproductive status of individuals (see Details).
#' @param x.label a string to be used as a label for x-axis. If non defined, \code{x.label} is equal to "Total" (see Details).
#' @param stage.label a string to be used as a label for ages or stages categories. If non defined, \code{stage.label} is equal to "Years" (see Details).
#' @param inner.color any valid way to specify colors. When \code{str.col} is \code{NULL}, \code{inner.color} is the color of bars. When \code{str.col} is not \code{NULL}, \code{innercolor} is the inner color of bars. If non defined, \code{inner.color} is equal to \code{"Gold2"}.
#' @param outer.color any valid way to specify colors. When \code{str.col} is \code{NULL}, \code{outer.color} is ignored. When \code{str.col} is not \code{NULL}, \code{outer.color} is the outer color of bars. If non defined, \code{outercolor} is equal to \code{"DarkOliveGreen"}.
#' @details \code{PlotPopPyramid} is mainly intended for companion animals population pyramids, although it can display other types of opposed bar charts with suitable modification of the arguments.
#' 
#' The bars to the left of \code{0} on the x axis correspond to the minimum value of \code{\link{as.numeric}} (\code{dat[, sex.col]}). If \code{str.col} is not \code{NULL}, bars will be stacked, with the minimum value of \code{\link{as.numeric}}(\code{dat[, str.col]}) as their base.
#' 
#' On the top of the plot, it is displayed the total number of observations of each level of \code{dat[, sex.col]}. The \code{\link{levels}} of \code{sex.col} are used as \code{\link{labels}}.
#' 
#' The legend title is equal to \code{names(dat[, str.col])} and the legend \code{\link{labels}} are equal to the \code{\link{levels}} of \code{dat[, str.col]}.
#' 
#' Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package (see examples).
#'  
#' @note In companion animals population surveys, some age categories might be empty. One difference between \code{PlotPopPyramid} and \code{pryramid.plot} is that the first does not drop empty age categories.
#' @return Two opposed horizontal barplots.
#' @references \url{http://oswaldosantos.github.io/capm}
#' @seealso \link[plotrix]{pyramid.plot}.
#' @export
#' @examples 
#' ## Load data with information about age, sex and reproductive status of individuals.
#' data(survey.data)
#' # Uncomment the following lines:
#' # PlotPopPyramid(survey.data, age.col = 5, sex.col = 4, str.col = 6)
#' # PlotPopPyramid(survey.data, age.col = 5, sex.col = 4)
#' 
PlotPopPyramid <- function(dat = NULL, age.col = NULL, sex.col = NULL, str.col = NULL, x.label = 'Total', stage.label = 'Years', inner.color = 'Gold2', outer.color = 'DarkOliveGreen') {
  if (!is.numeric(dat[, age.col])) {
    stop('The column containing age information must be numeric.')
  }
  age <- sex <- ster <- count <- NULL
  if (!is.null(str.col)) {
    if (is.numeric(str.col)) {
      str.col <- names(dat)[str.col]
    }
    dat2 <- aggregate(dat, list(dat[, age.col],
                                dat[, sex.col],
                                dat[, str.col]),
                      length)
    dat2 <- dat2[ , 1:4]
    names(dat2) <- c('age', 'sex', 'ster', 'count')
  } else {
    dat2 <- aggregate(dat, list(dat[, age.col],
                                dat[, sex.col]),
                      length)
    dat2 <- dat2[ , 1:3]
    names(dat2) <- c('age', 'sex', 'count')
  }
  ylb <- max(dat2$count)
  while (ylb %% 10 != 0) {
    ylb <- ylb + 1
  }
  dat2[dat2$sex == levels(dat2$sex)[1], 'count'] <-
    dat2[dat2$sex == levels(dat2$sex)[1], 'count'] * (-1)
  dat.f <- dat2[which(dat2[, 2] == levels(dat2$sex)[1]), ]
  dat.f <- rbind(dat.f, c(max(dat2$age), rep(0, ncol(dat2) - 1)))
  dat.m <- dat2[which(dat2[, 2] == levels(dat2$sex)[2]), ]
  dat.m <- rbind(dat.m, c(max(dat2$age), rep(0, ncol(dat2) - 1)))
  #options(warn = -1)
  if (!is.null(str.col)) {
    plot.f <- ggplot(dat.f, aes(x = age, y = count , fill = ster)) +
      scale_fill_manual(values = c(inner.color, outer.color))
    plot.m <- ggplot(dat.m, aes(x = age, y = count , fill = ster)) +
      scale_fill_manual(values = c(inner.color, outer.color))
  } else {
    plot.f <- ggplot(dat.f, aes(x = age, y = count , fill = sex)) +
      scale_fill_manual(values = inner.color)
    plot.m <- ggplot(dat.m, aes(x = age, y = count , fill = sex)) +
      scale_fill_manual(values = inner.color)
  }
  plot.f <- plot.f +
    geom_bar(stat = 'identity') + coord_flip() +
    theme(legend.position = 'none',
          plot.margin = unit(c(0.5, 0, 0.5, 0.5), "lines"),
          axis.ticks.length = unit(0, 'lines'),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 11)) +
    scale_x_continuous(breaks = 0:max(dat2$age),
                       labels = 0:max(dat2$age)) +
    scale_y_continuous(breaks = seq(0, (ylb * -1), by = ylb / -5),
                       labels = seq(0, ylb, by = ylb / 5)) +
    ggtitle(paste(levels(dat[, sex.col])[1], ' = ',
                  summary(dat[, sex.col])[1])) +
    ylab(x.label)
  plot.m <- plot.m +
    geom_bar(stat = 'identity') + coord_flip() +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0), "lines"),
          axis.ticks.length = unit(0, 'lines'),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 11),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10)) +
    scale_x_continuous(breaks = 0:max(dat2$age),
                       labels = 0:max(dat2$age)) +
    scale_y_continuous(breaks = seq(0, ylb, by = ylb / 5),
                       labels = seq(0, ylb, by = ylb / 5)) +
    ggtitle(paste(levels(dat[, sex.col])[2], ' = ',
                  summary(dat[, sex.col])[2])) +
    labs(fill = str.col) +
    ylab(x.label)
  if (is.null(str.col)) {
    plot.m <- plot.m + theme(legend.position = 'none')
  }
  ages <- ggplot(data.frame(age = 0:max(dat2$age), count = 0),
                 aes(x = age, y = count)) +
    geom_bar(stat = 'identity') + coord_flip() +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          plot.margin = unit(c(0.5, 0, 0.5, 0), "lines"),
          axis.ticks.length = unit(0, 'lines'),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_continuous(breaks = 0, labels = '') +
    scale_x_continuous(breaks = 0:max(dat2$age),
                       labels = 0:max(dat2$age)) +
    annotate("text", y = 0, x = 0:max(dat2$age),
             label = 0:max(dat2$age)) +
    ylab('') +
    ggtitle(stage.label)
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 200)))
  print(plot.f, vp = vplayout(1, 1:92))
  print(plot.m, vp = vplayout(1, 108:200))
  print(ages, vp = vplayout(1, 93:107))
}