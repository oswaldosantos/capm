#' Population pyramid
#' @description Dysplays two opposed horizontal barplots (pyramid).
#' @param dat \code{\link{data.frame}}.
#' @param col.age the number of \code{dat} column which have a \code{\link{numeric}} \code{\link{vector}} representing ages.
#' @param col.sex the number of \code{dat} column which have a \code{\link{factor}} with two levels representing the sex of individuals (see Details).
#' @param col.cas the number of \code{dat} column which have a \code{\link{factor}} representing the reproductive status of individuals (see Details).
#' @param xlabel a string to be used as a label for x-axis. If non defined, \code{xlabel} is equal to "Percentage" (see Details).
#' @param ylabel a string to be used as a label for y-axis. If non defined, \code{xlabel} is equal to "Age category" (see Details).
#' @param innercolor any valid way to specify colors. When \code{cas} is \code{NULL}, \code{innercolor} is the color of bars. When \code{cas} is not \code{NULL}, \code{innercolor} is the inner color of bars. If non defined, \code{innercolor} is equal to "Gold2".
#' @param outercolor any valid way to specify colors. When \code{cas} is \code{NULL}, \code{outercolor} is ignored. When \code{cas} is not \code{NULL}, \code{outercolor} is the outer color of bars. If non defined, \code{outercolor} is equal to "DarkOliveGreen".
#' @details \code{pyramid} is mainly intended for companion animals population pyramids, although it can display other types of opposed bar charts with suitable modification of the arguments.
#' 
#' The bars to the left of \code{0} on the x axis correspond to the minimum value of \code{\link{as.numeric}}(\code{dat[, col.sex]}). If \code{col.cas} is not \code{NULL}, bars will be stacked, with the minimum value of \code{\link{as.numeric}}(\code{dat[, col.cas]}) as their base.
#' 
#' On the top of the plot, it is displayed the total number of observations of each level of \code{dat[, col.sex]}. The \code{\link{levels}} of \code{col.sex} are used as \code{\link{labels}}.
#' 
#' The legend title is equal to \code{names(dat[, col.cas])} and the legend \code{\link{labels}} are equal to the \code{\link{levels}} of \code{dat[, col.cas]}.
#' 
#' \code{xlabels} and \code{ylabels} can be interpereted as refering to the horizontal and vertical axis respectively. However, because the plot is a rotated barplot, \code{pyramid} internally uses \code{xlabels} as an argument to the y-axis and \code{ylabels} to the x-axis. 
#' 
#' Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package (see examples).
#'  
#' @note In companion animals population surveys, some age categories might be empty. One difference between \code{pryramid} and \code{pryramid.plot} is that the first does not drop empty age categories.
#' @references Chang W (2012). R Graphics Cookbook. O'Reilly Media, Inc.
#' @seealso \link[plotrix]{pyramid.plot}.
#' @export
#' @examples 
#' ## Load data with information about age, sex and reproductive status of individuals.
#' data(Sample)
#' 
#' ## Make a population pyramid.
#' pyramid(Sample, col.age = 5, col.sex = 4, col.cas = 6)
#' 
#' # Put legend to the right.
#' pyram = pyramid(Sample, col.age = 5, col.sex = 4, col.cas = 6)
#' pyram = pyram + theme(legend.position = 'right')
#' pyram
#' 
#' # Increase legend font size.
#' pyram = pyram + theme(legend.title = element_text(size = 16), legend.text = element_text(size = 16))
#' pyram
#' 
#' # Increase axis labels font size.
#' pyram = pyram + theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16))
#' pyram
#' pyram = pyram + theme(axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 16))
#' pyram
#' 
#' #' # Increase sex labels font size.
#' pyram = pyram + theme(plot.title = element_text(size = 16))
#' pyram

pyramid = function(dat = NULL, col.age = NULL, col.sex = NULL, col.cas = NULL, xlabel = 'Percentage', ylabel = 'Age category', innercolor = 'Gold2', outercolor = 'DarkOliveGreen') {
  if (!is.numeric(dat[, col.age])) {
    stop('The column containing age information must be numeric.')
  }
  sex=..count..=NULL
  dat = dat[complete.cases(dat), ]
  xlb = dat[, col.age]
  xlb = seq(min(xlb), max(xlb))
  fac = nrow(dat) / 100
  tmp = tapply(dat[, col.age], dat[, col.sex], table)
  ylb = ceiling(max(c(tmp[[1]], tmp[[2]])) / fac)
  while (ylb %% 3 != 0) {
    ylb = ylb + 1
  }
  ylb = seq(-ylb, ylb, by = 3)
  options(warn = -1)
  if (!is.null(col.cas)) {
    pyr = ggplot(data=dat) + 
      aes_string(x = names(dat)[col.age], fill = names(dat)[col.cas]) +
      theme(plot.margin = unit(c(2,.5,1,.5), "lines")) +
      scale_fill_manual(values = c(innercolor, outercolor)) +
      geom_bar(subset=.(sex==levels(dat[, col.sex])[1])) + 
      geom_bar(subset=.(sex==levels(dat[, col.sex])[1]),aes(y=..count..*(-1)))
  } else {
    pyr = ggplot(data=dat) + 
      aes_string(x = names(dat)[col.age], fill = names(dat)[col.sex]) +
      theme(plot.margin = unit(c(2,.5,1,.5), "lines")) +
      geom_bar(subset=.(sex==levels(dat[, col.sex])[1]), fill = innercolor) + 
      geom_bar(subset=.(sex==levels(dat[, col.sex])[1]),aes(y=..count..*(-1)), , fill = innercolor)
  }
  pyr = pyr + geom_hline(aes(yintercept = 0), colour = 'white') +
    xlab(ylabel) +
    scale_x_discrete(breaks = xlb, labels = xlb, drop = F,
                     limits = min(xlb):max(xlb)) +
    ylab(xlabel) +
    scale_y_continuous(breaks = ylb * fac, labels = abs(ylb),
                       limits = c(min(ylb * fac), 
                                  max(ylb * fac))) +
    ggtitle(paste(levels(dat[, col.sex])[1], ' = ',
                  summary(dat[, col.sex])[1],
                  '                     ',
                  levels(dat[, col.sex])[2], ' = ',
                  summary(dat[, col.sex])[2], '  '
    )) +
    theme(legend.key.size = unit(.03, 'npc')) +
    theme(legend.position=c(.9, .9)) +
    coord_flip()
  return(pyr)
  options(warn = 0)
}