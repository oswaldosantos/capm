#' Population PlotPopPyramid
#' @description Displays two opposed horizontal barplots (pyramid).
#' @param dat \code{\link{data.frame}}.
#' @param age.col \code{dat} column that has a \code{\link{numeric}} \code{\link{vector}} representing ages or stage categories.
#' @param sex.col \code{dat} column that has two unique values representing the sex of individuals (see Details).
#' @param str.col \code{dat} column that has two unique values representing the reproductive status of individuals (see Details).
#' @param x.label string to be used as a label for the x axis. If undefined, \code{x.label} is equal to "Total" (see Details).
#' @param stage.label a string to be used as a label for the ages or stage categories. If undefined, \code{stage.label} is equal to "Years" (see Details).
#' @param legend.label a string to be used as a label for the legend. If undefined, \code{legend.label} is equal to "Sterilized".
#' @param inner.color any valid specification of a color. When \code{str.col} is not \code{NULL}, \code{inner.color} is the color of inner bars.
#' @param outer.color any valid way specification of a color. When \code{str.col} is \code{NULL}, \code{outer.color} is the default color. When \code{str.col} is not \code{NULL}, \code{outer.color} is the outer color of bars.
#' @param label.size string to define the font size for labels.
#' @details \code{PlotPopPyramid} is mainly intended for companion animals population pyramids, although it can display other types of opposed bar charts.
#' 
#' The bars to the left of the x axis correspond to \code{sort(unique(dat[, sex.col]))[1]}. If \code{str.col} is not \code{NULL}, bars will be stacked, with \code{sort(unique(dat[, str.col]))[1]} as their base.
#' 
#' On the top of the plot, it is displayed the total number of observations of each \code{dat[, sex.col]} unique value. This unique values are used as \code{\link{labels}}.
#' 
#' The legend \code{\link{labels}} are equal to the \code{dat[, str.col]} unique values.
#' 
#' Font size of saved plots is usually different to the font size seen in graphic browsers. Before changing font sizes, see the final result in saved (or preview) plots.
#'  
#' Other details of the plot can be modifyed using appropriate functions from \code{ggplot2} package (see examples).
#'  
#' @note In companion animals population surveys, some age categories might be empty. One difference between \code{PlotPopPyramid} and \code{pryramid.plot} is that the first does not drop empty age categories.
#' @return Two opposed horizontal barplots.
#' @references \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples 
#' data(cluster_sample_animals)
#' dogs <- cluster_sample_animals[complete.cases(cluster_sample_animals), ]
#' dogs <- dogs[dogs$species == "dog", ]
#' PlotPopPyramid(dogs,
#'                age.col = "age",
#'                sex.col = "sex",
#'                str.col = "sterilized")
#' PlotPopPyramid(dogs,
#'                age.col = "age",
#'                sex.col = "sex")
#'
PlotPopPyramid <-  function (dat = NULL, age.col = NULL, sex.col = NULL, str.col = NULL, x.label = "Total", stage.label = "Years", legend.label = "Sterilized", inner.color = "LightBlue", outer.color = "DarkRed", label.size = 13) {
  dat <- as.data.frame(dat)
  age <- sex <- ster <- count <- unit <- NULL
  if (!is.null(str.col)) {
    if (is.numeric(str.col)) {
      str.col <- names(dat)[str.col]
    }
    dat2 <- aggregate(dat, list(dat[, age.col], dat[, sex.col], 
                                dat[, str.col]), length)
    dat2 <- dat2[, 1:4]
    names(dat2) <- c("age", "sex", "ster", "count")
  }
  else {
    dat2 <- aggregate(dat, list(dat[, age.col], dat[, sex.col]), 
                      length)
    dat2 <- dat2[, 1:3]
    names(dat2) <- c("age", "sex", "count")
  }
  ylb <- max(aggregate(dat2$count, list(dat2[, "age"], dat2[, "sex"]), sum)$x)
  while (ylb%%5 != 0) {
    ylb <- ylb + 1
  }
  dat2[dat2$sex == sort(unique(dat2$sex))[1], "count"] <-
    dat2[dat2$sex == sort(unique(dat2$sex))[1], "count"] * (-1)
  dat.f <- dat2[which(dat2[, 2] == sort(unique(dat2$sex))[1]), ]
  dat.f[nrow(dat.f), "sex"] <- sort(unique(dat2$sex))[1]
  dat.m <- dat2[which(dat2[, 2] == sort(unique(dat2$sex))[2]), ]
  if (is.numeric(dat2$age)) {
    age_categories <- 0:max(dat2$age)
    max_age <- max(dat2$age)
    max_age.m <- max(dat.m$age)
    max_age.f <- max(dat.f$age)
    if (max(dat.f$age) < max(dat2$age)) {
      dat.f <- rbind(dat.f, c(max(dat2$age), rep(NA, ncol(dat2) - 2), 0))
    }
    if (max(dat.m$age) < max(dat2$age)) {
      dat.m <- rbind(dat.m, c(max(dat2$age), rep(NA, ncol(dat2) - 2), 0))
    }
  } else {
    age_categories <- factor(levels(dat2$age), levels = levels(dat2$age))
    max_age <- tail(levels(dat2$age), 1)
    max_age.m <- tail(levels(dat.m$age), 1)
    max_age.f <- tail(levels(dat.f$age), 1)
    all.cats <- cbind.data.frame(age_categories,
                                 matrix(nrow = length(age_categories),
                                        ncol = ncol(dat2) - 2),
                                 rep(0, length(age_categories)))
    colnames(all.cats) <- names(dat2)
    for (i in ncol(dat.f)) {
      dat.f[, i] <- ifelse(is.na(dat.f[, i]), levels(dat.f[, i])[1], dat.f[, i])
      dat.m[, i] <- ifelse(is.na(dat.m[, i]), levels(dat.m[, i])[1], dat.m[, i])
    }
  }
  dat.m[nrow(dat.m), "sex"] <- sort(unique(dat2$sex))[2]
  if (!is.null(str.col)) {
    plot.f <- ggplot(dat.f, aes(x = age, y = count, fill = ster)) + 
      scale_fill_manual(values = c(inner.color, outer.color)) +
      theme_minimal()
    plot.m <- ggplot(dat.m, aes(x = age, y = count, fill = ster)) + 
      scale_fill_manual(name = legend.label, values = c(inner.color, 
                                                        outer.color)) +
      theme_minimal()
  }
  else {
    plot.f <- ggplot(dat.f, aes(x = age, y = count, fill = sex)) + 
      scale_fill_manual(values = outer.color) +
      theme_minimal()
    plot.m <- ggplot(dat.m, aes(x = age, y = count, fill = sex)) + 
      scale_fill_manual(values = outer.color) +
      theme_minimal()
  }
  if (is.numeric(dat2$age)) {
    plot.f <- plot.f + geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() +
      theme(legend.position = "none",
            plot.margin = unit(c(0.5, 0, 0.5, 0.5), "lines"),
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_blank(), axis.title.y = element_blank(), 
            axis.text.x = element_text(size = label.size),
            axis.title.x = element_text(size = label.size), 
            plot.title = element_text(size = label.size, hjust = 0.5)) + 
      scale_x_continuous(breaks = age_categories, labels = age_categories) + 
      scale_y_continuous(breaks = seq(0, (-ylb), by = ylb/-5), 
                         labels = seq(0, ylb, by = ylb/5),
                         limits = c(-ylb, 0)) +
      ggtitle(paste(sort(unique(dat[, sex.col]))[1], " = ",
                    table(dat[, sex.col])[1])) +
      ylab(x.label)
    plot.m <- plot.m + geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() +
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1),
            plot.margin = unit(c(0.5, 1, 0.5, 0), "lines"), 
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = label.size), 
            axis.title.x = element_text(size = label.size), 
            legend.title = element_text(face = "plain", size = label.size), 
            legend.text = element_text(size = label.size),
            plot.title = element_text(size = label.size, hjust = 0.5)) +
      scale_x_continuous(breaks = age_categories, labels = age_categories) +
      scale_y_continuous(breaks = seq(0, ylb, by = ylb/5),
                         labels = seq(0, ylb, by = ylb/5), 
                         limits = c(0, ylb)) +
      ggtitle(paste(sort(unique(dat[, sex.col]))[2], " = ",
                    table(dat[, sex.col])[2])) + labs(fill = str.col) + 
      ylab(x.label)
    if (is.null(str.col)) {
      plot.m <- plot.m + theme(legend.position = "none")
    }
    ages <- ggplot(data.frame(age = age_categories, count = 0), 
                   aes(x = age, y = count)) + geom_bar(stat = "identity") + 
      coord_flip() +
      theme_minimal() +
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1),
            plot.margin = unit(c(0.5, 0, 0.5, 0), "lines"), 
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = label.size), 
            axis.title.x = element_text(size = label.size),
            plot.title = element_text(size = label.size)) + 
      scale_y_continuous(breaks = 0, labels = "") +
      scale_x_continuous(breaks = age_categories, labels = age_categories) +
      annotate("text", y = 0, x = age_categories,
               label = age_categories, size = label.size/3) + 
      ylab("") + ggtitle(stage.label)
  } else {
    plot.f <- plot.f + geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() +
      theme(legend.position = "none",
            plot.margin = unit(c(0.5, 0, 0.5, 0.5), "lines"),
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_blank(), axis.title.y = element_blank(), 
            axis.text.x = element_text(size = label.size),
            axis.title.x = element_text(size = label.size), 
            plot.title = element_text(size = label.size, hjust = 0.5)) + 
      scale_x_discrete(breaks = age_categories, labels = age_categories) + 
      scale_y_continuous(breaks = seq(0, (-ylb), by = ylb/-5), 
                         labels = seq(0, ylb, by = ylb/5),
                         limits = c(-ylb, 0)) +
      ggtitle(paste(sort(unique(dat[, sex.col]))[1], " = ",
                    table(dat[, sex.col])[1])) +
      ylab(x.label)
    plot.m <- plot.m + geom_bar(stat = "identity") + coord_flip() +
      theme_minimal() +
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1),
            plot.margin = unit(c(0.5, 1, 0.5, 0), "lines"), 
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = label.size), 
            axis.title.x = element_text(size = label.size), 
            legend.title = element_text(face = "plain", size = label.size), 
            legend.text = element_text(size = label.size),
            plot.title = element_text(size = label.size, hjust = 0.5)) +
      scale_x_discrete(breaks = age_categories, labels = age_categories) +
      scale_y_continuous(breaks = seq(0, ylb, by = ylb/5),
                         labels = seq(0, ylb, by = ylb/5), 
                         limits = c(0, ylb)) +
      ggtitle(paste(sort(unique(dat[, sex.col]))[2], " = ",
                    table(dat[, sex.col])[2])) + labs(fill = str.col) + 
      ylab(x.label)
    if (is.null(str.col)) {
      plot.m <- plot.m + theme(legend.position = "none")
    }
    ages <- ggplot(data.frame(age = age_categories, count = 0), 
                   aes(x = age, y = count)) + geom_bar(stat = "identity") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1),
            plot.margin = unit(c(0.5, 0, 0.5, 0), "lines"), 
            axis.ticks.length = unit(0, "lines"),
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = label.size), 
            axis.title.x = element_text(size = label.size),
            plot.title = element_text(size = label.size)) + 
      scale_y_continuous(breaks = 0, labels = "") +
      scale_x_discrete(breaks = age_categories, labels = age_categories) +
      annotate("text", y = 0, x = age_categories,
               label = age_categories, size = label.size/3) + 
      ylab("") + ggtitle(stage.label)
  }
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 200)))
  print(plot.f, vp = vplayout(1, 1:92))
  print(plot.m, vp = vplayout(1, 108:200))
  print(ages, vp = vplayout(1, 93:107))
}
