shinyServer(function(input, output) {
  
  dat <- function() {
    if (input$examples) {
      data(psu.ssu)
      return(psu.ssu)
    } else if (is.null(input$psu.ssu)) {
      return()
    } else {
      return(read.csv(input$psu.ssu$datapath,
                      sep=input$sep,
                      quote=input$quote,
                      header = input$header))
    }
  }
  
  psu <- function() {
    if (is.null(dat()) | is.null(input$psu) |
          !is.numeric(input$psu)) {
      return()
    } else {
      return(SamplePPS(dat(), input$psu))
    }
  }
  
  map <- function() {
    if (input$examples) {
      shape.path <- system.file('extdata', package="capm")
      shape.name <- 'santos'
    } else {
      shape.path <- input$shape.path
      shape.name <- input$shape.name
    }
    return(readOGR(shape.path, shape.name))
    
  }
  
  output$selected <- renderTable({
    if (!is.na(input$total) & !is.na(input$su)) {
      if (input$total > 0 & input$su > 0) {
          data.frame(SSU = SampleSystematic(total = input$total,
                                            ssu = input$su))
      }
    } else if (is.null(dat()) | is.null(psu()) |
                 is.null(input$ssu)) {
      return()
    } else {
      SampleSystematic(psu(), input$ssu)
    }
  })
  
  output$map <- renderPlot({
    if (input$get.map == 0)
      return()
    
    isolate({
      if (!is.null(input$psu) & !is.null(dat())) {
        tmp <- NULL
        for (i in 1:length(psu()[ , 1])) {
          tmp[i] <- which(
            as.character(map()@data[, input$col]) == psu()[ , 1][i])
        }
        plot(map(), border = 'grey', axes = T, las = 1,
             xlab = 'Easting', ylab = 'Northing')
        plot(map()[tmp, ], col = 'red', border = 'red', add = T)
      }
    })
  })
  
})                    