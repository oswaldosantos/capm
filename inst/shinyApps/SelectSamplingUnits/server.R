shinyServer(function(input, output) {
  
  dat <- function() {
    if (input$examples) {
      data(psu.ssu)
      return(psu.ssu)
    } else if (is.null(input$psu.ssu)) {
      return(NULL)
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
      return(NULL)
    } else {
      return(SamplePPS(dat(), input$psu))
    }
  }
  
  map <- function() {
    if (input$examples) {
      shp.path <- system.file('shp', package="capm")
    } #else {
      #shp.path <- ()
    #}
    options(warn = -1)
    return(readOGR(shp.path, 'santos'))
    
  }
  
  output$selected <- renderTable({
    if (is.null(dat()) | is.null(psu()) |
          is.null(input$ssu)) {
      return(NULL)
    } else {
      SampleSystematic(psu(), input$ssu)
    }
  })
  
  output$map <- renderPlot({
    if (input$psu & !is.null(input$psu) & !is.null(dat())) {
      tmp <- NULL
      for (i in 1:length(psu()[ , 1])) {
        tmp[i] <- which(as.character(map()@data[, 1]) == psu()[ , 1][i])
      }
      plot(map(), border = 'grey', axes = T, las = 1,
           xlab = 'Easting', ylab = 'Northing')
      plot(map()[tmp, ], col = 'red', border = 'red', add = T)
    }
  })
  
})