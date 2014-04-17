library(rgdal)
shinyServer(function(input, output) {
  
  # Universe of PSUs and SSUs.
  dat <- function() {
    if (input$examples) {
      data(psu.ssu)
      return(psu.ssu)
    } else if (is.null(input$psu.ssu)) {
      return()
    } else {
      return(read.csv(input$psu.ssu$datapath,
                      sep = input$sep,
                      quote = input$quote,
                      header = input$header))
    }
  }
  
  # Selection of PSUs.
  psu <- function() {
    if (is.null(dat()) | !is.numeric(input$psu)) {
      return()
    } else {
      return(SamplePPS(dat(), input$psu))
    }
  }
  
  # Selection of SSUs or simple sampling units.
  ssu <- function() {
    if (is.numeric(input$total) & is.numeric(input$su)) {
      return(data.frame('Sampling_units' = SampleSystematic(total = input$total,
                                                            ssu = input$su)))
    } else if (is.null(psu()) | !is.numeric(input$ssu)) {
      return()
    } else {
      return(SampleSystematic(psu(), input$ssu))
    } 
  }
  
  # Shapefile with the universe of PSUs.
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
  
  # Write kml files of the selected PSUs.
  
  # Redefinitio of MapkmlPSU function from capm to write KMLs in a
  # user-defined directory (new argument: write.to.path).
  MapkmlPSU2 <- function (shape = NULL, psu = NULL, id = NULL,
                          path = '.', write.to.path = NULL) 
  {
    if (class(shape) == "SpatialPolygonsDataFrame") {
      tmp <- shape
    }
    else {
      tmp <- readOGR(path, shape)
    }
    tmp <- spTransform(tmp, CRS("+proj=longlat +ellps=WGS84"))
    tmp2 = NULL
    for (i in 1:length(psu)) {
      tmp1 <- tmp[which(as.character(tmp@data[, id]) == psu[i]), 
                  ]
      writeOGR(tmp1, dsn = paste0(write.to.path, '/', eval(psu[i]), ".kml"), 
               layer = 'selected_psu', driver = "KML",
               overwrite_layer = TRUE)
      tmp2[i] <- which(as.character(tmp@data[, id]) == psu[i])
    }
    tmp2 <- tmp[tmp2, ]
    if (file.exists("all_psu.kml")) {
      file.remove("all_psu.kml")
    }
    writeOGR(tmp2, dsn = paste0(write.to.path, '/', "all_psu.kml"),
             layer = 'all_selected_psu', overwrite_layer = TRUE,
             driver = "KML")
  }
  
  observe({
    if (input$examples) {
      shape.path <- system.file('extdata', package="capm")
      shape.name <- 'santos'
    } else {
      .name <- input$shape.name
    }
    if (input$kml == 0) {
      return()
    }
    isolate({
      if (input$examples) {
        shape.path <- system.file('extdata', package="capm")
      } else {
        shape.path <- input$shape.path
      }
      MapkmlPSU2(shape.name, psu()[ , 1], 1,
                 path = shape.path, write.to.path = input$write.to.path)
    })
  })
  
  output$selected <- renderTable(ssu())
  
  output$dataset <- renderDataTable({
    dat()
  }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  
  output$downloadData <- downloadHandler(
    filename = 'selected_psu.csv',
    content = function(file) {
      write.csv(ssu(), file, row.names = FALSE)
    }
  )
  
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