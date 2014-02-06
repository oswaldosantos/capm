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
  
  dat2 <- function() {
    if (input$examples) {
      data(pilot)
      return(pilot)
    } else if (is.null(input$psu.x)) {
      return()
    } else {
      return(read.csv(input$psu.x$datapath,
                      sep=input$sep,
                      quote=input$quote,
                      header = input$header))
    }
  }
  
  output$size <- renderTable({
    if (is.null(dat()) | is.null(dat2())) {
      return()
    } else {
      Calculate2StageSampleSize(dat(), dat2(), input$level,
                                input$error, input$cost)
    }
  }, align = c('l', 'r'))
  
})                    