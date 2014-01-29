

shinyServer(function(input, output) {
  output$su <- renderTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    dat <- read.csv(inFile$datapath, sep=input$sep,
                    quote=input$quote, header = input$header)
    
    selected.psu <- SamplePPS(dat, input$psu)
    SampleSystematic(selected.psu, input$ssu, write = FALSE)
  })
  
})