library(capm)

shinyServer(function(input, output) {

  SIASA <- function() {
    parms <- c(
      b1 = input$b1, b2 = input$b2, df1 = input$df1,
      dm1 = input$dm1, df2 = input$df2, dm2 = input$dm2,
      sf1 = input$sf1, sf2 = input$sf2, sm1 = input$sm1,
      sm2 = input$sm2, k1 = input$k1, k2 = input$k2,
      h1 = input$h1, h2 = input$h2, ab = input$ab,
      ad = input$ad, v = input$v, z = input$z)
    
    inits <- c(
      f1 = input$f1, fs1 = input$fs1,
      m1 = input$m1, ms1 = input$ms1,
      f2 = input$f2, fs2 = input$fs2,
      m2 = input$m2, ms2 = input$ms2)
    
    sol <- SolveIASA(pars = parms,
                     init = inits,
                     time = seq(0, input$time, by = input$t_steps))
    return(sol)
  }
  
  sensv <- function() {
    switch(input$sensv,
           sS = "S",
           sI = "I",
           sR = "R")
  }
  
  Local <- function() {
    return(CalculateLocalSens(DiffEqs(), sensv = sensv()))
  }
  
  GlobalAll <- function() {
    ranges <- SetRanges(c(beta = input$beta, gamma = input$gamma))
    glob.all <- CalculateGlobalSens(DiffEqs(), ranges = ranges,
                                    sensv = sensv(), all = T)
    return(glob.all)
  }
  
  Global <- function() {
    ranges <- SetRanges(c(beta = input$beta, gamma = input$gamma))
    glob <- CalculateGlobalSens(DiffEqs(), ranges = ranges,
                                    sensv = sensv())
    return(glob)
  }
  
  output$points_p <- renderPlot({
    plot(PlotModels(SIASA(), variable = "n2"))
  })
  
  output$table <- renderDataTable({
    DiffEqs()$results
  }, options = list(iDisplayLength = 10))
  
  output$local <- renderPlot({
    if (input$sensitivities == 0) {
      return()
    }
    
    isolate({
      PlotLocalSens(Local(), ax.size = 15) +
        theme(text=element_text(size=15),
              legend.title = element_blank())
    })
  }, height = 1000)
  
  output$globalall <- renderPlot({
    if (input$sensitivities == 0) {
      return()
    }
    
    isolate({
      pl <- PlotGlobalSens(GlobalAll(), ylab = "Variable") +
        theme(text=element_text(size=15),
              legend.title = element_blank())
      plot(pl)
    })
  })
  
  output$global <- renderPlot({
    if (input$sensitivities == 0) {
      return()
    }
    
    isolate({
      pl <- PlotGlobalSens(Global(), ylab = "Variable") +
        theme(text=element_text(size=15),
              legend.title = element_blank())
      plot(pl)
    })
  })
})