shinyServer(function(input, output) {
  
  Universe <- function() {
    if (input$examples) {
      data(psu.ssu)
      return(psu.ssu)
    } else if (is.null(input$psu.ssu)) {
      return()
    } else {
      return(read.csv(input$universe$datapath,
                      sep=input$sep,
                      quote=input$quote,
                      header = input$header))
    }
  }
  
  SampleData <- function() {
    if (input$examples) {
      data(survey.data)
      return(survey.data)
    } else if (is.null(input$sample)) {
      return()
    } else {
      return(read.csv(input$sample$datapath,
                      sep=input$sep,
                      quote=input$quote,
                      header = input$header))
    }
  }
  
  Pyramid <- function() {
    if (is.null(SampleData()) | is.na(input$age.col) | is.na(input$sex.col)) {
      return()
    } else if (is.na(input$cas.col)) {
      return(PlotPopPyramid(SampleData(), input$age.col,
                            input$sex.col))
    } else {
      return(PlotPopPyramid(SampleData(), input$age.col,
                            input$sex.col, input$cas.col))
    }
  }
  
  Design <- function() {
    if (is.na(input$total)) {
      if (is.null(Universe()) | is.null(SampleData()) |
            is.na(input$psu.col) | is.na(input$ssu.col) |
            is.na(input$psu.2cdl)) {
        return()
      } else {
        return(DesignSurvey(Universe(), SampleData(),
                            input$psu.col, input$ssu.col,
                            psu.2cd = input$psu.2cdl))
      }
    } else {
      return(DesignSurvey(Universe(), SampleData(),
                          input$psu.col, input$ssu.col,
                          design = 'simple',
                          total = input$total))
    }
  }
    
    Variables <- function() {
      if (is.null(input$variables)) {
        return()
      } else if (is.na(input$total)) {
        no.spaces <- gsub(' +', '', input$variables)
        unquoted <- unlist(strsplit(no.spaces, ','))
        return(c('', '', unquoted, '', '', ''))
      } else {
        no.spaces <- gsub(' +', '', input$variables)
        unquoted <- unlist(strsplit(no.spaces, ','))
        return(c('', '', unquoted, ''))
      }
    }
    
    output$universe <- renderDataTable({
      Universe()
    }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
    
    output$sample <- renderDataTable({
      SampleData()
    }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
    
    output$summ.sample <- renderPrint({
      if (!is.null(SampleData())) {
        summary(SampleData())
      }
    })
    
    output$pyramid <- renderPlot({
      Pyramid()
    })
    
    output$variables <- renderTable({
      if (is.null(Design()) | is.null(Variables())) {
        return()
      } else {
        des.var <- names(Design()$variables)
        cbind(Variables = des.var, 'Tipe of estimate' = Variables())[
          -c(1:2, (length(des.var) - 2):length(des.var)), ]
      }
    })
    
    output$estimates <- renderTable({
      if (input$calc.estimates == 0) {
        return()
      }
      isolate({
        if (is.null(Design()) | is.null(Variables())) {
          return()
        } else {
          SummarySurvey(Design(), Variables(), level = input$level)
        }
      })
    })
    
  })