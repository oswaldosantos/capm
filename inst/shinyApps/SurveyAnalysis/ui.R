shinyUI(pageWithSidebar(
  
  headerPanel("Survey analysis"),
  
  sidebarPanel(
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 1',
      helpText('Intro')),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 2',
      helpText('Choose a csv file having PSU unique identifiers in the first column and PSU sizes in the second column.'),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   'Semicolon'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   'Double Quote'),    
      fileInput('psu.ssu', '',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'))),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 3',
      helpText('Choose a csv file with sample data.'),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   'Semicolon'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   'Double Quote'),    
      fileInput('sample', '',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv'))),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 4',
      helpText('Survey design'),
      numericInput('psu.col',
                   'Column in sample data csv that contain PSU identifiers.',
                   value = NULL, min = 0),
      numericInput('ssu.col',
                   'Column in sample data csv that contain SSU identifiers.',
                   value = NULL, min = 0),
      numericInput('psu.2cdl',
                   'Number  of PSU included in the design.',
                   value = NULL, min = 0),
      numericInput('systematic',
                   'For systematic sampling, ingore the previous three fields and specify here the total number of sampling units in the population.',
                   value = NULL, min = 0)),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 5',
      helpText('Population pyramid.'),
      numericInput('age.col',
                   'Column in sample data csv containing age variable.',
                   value = NULL, min = 0),
      numericInput('sex.col',
                   'Column in sample data csv containing sex variable.',
                   value = NULL, min = 0),
      numericInput('cas.col',
                   'Column in sample data csv containing reproductive status variable.',
                   value = NULL, min = 0),
      
      tags$hr(),
      checkboxInput('examples', 'Instead of choosing your own csv files, use example files from capm package. After checking this box, press the "Get map" above.', F)
      
    ),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 6',
      helpText('Population pyramid.'))
    
    ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel(
        'Introduction', value = 1,
        HTML(
          '<p>
          bla, : <br>
          <ul>
          <li> bla. </li>
          <li> bla. </li>
          </ul>
          </p>
          <p>
          bla.
          </p>
          ')
      ),
      
      #tabPanel('Universe of sampling units', value = 2,
       #        tableOutput('universe')),
      
      tabPanel('Sample data', value = 3,
               HTML('bla'),
               tableOutput('sample')),
      
      tabPanel('Survey design', value = 4,
               tableOutput('pyramid')),
      
      tabPanel('Population pyramid', value = 5,
               plotOutput('pyramid', height = 600)),
      
      tabPanel("Estimates", value = 6,
               tableOutput('estimatess')),
      id = "conditionedPanels"
    )
  )
))