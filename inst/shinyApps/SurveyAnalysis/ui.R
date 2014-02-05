shinyUI(pageWithSidebar(
  
  headerPanel("Survey analysis"),
  
  sidebarPanel(
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 1',
      checkboxInput('examples', 'Use example files from capm package instead of your own data.', F)),
    
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
      fileInput('universe', '',
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
      helpText('Population pyramid.'),
      numericInput('age.col',
                   'Column in sample data containing age variable.',
                   value = NULL, min = 0),
      numericInput('sex.col',
                   'Column in sample data containing sex variable.',
                   value = NULL, min = 0),
      numericInput('cas.col',
                   'Column in sample data containing reproductive status variable.',
                   value = NULL, min = 0)
    ),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 5',
      numericInput('psu.col',
                   'Column in sample data that contain PSU identifiers.',
                   value = NULL, min = 0),
      numericInput('ssu.col',
                   'Column in sample data that contain SSU identifiers.',
                   value = NULL, min = 0),
      numericInput('psu.2cdl',
                   'Number  of PSU included in the design.',
                   value = NULL, min = 0),
      tags$hr(),
      HTML('<p> Type of estimates <br> If you are using example files, copy and paste the following terms: <br> total, prop, mean, prop, prop, prop, total, prop, prop, prop'),
      textInput('variables', '',
                 value = NULL),
      tags$hr(),
      numericInput('total',
                   'For systematic sampling, ingore the first three fields and specify here the total number of sampling units in the population.',
                   value = NULL, min = 0),
      tags$hr(),
      actionButton('calc.estimates', 'Get estimates'))
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel(
        'Introduction', value = 1,
        HTML(
          '<p>
          With this App, you can upload data collected in a survey, make some descriptive analysis, and estimate population parameters. To get the estimates, it is necessary to specify the survey design used to collect the data.<br><br>

<b>Runing example files</b><br>
To run the App with example files from capm package instead of your own data, check the box in the left side panel.<br><br>

<b>Universe of sampling units</b><br>
If you checked the box to use example files, ignore the left side panel of the <i>Universe of sampling units</i> Tab. If you are using your own files, make sure you choose the apropriate options in the left side panel to avoid error messages or awkward results. You must be able to see a table with two columns and as many rows as sampling units in the population.<br><br>

<b>Survey data</b><br>
If you checked the box to use example files, ignore the left side panel of the <i>Survey data</i> Tab.  If you are using your own files, make sure you choose the apropriate options in the left side panel to avoid error messages or awkward results. The csv file must have one column for each variable for which estimates are needed, one column for PSU identifiers and one column for SSU identifiers. You must be able to see that columns and as many rows as SSU (across all PSU) in the sample. On the top of the table, the gray box shows summary statistics for each column. In the example file, the first column contain SSU identifiers, the second contain PSU identifiers and the remaining contain the variables.<br>
To see a description of the meaning of each variable, go to RStudio and run <code>help(Sample)</code>.<br><br>

<b>Population pyramid</b><br>
If you checked the box to use example files, fill in the fields from top to bottom with, 5, 4 and 6 respectively (see this column positions in the output from <i>Survey data</i> Tab).<br><br>

<b>Estimates</b><br>
If you checked the box to use example files, fill in the first three fields from top to bottom with, 2, 1 and 20 respectively (see the first two columns in the output from <i>Survey data</i> Tab). The example survey design included 20 PSU and that is the reason for the last number.<br>
For systematic sampling, ignore the first three fields, and specify the total number of sampling units in the population (fourth field).<br>
In the last field, specify the type of estimate for each variable. To estimate a total, type <i>total</i>, to estimate a mean, type <i>mean</i> and to estimate a proportion, type <i>prop</i>. To get estimates of more than one variable, type the appropriate term for each variable and separate them by commas. The order of term from left to right must be the order (left to right) of variables (columns) in the survey file.<br>
From the command line in RStudio, it is possible to get estimates for specific subpopulations (i.e. by sex). To estimate the total for categorical variables such as "castrated" from the example file (<i>Survey data</i> Tab), the command line is the place to go.<br><br>

<b>Further information</b><br>
<ul>
<li>The display of some outputs might take a few seconds, be patient!</li>
<li>Reload the page to reset the fields</li>
<li>Working from command line, you will have more options and flexibility.</li>
<li>Torials with more detailed information can be found in <a href="https://github.com/oswaldosantos/capm">https://github.com/oswaldosantos/capm</a></li>
<li>If you find errors, have suggestions or any question, I will be glad to know <a href="mailto:oswaldosant@gmail.com">oswaldosant@gmail.com</a></li>
</ul>
          </p>
          ')),
      
      tabPanel('Universe of sampling units', value = 2,
               dataTableOutput('universe')),
      
      tabPanel('Survey data', value = 3,
               verbatimTextOutput('summ.sample'),
               dataTableOutput('sample')),
      
      tabPanel('Population pyramid', value = 4,
               plotOutput('pyramid', height = 600)),
      
      tabPanel('Estimates', value = 5,
               tableOutput('variables'),
               tags$hr(),
               tableOutput('estimates')),
      
      id = "conditionedPanels"
    )
  )
))