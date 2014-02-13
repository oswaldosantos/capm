shinyUI(pageWithSidebar(
  
  headerPanel("Calculate sample size and composition"),
  
  sidebarPanel(
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 1',
      HTML('<p>Choose a csv file having PSU unique identifiers in the first column and PSU sizes in the second column.</p>'),
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
                         '.csv')),
      
      HTML('<p>Choose a csv file having PSU unique identifiers in the first column and the totals observed in a pilot sample in the second column.</p>'),
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
      fileInput('psu.x', '',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      
      tags$hr(),
      checkboxInput('examples', 'Instead of choosing your own csv files, use the example files from capm package.', F)
    ),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 2',
      numericInput('level', 'Confidence level',
                   value = 0.95, min = 0, max = 1),
      numericInput('error', 'Accepted error',
                   value = 0.1, min = 0, max = 1),
      numericInput('cost', 'Cost',
                   value = 4, min = 0)
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        'Introduction', value = 1,
        HTML(
          '<p>In the context of two-stage cluster sampling, suppose that census tracks are PSU and the number of households in each census track represent PSU sizes. In the left side panel, you are asked to choose two csv files. The first file must have just two columns with that information. The second file must have one row per SSU sampled in a pilot study. The first column contains the PSU identifier to which the SSU belongs to. The second column contains the total observed in that SSU, for the variable of interest.</P>

<p>Make sure you choose the appropriate options (header, separator and quote), otherwise, you will get an error or an awkward result. You can also use example files from capm package cheking the box at the bottom of the the left side panel. In this case you do not need to choose any csv file.</p>

<p><b>Sample Size</b><br>
In the left side panel of the <i>Sample size</i> Tab, <i>Confidence level</i> and <i>Accepted error</i> must be a number between 0 and 1 inclusive. <i>Cost </i>is the ratio of the cost of sampling a PSU to the cost of sampling a SSU.</p>

          <p>
          <b>Further information</b><br>
          <ul>
          <li>The display of some outputs might take a few seconds, be patient!</li>
          <li>Reload the page to reset the fields</li>
          <li>Working from command line, you will have more options and flexibility.</li>
          <li>Tutorials with more detailed information can be found in <a href="https://github.com/oswaldosantos/capm">https://github.com/oswaldosantos/capm</a></li>
          <li>If you find errors, have suggestions or any question, I will be glad to know it <a href="mailto:oswaldosant@gmail.com">oswaldosant@gmail.com</a></li>
          </ul>
          </P>
          ')
      ),
      tabPanel('Sample size', value = 2,
               tableOutput('size')),
      id = "conditionedPanels"
    )
  )
))