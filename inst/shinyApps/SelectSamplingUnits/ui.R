shinyUI(pageWithSidebar(
  
  headerPanel("Selection of sampling units"),
  
  sidebarPanel(
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 1',
      
      HTML('<p><b>Two-stage cluster sampling</b><br>
           Choose a csv file having PSU unique identifiers in the first column and PSU sizes in the second column.</p>'),
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
      
      tags$hr(),
      checkboxInput('examples', 'Instead of choosing your own csv, use the example file from capm package.', F)
    ),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 2',
      
      HTML('<p><b>Two-stage cluster sampling</b></p>'),
      numericInput('psu', 'Number of PSU to be selected',
                   value = NULL, step = 10, min = 0),
      numericInput('ssu', 'Number of SSU to be selected',
                   value = NULL, min = 0),      
      
      tags$hr(),
      HTML('Map of selected PSU (for the example files, ignore the next two fields).'),
      br(),br(),
      textInput('shape.path', 'Path to the shapefile.'),
      textInput('shape.name', 'Name of the shapefile.'),
      numericInput('col', 'Column with PSU identifiers (in dbf file).', value = NULL, min = 0),
      br(),
      helpText('Press the buttom and wait'),
      actionButton('get.map', 'Get/Update map'),
      
      tags$hr(),
      helpText(strong('Systematic sampling')),
      numericInput('total', 'Total number of sampling units in the population.', value = NULL, step = 10, min = 0),
      numericInput('su', 'Total number of sampling units in the sample',
                   value = NULL, min = 0)
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        'Introduction', value = 1,
        HTML(
          '<p>Here, you can select sampling units and there are two cases you might be interested in: <br>
<ul>
<li> Selection of sampling units to design a pilot sample. </li>
<li> Selection of sampling units to design a (final) sample. </li>
</ul></p>

<p>How many sampling units you must to select in each case? We are preparing a peer-reviewed paper to address this issue. See also the link at the bottom.</p>

<p>In the context of two-stage cluster sampling, suppose that census tracks are PSU and the number of households in each census track represent PSU sizes. In the left side panel, you are asked to choose a csv file. This file must have just two columns with that information. Make sure you choose the appropriate options (header, separator and quote), otherwise, you will get an error or an awkward result. You can also use example files from capm package, checking the box at the bottom of the the left side panel. In this case you do not need to choose any csv file.</P>

<b>Selection of sampling units and Maps</b><br>
<p>In the the <i>Selection of sampling units</i> and <i>Maps</i> Tabs, there are three sections in the left side panel. The first two are for two-stage cluster sampling. The mid section is optional. To use it, indicate the path to the directory containing the shapefile with PSU. In the dbf file associated with the shapefile, there must be a column with the same PSU identifiers contained in the csv file uploaded in the <i>Introduction</i> Tab.</p>

<p>Using the command line it is possible to save the results. Selected sampling units can be saved in a csv file, which can be opened in a spreadsheet software like Calc or Excel. Maps can be saved in kml files, which can be opened in Google Earth (one map with all PSU and one map for each PSU).</p>

<p>To select sampling units in the context of systematic sampling, you do not need to upload any file.</p>

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
      tabPanel('Selected sampling units', value = 2,
               tableOutput('selected')),
      tabPanel('Maps', value = 2,
               plotOutput('map', height = 600)),
      id = "conditionedPanels"
    )
  )
))