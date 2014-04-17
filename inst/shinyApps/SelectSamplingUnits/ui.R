shinyUI(fluidPage(
  
  titlePanel("Selection of sampling units"),
  
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
                   ';'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),    
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
      numericInput('psu', 'Number of PSU to be selected:',
                   value = NULL, step = 10, min = 0),
      numericInput('ssu', 'Number of SSU to be selected:',
                   value = NULL, min = 0),
      
      tags$hr(),
      HTML('<b>Systematic sampling</b>'),
      numericInput('total', 'Total number of sampling units in the population:', value = NULL, step = 10, min = 0),
      numericInput('su', 'Total number of sampling units in the sample:',
                   value = NULL, min = 0)
      ),
      
    conditionalPanel(
      condition = 'input.conditionedPanels == 3',
      
      HTML('<b>Map of selected PSU (for the example files, ignore the next two fields).</b>'),
      br(),br(),
      textInput('shape.path', 'Path to the shapefile:'),
      textInput('shape.name', 'Name of the shapefile:'),
      numericInput('col', 'Column with PSU identifiers (in dbf file):',
                   value = NULL, min = 0),
      helpText('Press the buttom and wait'),
      actionButton('get.map', 'Get/Update map'),
      tags$hr(),
      
      HTML('<b>Save selected PSU as KML files.</b>'),
      br(),br(),
      textInput('write.to.path', 'Save KML files in this directory:'),
      actionButton('kml', 'Write KML files')
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

<b>Uploaded dataset</b><br>
<p>This Tab is intended to check that the CSV file was appropriately uploaded. If you see an awkward structure, try another specification for "Separator" and "Quote".</p>

<b>Selection of sampling units</b><br>
<p>In the <i>Selection of sampling units</i> Tab, there are two sections to specify the sample size and composition. Use the first for two-stage cluster sampling and the second for systematic sampling. To select sampling units in the context of systematic sampling, you do not need to upload any file.</p>

<b>Maps</b><br>
<p>The use of <i>Map</i> Tab is optional and is intended for mapping PSU of a two-stage cluster sampling design.</p>
<p>Using the first section of the left side panel, you can map the selected PSU in the browser. Indicate the path to the directory containing the shapefile with PSU (see below - <i>Specifying paths</i>). In the dbf file associated with the shapefile, there must be a column with the same PSU identifiers contained in the csv file uploaded in the <i>Introduction</i> Tab. Specify this column in the respective field.</p>
<p> The second section allows you to write a KML file of each selected PSU plus a KML file with all selected PSU. These files can be opened in Goole Earth or in a GIS software such as QGIS, to locate the areas that must be visited. Specify the directory to save the files in as described below.</p><br>

<i>Specifying paths</i>
<p>Specification of a path to a given directory is operating system dependent.</p>

<p>Windows<br>
Suppose the shapefile is in C:\\Users\\Oswaldo\\Documents and I want to save the KML files in this directory to. I must to write the following in the "Path to the shapefile" and "Save KML files in this directory" fields: C:/Users/Oswaldo/Documents</p>

<p>Mac<br>
Suppose the shapefile is in Users/Oswaldo/Desktop and I want to save the KML files in this directory to. I must to write the following in the "Path to the shapefile" and "Save KML files in this directory" fields: /Users/Oswaldo/Documents</p>

<p>Linux<br>
Suppose the shapefile is in home/Oswaldo/Documents and I want to save the KML files in this directory to. I must to write the following in the "Path to shapefile" and "Save KML files in this directory" fields: home/Oswaldo/Documents </p><br>


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
      tabPanel('Uploaded dataset', value = 1,
               dataTableOutput('dataset')),
      tabPanel('Selected sampling units', value = 2,
               tableOutput('selected'),
               downloadButton('downloadData','Download')),
      tabPanel('Maps', value = 3,
               plotOutput('map', height = 600)),
      id = "conditionedPanels"
    )
  )
))