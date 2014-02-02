shinyUI(pageWithSidebar(
  
  headerPanel("Selection of sampling units"),
  
  sidebarPanel(    
    helpText("For two-stage cluster sampling, Primary Sampling Units (PSU) are selected with probability proportional to size. Secondary Sampling Units (SSU) are selected through systematic sampling."),
    
    tags$hr(),
    
    numericInput('psu', 'Number of PSU to be selected',
                 value = NULL, step = 10, min = 0),
    numericInput('ssu', 'Number of SSU to be selected',
                 value = NULL, min = 0),
    
    tags$hr(),
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
                       '.csv')),
    
    tags$hr(),
    helpText('To map the selected PSU, fill in the following fields and press "Get map" to choose a shapefile file.'),
    textInput('shape.path', 'Path to the file with the PSU (without quotes).'),
    textInput('shape.name', 'Name of the file (without quotes).'),
    numericInput('col', 'Column with PSU identifiers.', value = NULL, min = 0),
    br(),
    actionButton('get.map', 'Get map'),
    
    tags$hr(),
    checkboxInput('examples', 'Instead of choosing your own csv and shapefile, use example files from capm package. After checking this box, press the "Get map" above.', F),
    
    tags$hr(),
    numericInput('total', 'If a systematic sample instead of a two-stage sample is required, specify here the total number of sampling units in the population. Specify the number of sampling units to be selected into the "Number of SSU to be selected" field.\nYou do not need to choose any csv file.',
                 value = NULL, step = 10, min = 0)
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        'Comments',
        HTML(
          '<p>
          Here, you can select sampling units and there are two cases you might be interested in: <br>
<ul>
<li> Selection of sampling units to design a pilot sample. </li>
<li> Selection of sampling units to design a (final) sample. </li>
</ul>
          </p>
          <p>
How many sampling units you must to select in each case? We are preparing a peer-reviewed paper to address this issue. See also the link at the bottom.
</p>
<p>
In the context of two-stage cluster sampling, suppose that census tracks are PSU and the number of households in each census track represent PSU sizes. In the left side panel, you are asked to chosse a csv file. This file must have just two columns with that information. Make sure you choose the appropriate options (header, separator and quote), otherwise, you will get an error or an awkward result.
</P>
<p>
To select sampling units in the context of systematic sampling, you do not need to upload any file. Just specify the number of sampling unitis in the "Number of SSU to be selected" field at the left side panel (although technically, you will not select SSU as the name of the field suggest).
</p>
<p>
In the middle tab "Selected sampling units", you will find a table showing the SSU (rows) to be selected in each PSU (columns). Each time you run the program, a different result (almost allways) will be displayed - it is random selection!. For systematic sampling, a unique column with the sampling units will be displayed.
</p>
<p>
If you upload a shapefile (for two-stage cluster sampling), specify the column of the dbf file containing the PSU. All the PSU unique identifiers in the csv file must be contained in that column too.
</p>
<p>
Using the command line it is possible to save the results. Selected sampling units can be saved in a csv file, which can be opened in a spreadsheet software like Calc or Excel. Maps can be saved in kml files, which can be opened in Google Earth.
</p>
<p>
Further information can be found in <a href="https://github.com/oswaldosantos/capm">https://github.com/oswaldosantos/capm</a>.
</P>
          ')
      ),
      tabPanel('Selected sampling units',
               tableOutput('selected')),
      tabPanel('Maps',
               HTML('After pressing the "Get map" button in the left side panel, the map will be plotted below after some seconds.'),
               plotOutput('map', height = 600))
    )
  )
))