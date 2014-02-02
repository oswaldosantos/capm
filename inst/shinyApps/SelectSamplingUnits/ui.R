#library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Selection of sampling units"),
  
  sidebarPanel(    
    helpText("For two-stage cluster sampling, Primary Sampling Units (PSU) are selected with probability proportional to size. Secondary Sampling Units (SSU) are selected through systematic sampling."),
    
    tags$hr(),
    
    numericInput('psu', 'Number of PSU to be selected',
                 value = 10, step = 10, min = 0),
    numericInput('ssu', 'Number of SSU to be selected',
                 value = 10, min = 0),
    
    tags$hr(),
    helpText('Choose a *.csv file having PSU unique identifiers in the first column and PSU sizes in the second column.'),
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
    fileInput('shape', 'To map the selected PSU, choose a *.shp file.'),
    
    tags$hr(),
    checkboxInput('examples', 'Instead of choosing the previous two files, use example files from capm package.', T),
    
    tags$hr(),
    numericInput('total', 'If a systematic sample instead of a two-stage sample is required, specify here the total number of sampling units in the population. Specify the number of sampling units to be selected into the "Number of SSU to be selected" field.\nYou do not need to choose any *.csv file.',
                 value = 0, step = 10, min = 0),
    
    tags$hr(),
    actionButton('csv', 'Generate csv'),
    actionButton('maps', 'Generate maps')
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        'Comments',
        HTML(
          '<p>
          Here, you can select sampling units and there is two cases you might be interested in: <br>
<ul>
<li> Selection of sampling units to design a pilot sample. </li>
<li> Selection of sampling units to design a (final) sample. </li>
</ul>
          </p>
          <p>
How many sampling units you must to select in each case? We are preparing a peer-reviewed paper to address this issue. See also the link at the bottom.
</p>
<p>
In the context of two-stage cluster sampling, suppose that census tracks are PSU and the number of households in each census tracks represent PSU sizes. Your *.csv file must have just two columns with this information. Make sure you choose the appropriate options (header, separator and quote), otherwise, you will get an error or an awkward result.
</P>
<p>
To select sampling units in the context of systematic sampling, you do not need to upload any file. Just specify the number of sampling unitis in the "Number of SSU to be selected" entry at the left side panel. Obviously, you will select "simple" sample units, not SSU.
</p>
<p>
In the middle tab "Selected sampling units", you will find a table showing the SSU (rows) to be selected in each PSU (columns). Each time you run the program, a different result (almost allways) will be displayed - it is random selection! To save a particular output, press the "Generate csv" button at the bottom of the left side panel. You will find a file named "ssu.csv" with the results in your current working directory. 
</p>
<p>
If you upload a shapefile (for two-stage cluster sampling), specify the column of the *.dbf file containing the PSU. All the PSU unique identifiers in the *.csv file must be in that column too. When you press the "Generate maps" button at the bottom of the left side panel, selected PSU will be displayed in right tab "Map". In addition, after pressing this button a *.kml file with all selected PSU and a *.kml file for each selected PSU will be generated and will be available in your current working directory. This files can be opened in Google Earth.
</p>
<p>
Further information can be found in <a href="https://github.com/oswaldosantos/capm">https://github.com/oswaldosantos/capm</a>.
</P>
          ')
      ),
      tabPanel('Selected sampling units',
               tableOutput('selected')),
      tabPanel('Maps', plotOutput('map', height = 600))
    )
  )
))