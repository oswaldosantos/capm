#library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Selection of sampling units"),
  
  sidebarPanel(    
    helpText("For two-stage cluster sampling, Primary Sampling Units (PSU) are selected with probability proportional to size. Secondary Sampling Units (SSU) are selected through systematic sampling."),
    
    tags$hr(),
    
    numericInput('psu', 'Number of PSU to be selected',
                 value = 0, step = 10),
    numericInput('ssu', 'Number of SSU to be selected',
                 value = 0),
    
    tags$hr(),
    
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
    
    fileInput('file', 'Choose a CSV file having PSU unique identifiers in the first column and PSU sizes in the second column.',
              accept=c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')),
    tags$hr(),
    
    numericInput('total', 'If a systematic sample instead of a two-stage sample is required, specify here the total number of sampling units in the population. Specify the number of sampling units to be selected into the "Number of SSU to be selected" field.\nYou do not need to choose any CSV file.',
                 value = 0, step = 10),
    
    tags$hr(),
    checkboxInput('maps', 'Generate maps (only for two-stage samples)', F),
    checkboxInput('csv', 'Generate CSV files', F)
    
    #submitButton("Submit")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Sampling units selected",
               tableOutput("su")),
      tabPanel("Maps", plotOutput("table")),
      tabPanel(
        "Additional explanations",
        HTML("
             S'(t) = -&beta;SI<br>
             I'(t) = &beta;SI - &gamma;I<br>
             R'(t) = &gamma;I
             <br><br>
             <table border='l'>
             <tr>
             <td> &nbsp; S &nbsp; </td>
             <td> &nbsp; Susceptibles &nbsp; </td>
             </tr>
             <tr>
             <td> &nbsp; I &nbsp; </td>
             <td> &nbsp; Infectious &nbsp; </td>
             </tr>
             <tr>
             <td> &nbsp; R &nbsp; </td>
             <td> &nbsp; Recovered &nbsp; </td>
             </tr>
             <tr>
             <td> &nbsp; &beta; &nbsp; </td>
             <td> &nbsp; Transmission rate &nbsp; </td>
             </tr>
             <tr>
             <td> &nbsp; &gamma; &nbsp; </td>
             <td> &nbsp; Recovery rate (reciprocal of average infectious period) &nbsp; </td>
             </tr>
             </table>
             "))
    )
  )
))