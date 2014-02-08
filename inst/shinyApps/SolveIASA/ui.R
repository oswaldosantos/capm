#library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel('IASA model: Immigration, Abandonment, Sterilization and Adoption'),
  
  sidebarPanel(
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 1', value = 1,
      HTML('<p><b><Initial conditions</b></p>'), 
      HTML('<p>Owned population</p>'),
      numericInput('f1', 'Intact females (f1)',
                   value = 33425, min = 0),
      numericInput('fs1', 'Sterilized females (fs1)',
                   value = 10865, min = 0),
      numericInput('m1', 'Intact males (m1)',
                   value = 38039, min = 0),
      numericInput('ms1', 'Sterilized males (ms1)',
                   value = 6008, min = 0),
      
      br(),br(),
      HTML('<p>Stray population</p>'),
      numericInput('f2', 'Intact females (f2)',
                   value = 3343, min = 0),
      numericInput('fs2', 'Sterilized females (fs2)',
                   value = 109, min = 0),
      numericInput('m2', 'Intact males (m2)',
                   value = 3804, min = 0),
      numericInput('ms2', 'Sterilized males (ms2)',
                   value = 68, min = 0),
      
      tags$hr(),
      HTML('<p><b>Parameters</b></p>'),      
      HTML('<p>Owned population</p>'),
      numericInput('b1', 'Births per year (b1)',
                   value = 21871, min = 0),
      numericInput('df1', 'Female death rate (df1)',
                   value = 0.104, min = 0, step = 0.01),
      numericInput('dm1', 'Male death rate (dm1)',
                   value = 0.098, min = 0, step = 0.01),
      numericInput('sf1', 'Female sterilization rate (sf1)',
                   value = 0.069, min = 0, step = 0.01),
      numericInput('sm1', 'Male sterilization rate (sm1)',
                   value = 0.028, min = 0, step = 0.01),
      numericInput('k1', 'Carrying capacity (k1)',
                   value = 98050, min = 0),
      numericInput('h1', 'Mean harem size (h1)',
                   value = 1, min = 0, step = 0.1),
      numericInput('ab', 'Abandonment rate (ab)',
                   value = 0.054, min = 0, step = 0.01),
      numericInput('v', 'Immigration rate (v)',
                   value = 0.2, min = 0, step = 0.01),
      numericInput('z', 'Proportion of sterilized immigrants (z)',
                   value = 0.1, min = 0, step = 0.01),
      
      br(),br(),
      HTML('<p>Stray population</p>'),
      numericInput('b2', 'Births per year (b2)',
                   value = 4374, min = 0),
      numericInput('df2', 'Female death rate (df2)',
                   value = 0.125, min = 0, step = 0.01),
      numericInput('dm2', 'Male death rate (dm2)',
                   value = 0.118, min = 0, step = 0.01),
      numericInput('sf2', 'Female sterilization rate (sf2)',
                   value = 0.05, min = 0, step = 0.01),
      numericInput('sm2', 'Male sterilization rate (sm2)',
                   value = 0.03, min = 0, step = 0.01),
      numericInput('k2', 'Carrying capacity (k2)',
                   value = 8055, min = 0),
      numericInput('h2', 'Mean harem size (h2)',
                   value = 0.5, min = 0, step = 0.1),
      numericInput('ad', 'Adoption rate (ad)',
                   value = 0.1, min = 0, step = 0.01),      
      
      tags$hr(),    
      numericInput('time', strong('Simulation time'), value = 15),
      numericInput('t_steps', 'Time steps', value = 1),
      
      tags$hr(),
      selectInput('output_var', strong('Choose a population'), 
                  choices = list(
                    'Owned intact animals (n1)',
                    'Owned sterilized animals (ns1)',
                    'Stray intact animals (n2)',
                    'Stray sterilized animals (ns2)',
                    'Owned animals (N1)',
                    'Stray animals (N2)',
                    'Total population (N)',
                    'Owned intact females (f1)',
                    'Owned sterilized females (fs1)',
                    'Owned intact males (m1)',
                    'Owned sterilized males (ms1)',
                    'Stray intact females (f2)',
                    'Stray sterilized females (fs2)',
                    'Stray intact males (m2)',
                    'Stray sterilized males (ms2)'),
                  'Owned sterilized animals (ns1)'
                  )
    ),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 2', value = 2,
      HTML('<p><b>Scenarios</b></p>'),
      
      tags$hr(),
      sliderInput('s.range',
                   'Sterilization range',
                   min = 0, max = 0.8, value = c(0, 0.2), step = 0.01),
      numericInput('s.intr', 'Number of intervals', value = 10),
      checkboxInput('s.fm', 'Sterilization of only females'),
      
      tags$hr(),
      sliderInput('ab.range',
                  'Abandonment range',
                  min = 0, max = 0.8, value = c(0, 0.2), step = 0.01),
      
      tags$hr(),
      sliderInput('ad.range',
                  'Adoption range',
                  min = 0, max = 0.8, value = c(0, 0.2), step = 0.01),
      
      tags$hr(),
      sliderInput('im.1',
                  'Immigration 1',
                  min = 0, max = 0.8, value = 0, step = 0.01),
      sliderInput('im.2',
                  'Immigration 2',
                  min = 0, max = 0.8, value = 0.2, step = 0.01),
      tags$hr(),
      selectInput('output_var2', strong('Choose a population'), 
                  choices = list(
                    'Intact females (f)',
                    'Sterilized females (fs)',
                    'Intact males (m)',
                    'Sterilized males (ms)',
                    'Intact animals (n)',
                    'Sterilized animals (ns)',
                    'Total population (N)'),
                  'Sterilized animals (ns)'
                  ),
      
      tags$hr(),
      helpText('Press the button and wait until full color plots appear'),
      actionButton('create.scenarios', 'Create scenarios')
    ),
    
    conditionalPanel(
      condition = 'input.conditionedPanels == 3', value = 3,
      HTML('<p>Sensitivities</p>'),
      numericInput('range',
                   'Proportional perturbation of parameters in global sensitivity',
                   value = 0.1, step = 0.01),
      helpText('Press the button and wait until full color plots appear'),
      actionButton('sensitivities', 'Calculate sensitivities')
    )
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Point estimates - plot', value = 1,
               plotOutput('points_p', height = 600)),
      tabPanel('Point estimates - table', value = 1,
               dataTableOutput('points_t')),
      tabPanel('Scenarios', value = 2,
               plotOutput('scenarios', height = 600)),
      tabPanel('Global sensitivities', value = 3,
               plotOutput('globalall', height = 600),
               plotOutput('global', height = 600)),
      tabPanel('Local sensitivities',
               plotOutput('local', height = 1200)),
      id = 'conditionedPanels'
    )
  )
))