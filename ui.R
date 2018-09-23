library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demonstration of Link Prediction"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(fluid = T,
        sidebarPanel(
          fileInput('file1', 'Choose CSV File',
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
          checkboxInput('header', 'Header', FALSE),
          radioButtons('sep', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab=' '),
                       'Comma'),
          radioButtons('quote', 'Quote',
                       c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       'Double Quote'),
          numericInput("th","Enter threshold",value = 0.75),
          tags$hr(),
          
          sliderInput("vsize",label = "Select vertex size",min = 1,max = 10,value = 10),
          tags$hr(),
          sliderInput("esize",label = "Select edge size",min = 1,max = 10,value = 1),
          tags$hr(),
          actionButton('action', 'Take a closer look')
          
        ),
    
    # Show a plot of the generated distribution
    mainPanel(
              tabsetPanel(
                tabPanel(h5("Table"), textOutput('myText1'), textOutput('myText2'), tableOutput('contents')), 
                tabPanel(h5("Raw Plot"), plotOutput("dcompplot",width = "500px",height = "500px")), 
                tabPanel(h5("Predicted Plot"), plotOutput("outplot"), plotOutput('myPlot2'))
              )
              
    )
  )
))

