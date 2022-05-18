#### Libraries ####
library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)


ui <- fluidPage(
  # this sets the entire 'theme'/style
  shinythemes::shinytheme("flatly"),
  
  # name of the whole project - stays at top next to page tabs
  navbarPage("RNA Analysis",
    
    # main tab: title of the whole page
    tabPanel("Main",  # part of navbarPage
             
      titlePanel("Input Data"),
      
      # accept normalized data:
      h5("Counts matrix: Rows = Gene names; Columns = Sample names"),
      fileInput("inputData", "Enter your count-normalized .csv or .xlsx file:", width = '35%'),
      
      # accept metadata
      h5("Metadata: Rows = Sample names; Columns = Related factors (e.g: sex, organ, time...)"),
      fileInput("inputMetadata", "Enter a metadata .csv or .xlsx file for the counts matrix:", width = '35%'),

      # present factors from metadata, and let the user choose factors from drop-down to check
      uiOutput("metaFactors")
    ),
    
    
    
    ################ UI Tab 1: Gene Expression ################
    tabPanel("Gene Changes",  # part of navbarPage
             
      sidebarLayout(
        # sidebarPanel should have all the input needed from the user
        sidebarPanel(
          tabPanel("Gene Expression"),
          
          # drop-down list to type in gene of interest to plot
          # gene input, change 'choices' to a vector of all rownames of the data matrix
          selectInput("userGene", "Choose a gene to plot:", choices = c(1,2,3)),
          
          # input factors
          
          # input Graph type: Boxplot/Violin
          radioButtons("graphType", "Graph Type",
                       c("Boxplot" = "boxplot", "Violin" = "violin")),
          # input scale: linear/log
          radioButtons("scaleType", "Graph Scale",
                       c("Linear" = "linear", "Log" = "log"))
          
        ),  # end of 'sidebarPanel()'
        
        # mainbarPanel should have the plots
        mainPanel(
          tabsetPanel(
            tabPanel(paste0("Across OBTAIN FACTOR 1 FROM DATA/USER")
                     # the ggplot should be here
            ),
            tabPanel(paste0("Across OBTAIN FACTOR 2 FROM DATA/USER")
                     # the ggplot across factor 2 should be here
            ),
            tabPanel("Raw Data Plotted")
          )
        )
        
      )
    ),
    
    
    
    
    ################ UI Tab 2: Gene Trajectories ################
    tabPanel("Trajectories",
      
      sidebarLayout(
        sidebarPanel(
          # user input list of genes to plot
          textAreaInput("Area", "Enter a list of genes seperated by new lines, commas, or spaces:",
                        value = "Fndc5, Pgc1a\nBdnf, Itgb5"),
          # add a description under the input area
          h4("Paste genes of interest")
        ),
        
        # should contain graphs and DT table
        mainPanel(
          tabsetPanel(
            tabPanel("Trajectories Plot"),
            
            tabPanel("Query Info")
          )
          
        )
      )
      
    )
  )
)

##################################################################################################################
##################################################### SERVER #####################################################
##################################################################################################################

server <- function(input, output, session) {
  # change the default limit of 5MB user uploads to 200MB (sample data is 129MB)
  options(shiny.maxRequestSize=200*1024^2)
  
  ################# Input Tab #################
  
  # this provides the user factors to chose
  # 'renderUI' dynamically changes by user input (i.e metadata input)
  output$metaFactors <- renderUI({
    # await user input in the relevant fileInput
    metadataFile <- input$inputMetadata
    
    # suppresses error, basically waits for input before it continues render function
    req(metadataFile)
    
    # read the uniquely produced datapath, read the file, extract the colnames
    factorNames <- colnames(read.csv(metadataFile$datapath))
    
    # to make an input that changes conditionally, it must be added here
    checkboxGroupInput("factorsChosen", "Choose all factors you wish to analyze:",
                       choices = factorNames)
  })
  

  ################# Server Tab 1 #################
  
  
  
  
  
  ################# Server Tab 2 #################
  ## render plots
  output$trajPlot <- renderPlot({
    ggplot()
  })
  

  ## render interactive DT table
}


##### this connects the two and runs the shiny app #####
shinyApp(ui = ui, server = server)
# 'options = list(display.mode = "showcase")' presents code being run in the server, nice debugging tool..




