#### Libraries ####
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(ggradar)


ui <- fluidPage(
  
  # name of the whole project - stays at top next to page tabs
  navbarPage(strong("RNA Analysis"),
             
             # this sets the entire 'theme'/style
             theme = shinythemes::shinytheme("flatly"),
             
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
                      # this should be something more elegant..
                      uiOutput("metaFactors"),
                      
                      # look at head(data), currently for TESTING purposes
                      tableOutput("dataMatPeek")
             ),
             
             
             
             ################ UI Tab 1: Single Gene Analysis ################
             tabPanel("Single Gene Analysis",  # part of navbarPage
                      
                      sidebarLayout(
                        # sidebarPanel should have all the input needed from the user
                        sidebarPanel(
                          tabPanel("Gene Expression"),
                          
                          # drop-down list to type in gene of interest to plot
                          # gene input, change 'choices' to a vector of all rownames of the data matrix
                          selectInput("userGene", "Choose a gene to plot:", choices = c(1,2,3)),
                          
                          # input factors
                          #
                          
                          # input Graph type: Boxplot/Violin
                          radioButtons("graphType", "Graph Type",
                                       c("Boxplot" = "boxplot", "Violin" = "violin")),
                          
                          # input scale: linear/log
                          radioButtons("scaleType", "Graph Scale",
                                       c("Linear" = "linear", "Log" = "log")
                                       
                          )
                        ),
                        
                        # mainbarPanel should have the plots
                        mainPanel(
                          tabsetPanel(
                            tabPanel(paste0("Across OBTAIN FACTOR 1 FROM DATA/USER")
                                     # the ggplot should be here
                            ),
                            tabPanel(paste0("Across OBTAIN FACTOR 2 FROM DATA/USER")
                                     # the ggplot across factor 2 should be here
                            ),
                            tabPanel("Raw Data Plotted",
                                     plotOutput("singlegene_plot")
                            )
                          )
                          
                        )
                      )
             ),
             
             
             ################ UI Tab 2: Multi-Gene Analysis ################
             tabPanel("Multi-Gene Analysis",  # part of navbarPage
                      
                      sidebarLayout(
                        # sidebarPanel should have all the input needed from the user
                        sidebarPanel(
                          tabPanel("Gene Expression"),
                          
                          # drop-down list to type in gene of interest to plot
                          # gene input, change 'choices' to a vector of all rownames of the data matrix
                          selectInput("userGene", "Choose gene(s) to plot:", choices = c(1,2,3)),
                          
                          # input factors
                          
                          
                          # input Graph type: Boxplot/Violin/Scatterplot/RadarCharts
                          radioButtons("graphType", "Graph Type",
                                       c("Scatterplot" = "scatterplot",
                                         "Radar Chart" = "radar chart")),
                          
                          # input scale: linear/log
                          radioButtons("scaleType", "Graph Scale",
                                       c("Linear" = "linear", "Log" = "log"))
                          
                        ), 
                        
                        # mainbarPanel should have the plots
                        mainPanel(
                          tabsetPanel(
                            tabPanel(paste0("Across OBTAIN FACTOR 1 FROM DATA/USER")
                                     # the ggplot should be here
                            ),
                            tabPanel(paste0("Across OBTAIN FACTOR 2 FROM DATA/USER")
                                     # the ggplot across factor 2 should be here
                            ),
                            tabPanel("Raw Data Plotted"),
                            plotOutput("multigene_plot")
                          )
                        )
                        
                      )
             ),
             
             
             
             ################ UI Tab 3: Gene Trajectories ################
             tabPanel("Trajectories",
                      
                      sidebarLayout(
                        sidebarPanel(
                          # user input list of genes to plot
                          textAreaInput("trajGenes",
                                        "Enter a list of genes seperated by new lines, commas, or spaces:",
                                        value = "Fndc5, Pgc1a\nBdnf, Itgb5"),
                          
                          # add a description under the input area
                          h4("Paste genes of interest")
                        ),
                        
                        # should contain graphs and DT table
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Trajectories Plot",
                                     plotOutput("trajPlot")
                            ),
                            
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

server <- function(input, output, session){
  
  # change the default limit of 5MB user uploads to 200MB (sample data is 129MB)
  options(shiny.maxRequestSize=200*1024^2)
  
  ################# Input Tab #################
  
  ##### data matrix reader #####
  # reactive functions should be used when an operation is done more than once (e.g reading an input)
  dataMatReader <- reactive({
    # await user input in the relevant fileInput
    dataFile <- input$inputData
    
    # suppresses error, basically waits for input before it continues render function
    req(dataFile)
    
    # read the uniquely produced datapath, read the file
    # consider looking at the extension 
    read.csv(dataFile$datapath)
  })
  
  ##### metadata #####
  # reactive functions should be used when an operation is done more than once (e.g reading an input)
  metadataReader <- reactive({
    # await user input in the relevant fileInput
    metadataFile <- input$inputMetadata
    
    # suppresses error, basically waits for input before it continues render function
    req(metadataFile)
    
    # read the uniquely produced datapath, read the file
    read.csv(metadataFile$datapath)
  })
  
  
  # this provides the user factors to chose
  # 'renderUI' dynamically changes by user input (i.e metadata input)
  output$metaFactors <- renderUI({
    
    # extract the colnames by calling the metadataReader reactive function
    factorNames <- colnames(metadataReader())
    
    # to make an input that changes conditionally, it must be added here
    checkboxGroupInput("factorsChosen", "Choose all factors you wish to analyze:",
                       choices = factorNames)
  })
  
  
  # look at input data
  output$dataMatPeek <- renderTable({
    head(dataMatReader())
  })
  
  ################# Single-Gene Analysis #################
  
  data <- reactive({
    req(input$userGene, input$graphType)
    
  })  
  
  output$singlegene_plot <- renderPlot({ 
    g <- ggplot(data(), aes(y = factor, x = gene), fill = gene, colour = "blue")
    g <- geom_boxplot()
    
    
    
  })
  
  
  
  
  ################# Multi-Gene Analysis #################
  
  output$multigene_plot <- renderPlot({ 
    g <- ggradarggradar(values.radar = c(0, 0.5, 1),
                        axis.labels = paste0("rownames"),legend.title = "rownames",
                        legend.position = "bottom", background.circle.colour = "white")
    
    
    
  })
  
  
  
  ################# Trajectories #################
  ## render plots
  output$trajPlot <- renderPlot({
    # transform matrix to z score matrix
    
    ggplot()
    # # load the data matrix
    # plotData <- dataMatReader()
    # ggplot(plotData, aes(x = input$trajGenes, y = )) +
    #   geom_line()
  })
  
  
  ## render interactive DT table
}



##### this connects the two and runs the shiny app #####
shinyApp(ui = ui, server = server)
# 'options = list(display.mode = "showcase")' presents code being run in the server, nice debugging tool..




