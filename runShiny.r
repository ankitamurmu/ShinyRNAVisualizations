#### Libraries ####
library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(ggradar)
library(readr)
library(stringr)


## function to convert data to ggplot style ('longer')
filterAndConvert <- function(dataLoader, metadataLoader, plotGenes, conditions){
  ### Produces a matrix with columns of "Gene, Sample, Expression, Conditions1, Condition2, ..." ###
  ### This is a convenient form for plotting with ggplot. ###
  # dataLoader: reactive that loads expression matrix.
  # metadataLoader: reactive that loads metadata matrix.
  # plotGenes: user-inputted genes to plot.
  # conditions: user-inputted conditions of interest.
  
  metadata <- metadataLoader
  metadataSampleColumn <- colnames(metadata)[1]
  # keep only the relevant columns in metadata: sample names and user-chosen conditions/factors
  metadata <- metadata %>%
    dplyr::select(all_of(metadataSampleColumn),
                  all_of(conditions))
  
  convertedData <- dataLoader %>%
    # filter for the user-chosen genes in the plotting tabs
    #TODO: 'gene' here is problematic, it should be a general name/code
    filter(gene %in% plotGenes) %>%
    # convert to long matrix
    tidyr::pivot_longer(cols = !gene,
                        names_to = "Samples",
                        values_to = "Expression") %>%
    # join with metadata to retrieve all metadata information for each row
    inner_join(metadata, by = c("Samples" = metadataSampleColumn))
}


## extract the number of facet rows in a ggplot, for setting the height of mainPanel with plots
gg_facet_nrow <- function(p){
  num_panels <- length(unique(ggplot_build(p)$data[[1]]$PANEL)) # get number of panels
  num_rows <- wrap_dims(num_panels)[1] # get number of rows
}


## used to change themes for plots
themeChanger <- function(inputTheme){
  switch(inputTheme,
         "min" = theme_minimal(),
         "gray" = NULL,
         "dark" = theme_dark(),
         "classic" = theme_classic())
}


ui <- fluidPage(
  titlePanel("Shiny RNA Visualizations"),
  # name of the whole project - stays at top next to page tabs
  navbarPage(strong("Analyses Tabs:"),
             
             # this sets the entire 'theme'/style
             theme = shinythemes::shinytheme("flatly"),
             
             # main tab: title of the whole page
             tabPanel("Input Data",  # part of navbarPage
                      
                      titlePanel("Input Data"),
                      
                      # accept normalized data:
                      h5("Counts matrix: Rows = Gene names; Columns = Sample names"),
                      fileInput("inputData", "Enter your count-normalized .csv or .xlsx file:", width = '35%'),
                      
                      # accept metadata
                      h5("Metadata: Rows = Sample names; Columns = Related factors (e.g: sex, organ, time...)"),
                      fileInput("inputMetadata", "Enter a metadata .csv or .xlsx file for the counts matrix:", width = '35%'),
                      
                      # present factors from metadata, and let the user choose factors from drop-down to check
                      selectizeInput("chosenFactors",
                                     "Choose all factors you wish to analyze:",
                                     choices = NULL,
                                     multiple = TRUE),
                      
                      # look at head(data), currently for TESTING purposes
                      DT::dataTableOutput("filteredConverted"),
                      tableOutput("dataMatPeek"),
                      tableOutput("metadataMatPeek")
             ),
             
             
             
             ################################# UI Tab 1: Single Gene Analysis #################################
             tabPanel("Single Gene Analysis",  # part of navbarPage
                      #TODO: maybe we can add a section that specifies if the chosen gene is significantly different between conditions
                      
                      sidebarLayout(
                        # sidebarPanel should have all the input needed from the user
                        sidebarPanel(
                          tabPanel("Gene Expression"),
                          
                          # drop-down list to type in gene of interest to plot
                          # gene input, change 'choices' to a vector of all rownames of the data matrix
                          selectizeInput("userGeneSingle", "Choose a gene to plot:",
                                         choices = NULL, selected = "Fndc5"),
                          
                          # input factors
                          selectizeInput("plotFactorsSingle",
                                         "Select 2 factors to plot by (based on input in 'Input Data' tab):",
                                         choices = NULL, multiple = TRUE),
                          
                          # input Graph type: Boxplot/Violin
                          radioButtons("graphTypeSingle", "Graph Type",
                                       c("Boxplot" = "boxplot", "Violin" = "violin")),
                          
                          # input scale: linear/log
                          radioButtons("scaleTypeSingle", "Graph Scale",
                                       c("Linear" = "linear", "Log" = "log")),
                          
                          # input theme
                          radioButtons("themeTypeSingle", "Graph Theme",
                                       c("Gray" = "gray", "Classic" = "classic",
                                         "Minimal" = "min", "Dark" = "dark"))
                          
                        ),
                        
                        # mainbarPanel should have the plots
                        mainPanel(
                          tabsetPanel(id = "plotTabSingle",
                                      tabPanel("Across FACTOR 1",
                                               value = "factorSingle1",
                                               plotOutput("singlegene_plot1")
                                      ),
                                      tabPanel("Across FACTOR 2",
                                               value = "factorSingle2",
                                               plotOutput("singlegene_plot2")
                                      ),
                                      # DT DataTable of plotted data
                                      tabPanel("Data Table",
                                               DT::dataTableOutput("singleGeneTable"))
                                      
                          )
                        )
                      )
             ),
             
             
             ################################# UI Tab 2: Multi-Gene Analysis #################################
             tabPanel("Multi-Gene Analysis",  # part of navbarPage
                      
                      sidebarLayout(
                        # sidebarPanel should have all the input needed from the user
                        sidebarPanel(
                          tabPanel("Gene Expression"),
                          
                          # drop-down list to type in gene of interest to plot
                          # gene input, change 'choices' to a vector of all rownames of the data matrix
                          selectInput("userGeneMulti", "Choose gene(s) to plot:", choices = c(1,2,3)),
                          
                          # input factors
                          
                          
                          # input Graph type: Boxplot/Violin/Scatterplot/RadarCharts
                          radioButtons("graphTypeMulti", "Graph Type",
                                       c("Scatterplot" = "scatterplot",
                                         "Radar Chart" = "radar chart")),
                          
                          # input scale: linear/log
                          radioButtons("scaleTypeMulti", "Graph Scale",
                                       c("Linear" = "linear", "Log" = "log"))
                          
                        ), 
                        
                        # mainbarPanel should have the plots
                        mainPanel(
                          tabsetPanel(
                            tabPanel(paste0("Across OBTAIN FACTOR 1 FROM DATA/USER"),
                                     plotOutput("multigene_plot")
                            ),
                            tabPanel(paste0("Across OBTAIN FACTOR 2 FROM DATA/USER")
                                     # the ggplot across factor 2 should be here
                            ),
                            
                          )
                        )
                        
                      )
                      
             ),
             
             
             
             ################################# UI Tab 3: Gene Trajectories #################################
             tabPanel("Trajectories",
                      
                      sidebarLayout(
                        sidebarPanel(
                          # user input list of genes to plot
                          selectizeInput("userGeneTraj",
                                         "Choose the genes to plot:",
                                         choices = NULL, multiple = TRUE),
                          
                          # input factors
                          selectInput("plotFactorsTraj",
                                      "Select the factor to observe gene changes:",
                                      choices = NULL),
                          
                          # input theme
                          radioButtons("themeTypeSingle", "Graph Theme",
                                       c("Gray" = "gray", "Classic" = "classic",
                                         "Minimal" = "min", "Dark" = "dark"))
                          
                        ),
                        
                        # should contain graphs and DT table
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Trajectories Plot",
                                     plotOutput("trajPlot")
                            ),
                            
                            tabPanel("Combined Trajectories",
                                     plotOutput("trajPlotCombined")
                            ),
                            
                            tabPanel("Data Table",
                                     # DT DataTable of plotted data
                                     DT::dataTableOutput("trajGeneTable"))
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
  
  ################################## Input Tab ##################################
  
  ##### data matrix reader #####
  dataMatReader <- reactive({
    # await user input in the relevant fileInput
    dataFile <- input$inputData
    
    # suppresses error, basically waits for input before it continues render function
    req(dataFile)
    
    # read the uniquely produced datapath, read the file
    #TODO consider looking at the extension to change ',' if needed
    readr::read_delim(dataFile$datapath, ",", col_names = TRUE, show_col_types = FALSE)
  })
  
  ##### metadata #####
  metadataReader <- reactive({
    # await user input in the relevant fileInput
    metadataFile <- input$inputMetadata
    
    # suppresses error, basically waits for input before it continues render function
    req(metadataFile)
    
    # read the uniquely produced datapath, read the file
    metadata <- readr::read_delim(metadataFile$datapath, ",", col_names = TRUE,
                                  show_col_types = FALSE)
    
    # remove problematic characters from colnames
    #TODO: remove all possible interferences, and more elegantly than these multiple calls
    newColnames <- str_replace_all(colnames(metadata), " ", "_") %>%
      str_replace_all(":", "_")
    colnames(metadata) <- newColnames
    
    return(metadata)
  })
  
  
  # this provides the user factors to chose
  observeEvent(
    input$inputMetadata, {
      updateSelectizeInput(session = session, "chosenFactors",
                           "Choose all factors you wish to analyze:",
                           choices = colnames(metadataReader()),
                           server = TRUE)
    })
  
  
  # for testing: look at input data
  output$dataMatPeek <- renderTable({
    head(dataMatReader())
  })
  
  # for testing: look at input metadata
  output$metadataMatPeek <- renderTable({
    head(metadataReader())
  })
  
  # peek at the plot-ready data with factors by user
  output$filteredConverted <- renderDT({
    plotData <- filterAndConvert(dataLoader = dataMatReader(),
                                 metadataLoader = metadataReader(),
                                 plotGenes = c("Fndc5","Bdnf"),  # arbitrary gene choice. maybe change to random or let user choose?
                                 conditions = input$chosenFactors)
    
    DT::datatable(plotData)
  })
  
  ################################## Single-Gene Analysis ##################################
  
  
  ## updates sidePanel gene selectizeInput based on input matrix gene names
  observeEvent(
    input$inputData, {
      updateSelectizeInput(session = session, "userGeneSingle", "Choose a gene to plot:",
                           #TODO change '$gene' to a general call
                           choices = dataMatReader()$gene,
                           server = TRUE)
    })
  
  ## make an updating choice selection for factors to plot;
  observeEvent(
    input$chosenFactors, {
      updateSelectizeInput(session = session, "plotFactorsSingle",
                           "Select 2 factors to plot by (based on input in 'Input Data' tab):",
                           choices = input$chosenFactors,
                           selected = input$chosenFactors[1:2],
                           options = list(maxItems = 2))
    })
  
  ## calls the plot data for the plots and the table
  plotDataSingle <- reactive({
    
    req(input$userGeneSingle,
        input$plotFactorsSingle)
    
    filterAndConvert(dataLoader = dataMatReader(),
                     metadataLoader = metadataReader(),
                     plotGenes = input$userGeneSingle,
                     conditions = input$plotFactorsSingle)
  })
  
  
  ## prepare both plots dynamically by tab chosen (input$plotTabSingle)
  singlegene_plot <- reactive({
    
    req(input$userGeneSingle,
        input$plotFactorsSingle)
    
    plotData <- plotDataSingle()
    
    # the 2 factors to plot
    firstCondition <- input$plotFactorsSingle[1]
    secondCondition <- input$plotFactorsSingle[2]
    
    # dynamically choose the plot variable ordering between tabs
    xAxVar <- switch(input$plotTabSingle,
                     "factorSingle1" = firstCondition,
                     "factorSingle2" = secondCondition)
    facetVar <- switch(input$plotTabSingle,
                       "factorSingle1" = secondCondition,
                       "factorSingle2" = firstCondition)
    
    # add a column called 'facet' for faceting.
    #TODO: I couldn't get this working by directly calling the 'facetVar' in 'facet_wrap()'
    plotData$facet <- plotData[[facetVar]]
    
    # the full ggplot call
    ggplot(plotData, aes_string(x = xAxVar, y = "Expression",
                                group = xAxVar)) +
      
      # boxplot/violin based on input$graphType*
      switch(input$graphTypeSingle,
             # 'list(geom_*, geom_*)' is a working alternative to adding '+' between plot layers
             "boxplot" = list(geom_boxplot(aes_string(color = xAxVar),
                                           outlier.shape = NA),
                              geom_point(aes_string(alpha = 0.3, size = 5, color = xAxVar),
                                         position = position_jitterdodge())),
             "violin" = geom_violin(aes_string(fill = xAxVar), trim = FALSE)) +
      
      ylab("Expression") +
      xlab(xAxVar) +
      
      # plot theme based on input$themeType*
      themeChanger(input$themeTypeSingle) +
      
      facet_wrap(~facet, ncol = 2) +
      theme(legend.position = "none") +
      
      # y scale based on input$scaleType*
      switch(input$scaleTypeSingle, "linear" = scale_y_continuous(), "log" = scale_y_log10())
  })
  
  
  ## gives us the number of rows in a facet_wrapped ggplot,
  # for determining the height of mainPanel with plots
  heightSingle <- reactive({
    req(input$plotTabSingle)
    gg_facet_nrow(singlegene_plot())
  })
  
  
  ## Single Gene Plot
  # funny double-assignment because shiny (HTML actually) can't handle same named outputs in the UI
  output$singlegene_plot1 <- output$singlegene_plot2 <- renderPlot({
    singlegene_plot()
    # height is input here:
  }, height = function(){heightSingle()*350})
  
  
  ## output an interactive DT table showing the plotted information
  output$singleGeneTable <- DT::renderDT({
    plotData <- plotDataSingle()
    
    DT::datatable(plotData)
  })
  
  
  ################################## Multi-Gene Analysis ##################################
  
 ## updates sidePanel gene selectizeInput based on input matrix gene names
  observeEvent(
    input$inputData, {
      updateSelectizeInput(session = session, "userGeneMulti", "Choose gene(s) to plot:",
                           #TODO change '$gene' to a general call
                           choices = dataMatReader()$gene,
                           server = TRUE, selected = input$userGeneMulti[])
    })
  ## make an updating choice selection for factors to plot;
  observeEvent(
    input$chosenFactors, {
      updateSelectizeInput(session = session, "plotFactorsMulti",
                           "Select factors to plot by (based on input in 'Input Data' tab):",
                           choices = input$chosenFactors,
                           server = TRUE, selected = input$chosenFactors[])
      
    }) 
    
  ## calls the plot data for the plots and the table
  plotDataMulti <- reactive({
    
    req(input$userGeneMulti,
        input$plotFactorsMulti)
    
    plotData <- filterAndConvert(dataLoader = dataMatReader(),
                                 metadataLoader = metadataReader(),
                                 conditions = input$userGeneMulti,
                                 conditions = input$plotFactorsMulti)
  })
  
  
  multigene_plot <- reactive({
    
    req(input$userGeneMulti,
        input$plotFactorsMulti)
    
    #simulate data
    set.seed(5000)
    N = 10000
    
    #radar graph using ggradar
    
    g <-ggradar(multigene_plot, values.radar = c(0, 0.5, 1),
                axis.labels = paste0("userGeneMulti"),legend.title = "Factors",
                legend.position = "bottom", background.circle.colour = "white",
                axis.label.size = 8, group.point.size = 3)
    #"scatterplot" = pairs(data = plotData) 
    
    # y scale based on input$scaleType
    #switch(input$scaleTypeSingle, "linear" = scale_y_continuous(), "log" = scale_y_log10())
  })
  
  
## Multi Gene Plot

output$multigene_plot <- renderPlot({
  multigene_plot()
})


## output an interactive DT table showing the plotted information
output$multiGeneTable <- DT::renderDT({
  plotData <- plotDataMulti()
  
  DT::datatable(plotData)
})

    
  
  
  
  ################################## Trajectories ##################################
  
  
  ## updates sidePanel gene selectizeInput based on input matrix gene names
  observeEvent(
    input$inputData, {
      updateSelectizeInput(session = session, "userGeneTraj",
                           "Choose the genes to plot:",
                           #TODO change '$gene' to a general call
                           choices = dataMatReader()$gene,
                           server = TRUE)
    })
  
  
  ## choose the factor to observe changes over
  observeEvent(
    input$chosenFactors, {
      updateSelectInput(session = session, "plotFactorsTraj",
                        "Select the factor to observe gene changes:",
                        choices = input$chosenFactors,
                        selected = input$chosenFactors[1])
    })
  
  
  ## convert data to Z-score
  zScorer <- reactive({
    # load original matrix
    rawData <- dataMatReader()
    
    # calculate Z-score
    #TODO: how do I know col 1 is the genes? maybe allow the user to override this if necessary
    zData <- rawData[,2:ncol(rawData)] %>%
      sapply(function(df) (df-mean(df))/sd(df))
    
    # now return the 'gene' column
    zData <- cbind(rawData[,1], zData)
    
  })
  
  
  ## prepare data for table & plot
  plotDataTraj <- reactive({
    
    req(input$userGeneTraj,
        input$plotFactorsTraj)
    
    normalData <- filterAndConvert(dataLoader = dataMatReader(),
                                   metadataLoader = metadataReader(),
                                   plotGenes = input$userGeneTraj,
                                   conditions = input$plotFactorsTraj)
    
    zScoreData <- filterAndConvert(dataLoader = zScorer(),
                                   metadataLoader = metadataReader(),
                                   plotGenes = input$userGeneTraj,
                                   conditions = input$plotFactorsTraj)
    zScoreData <- zScoreData %>% dplyr::rename(ZScore = Expression)
    
    # add ZScore data as a new column to normalData
    normalData[["ZScore"]] <- zScoreData$ZScore
    normalData
  })
  
  
  ## create plots
  trajectory_plot <- reactive({
    
    # loads data with additional column of 'ZScore'
    plotDataZscore <- plotDataTraj()
    
    ggplot()
  })
  
  
  ## render plots
  output$trajPlot <- output$trajPlotCombined <- renderPlot({
    trajectory_plot()
    # transform matrix to z score matrix
    
  })
  
  
  ## output an interactive DT table showing the plotted information
  output$trajGeneTable <- renderDT({
    plotData <- plotDataTraj()
    
    DT::datatable(plotData)
  })
  
}



##### this connects the two and runs the shiny app #####
shinyApp(ui = ui, server = server)
# 'options = list(display.mode = "showcase")' presents code being run in the server, nice debugging tool..




