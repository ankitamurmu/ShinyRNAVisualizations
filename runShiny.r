#### Libraries ####
library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)


ui <- fluidPage(
  # main tab: title of the whole page
  titlePanel("RNA Analysis"),

  # test out multiple themes
  shinythemes::themeSelector(),
  
  # accept a file:
  fileInput("inputData", "Enter your .csv file:"),
  # make tabs, which shiny function do we need?
  
  
  
  ####### Tab 1: Gene Expression #######
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
                   c("Boxplot" = "boxplot", "Violin" = "violin"))
      
      # input scale: linear/log
    ),  # end of 'sidebarPanel()'
    
    # mainbarPanel should have the plots
    mainPanel(
      tabsetPanel(
        tabPanel(paste0("Across OBTAIN FACTOR 1 FROM DATA/USER")
                 # the ggplot should be here
        ),
        tabPanel(paste0("Across OBTAIN FACTOR 2 FROM DATA/USER")
                 # the ggplot across factor 2 should be here
        )
      )
    )
    
  ),  # end of 'sidebarLayout()'
  
  
  
  
  
  ####### Tab 2: Gene Trajectories #######
  sidebarLayout(
    sidebarPanel(
      # user input list of genes to plot
      textAreaInput("Area", "Enter a list of genes seperated by new lines, commas, or spaces:",
                    value = "Fndc5, Pgc1a\nBdnf, Itgb5")
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

##################################################### SERVER #####################################################
server <- function(input, output, session) {
  ####### Tab 1 #######
  
  ####### Tab 2 #######
  # render plot

  
  # render interactive DT table
}


# this connects the two and runs the shiny app
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))
# 'display.mode = showcase' presents code being run in the server, nice debugging tool..




