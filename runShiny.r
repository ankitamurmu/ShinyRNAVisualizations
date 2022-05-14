#### Libraries ####
library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)


ui <- fluidPage(
  # main tab: title of the whole page
  titlePanel("RNA Analysis"),
  
  # test out multiple themes
  shinythemes::themeSelector(),
  
  # accept a file:
  fileInput("inputData", "Enter your .csv file:"),
  # make tabs, which shiny function do we need?
  
  
  
  ##### tab 1: Gene Expression Across Factors #####
  sidebarLayout(
    # sidebarPanel should have all the input needed from the user
    sidebarPanel(
      tabPanel("Gene Expression"),
      
      # drop-down list to type in gene of interest to plot
      # gene input, change 'choices' to a vector of all rownames of the data matrix
      selectInput("userGene", "Choose a gene to plot:", choices = c(1,2,3))
      
      # input factors
      
      # input Graph type: Boxplot/Violin
      
      # input scale: linear/log
    ),  # end of 'sidebarPanel()'
    
    # mainbarPanel should have the plot, possibly tabs also?
    mainbarPanel(
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
  
  
  
  ##### tab 2: Gene Trajectories #####
  # user input list of genes to plot
  textAreaInput("Area", "Enter a list of genes seperated by new lines, commas, or spaces:",
                value = "Fndc5, Pgc1a\nBdnf, Itgb5")
  
)


server <- function(input, output, session) {
  
}

# this connects the two and runs the shiny app
shinyApp(ui = ui, server = server)




### EXAMPLE OF TABS FROM SOF
shinyApp(
  ui = fluidPage(
    tabsetPanel(
      tabPanel("Map", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
                 mainPanel(
                   htmlOutput("Attacks")
                 )
               )
      ),
      tabPanel("plot", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )
                 )
               )
      )
    )
  ), 
  server = function(input, output) {
    
  }
)