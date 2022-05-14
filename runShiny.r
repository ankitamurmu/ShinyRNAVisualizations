#### Libraries ####
library(shiny)
library(dplyr)


ui <- fluidPage(
  # title of the whole page
  titlePanel("RNA Analysis")
)


server <- function(input, output, session) {
  
}

# this connects the two and runs the shiny app
shinyApp(ui = ui, server = server)