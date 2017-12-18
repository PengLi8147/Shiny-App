#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application to tune parameters for Random Forest
shinyUI(fluidPage(
   
   # Application title
   titlePanel("App to tune parameters for Random Forest Model"),
   
   # Inputs - numeric and sliderbar 
   sidebarLayout(
      sidebarPanel(
        numericInput("numeric", "The number of Trees ?",value = 20, min = 0, max = 500, step = 10), 
        sliderInput("sliderx","Pick the number of variables considered for each split" ,min = 0, max = 20, value = 7),
        sliderInput("sliderY","Pick the minimum size of terminal nodes" ,min = 0, max = 10, value = 5),
        submitButton("Submit", width = 200),
        h2("1.Input above"),
        h2(""),
        h3("2.Then Click Submit"),
        h2(""),
        h3(" *May take some time * "),
        h3(" * Please be patient * "),
        h3(" * Thank you * ")
      ),
      # Show two plots along with two prediction accuracy
      mainPanel(
        h3("Prediction Accuracy on the Out of Bag samples"),
        textOutput("pred1"),
        h3("Prediction Accuracy on the Test data samples"),
        textOutput("pred2"),
        h3("Confustion Matrix Plot"), 
        plotOutput("plot2"),
        h3("Feature Importance Plot"), 
        plotOutput("plot1")
      )
   )
))

# Run the application 
#shinyApp(ui = ui, server = server)


