library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction"),
  
  fluidRow(
    column(12,
           textInput("textinput", "Your text input to predict the next word", width = '100%')
    )
  ),
  fluidRow(
    column(6,
           plotOutput("plotTopwords")
    ),
    column(6,
           radioButtons("selectWord", label = "Select the top word to be inserted",
                        choices = character(0), selected = character(0))
    )
  )
))
