#ui for Marrel
library(shiny)
#https://github.com/KarthiAru/Data-Science-Specialization-Coursera/tree/master/10.%20Capstone/shinyapp
#

shinyUI(
  fluidPage(
    theme = "main.css",
    fluidRow(
      column(8, offset = 2,
             br(), br(),
             h1("Marrel"), br(),br(),
             #onHover <- "What is the "MAR REL" key do?Say you're typing and you reach where you have your margins set but want to type more beyond that; you would press this button to do so."
             p(textInput(inputId = "text",label = ""),
               textOutput("prediction")),
             #br(), em(strong("Note: "), span("orange", style = "color: #d16527;"), " indicates the autocomplete hints, ", span("blue", style = "color: #176de3;"), " indicates the predicted word", style = "color: #AAA"),
             br(), br(),checkboxInput("details", "Details", TRUE),
             conditionalPanel(
               condition = "input.details == true",
               wellPanel(
                 p(strong("Tokenized Phrase: "), textOutput("clean", inline = TRUE))
               )
             )
      )
    )))