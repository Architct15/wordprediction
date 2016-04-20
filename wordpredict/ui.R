#
# This is the user-interface definition of the application
# Application Name: Next Word Prediction Application
# Author: Simon Chan
# Date: 19 Apr 2016
# 

library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    shinyUI(fluidPage(
        fluidRow(
            # Change the font style for the application
            includeCSS("www//style.css"),
            column(6,
                titlePanel(
                h1("Next Word Prediction Application", 
                   style = "font-family: 'Lobster', cursive;
                   font-weight: 500; line-height: 1.1; 
                   color: #4d3a7d;")),
                # Add a short introduction section 
                includeHTML("Introduction.html"),
                # Define input area
                titlePanel(h3("Input Text", style = "color:blue")),
                textInput("text", label = "", value = "A deck of ", width = "100%"),
                hr(),
                # Define output area
                titlePanel(h3("Predicted next word",style = "color:blue")),
                tableOutput('table')
            ),
            column(6,
                # Documentation section on the right side of screen
                includeHTML("Documentation.html")
            )
        )
    ))
))
