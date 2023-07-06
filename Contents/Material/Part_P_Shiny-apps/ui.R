library(shiny)
library(tidyverse)
library(MASS)
library(splines)
library(ISLR)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Statistics in the Sky with Diamonds"),
    
    # Description
    p("A Shiny application, predicting the Carat of different Cuts of Diamonds, by their Price."),
    
    

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        
            sliderInput("priceInput", "Price", 0, 20000, c(100, 10000)),
            sliderInput("caratInput", "Carat", 0, 5, c(0, 5), step = 0.1),
            checkboxGroupInput("cutInput", "Diamond Cut",
                     choices = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
                     selected = c("Fair", "Good", "Very Good", "Premium", "Ideal")),
            selectInput("colInput", "Colour Choice", 
                     choices = c("Pastel1", "Paired", "Spectral", "RdBu", "PuOr"),
                     selected = "Spectral"),
            checkboxInput("regInput", "Linear regression", value = FALSE, width = NULL),
            checkboxInput("sqInput", "Square regression", value = FALSE, width = NULL),
            checkboxInput("cubInput", "Cubic regression", value = FALSE, width = NULL)
            ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # Sub-Heading
            h4("Graphical Output"),
            
            # Main Graphical Output
            plotOutput("diaPlot"),
            
            # Sub-Heading
            h4("Statistical Analysis"),
            
            # Statistical Analysis Outcome
            tableOutput("modelres"),
            
            # Text Commentary 
            textOutput("statout")
        )
    )
))
