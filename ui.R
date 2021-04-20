#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This applet was designed by Krissi Alari as apart of her senior capstone at
# Cal State Monterey Bay. This applet was designed specifically for E. coli
# testing in lettuce for a particular food testing company. The re-paramterization
# option of risk levels is only applicable for their testing purposes. The rest of
# the applet (using "Average (μ)" as the parameter of interest) is suitable for 
# general use of the SPRT.


library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Sequential Probability Ratio Test"),

    # Sidebar with inputs for numeric values and 
    sidebarLayout(
        sidebarPanel(
            
            textInput( inputId = "data.receive", 
                       label="Input your sample data points separated by commas. Data points should already be in log10 scale. (e.g., 6.44, 6.55, 6.68, 6.50,...).",
                       value="6.44, 6.55, 6.68, 6.50"),
            
            numericInput( inputId = "alpha",
                          label="Input your significance level, α.", 
                          value=0.01
            ),
            
            numericInput( inputId = "beta",
                          label="Input your value for β such that (1-β) is your desired power.", 
                          value=0.01
            ),
            
            selectInput(inputId="muCheck",
                        label="Input your parameter of interest",
                        choices =c("Average (μ)" = 'Opt1', "Risk level (r)" = 'Opt2')
            ),
            
            # Only shows up if user chooses Opt1: Input mu0 and mu1
            conditionalPanel(condition = "input.muCheck=='Opt1'",
                             numericInput( inputId="mu0",
                                           label="What is your value for μ0?", 
                                           value = 7),
                             numericInput( inputId="mu1",
                                           label="What is your value for μ1?", 
                                           value=4)
            ),
            
            # Only shows up if user chooses Opt2: Input r0 and r1
            conditionalPanel(condition = "input.muCheck=='Opt2'",
                             numericInput( inputId="r0",
                                           label="What is your acceptable null risk (r0)?", 
                                           value = 0.1),
                             numericInput( inputId="r1",
                                           label="What is your acceptable alternative risk (r1)?", 
                                           value=0.001)
            ),
            
            numericInput( inputId = "sigma",
                          label="What is your assumed value for σ? (Default 1.5)", 
                          value=1.5
            ),
            
            
            submitButton( "Submit", icon("refresh") )
        ),

        # Output
        mainPanel(
            ## Allows the boxplot and SPRT plot to be shown side by side
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot"), plotOutput("sprtPlot")),
            br(), ## adds some space between outputs
            plotOutput("distributions", height=250), ## Null/Alt Distributions plot
            htmlOutput("headingtext"),  ## For text output
            htmlOutput("sprtRslt"), ## For text output
            br()
        )
    )
))
