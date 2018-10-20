#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

ui <- fluidPage(
  h4("Average temperature anomaly for each year in 1880-2016 period",align="center"),
  actionButton("go", "Fetch data"),
  actionButton("show_plot1", "Plot"),
  textOutput("table"),
  # #numericInput("n", "n", 50),
  plotlyOutput("plot")
  
  # h4("Monthly temperature anomaly for specific year",align="center"),
  # plotlyOutput("plot2")
  
  #div(plotlyOutput("plot",width = "500px", height = "300px"), align = "center"),
  #h4("Monthly temperature anomaly for specific year",align="center"),
  #div(plotlyOutput("plot2",width = "500px", height = "300px"), align = "center")
)


# library(shiny)
# 
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        sliderInput("bins",
#                    "Number of bins:",
#                    min = 1,
#                    max = 50,
#                    value = 30)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        plotOutput("distPlot")
#     )
#   )
# ))
