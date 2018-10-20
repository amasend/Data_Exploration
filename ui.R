library(shiny)
library(plotly)

ui <- fluidPage(
  h4("Average temperature anomaly for each year in 1880-2016 period",align="center"),
  div(plotlyOutput("plot",width = "500px", height = "300px"), align = "center"),
  h4("Monthly temperature anomaly for specific year",align="center"),
  div(plotlyOutput("plot2",width = "500px", height = "300px"), align = "center")
)
