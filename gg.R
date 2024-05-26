library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
setwd("~/Github/dataVis-course-exercises2/mini-project")
load("data/aggData.rda")
data <- agg_dataNum



ui <- dashboardPage(
  dashboardHeader(title = "Hospital information"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("correlation_plot"),width = 8),
    box(
      selectInput("GG", "Features:", c("Sepal.Width", "Petal.Length", "Petal.Width")), width =4)
    )
  )
  

server <- function(input, output) {
  output$correlation_plot <- renderPlot({
    plot(iris$Sepal.Length, iris[[input$GG]], xlab = "Sepal length", ylab = "Features")
  })
}




shinyApp(ui, server) #Dashboard


