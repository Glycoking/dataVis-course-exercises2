library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
setwd("~/Github/dataVis-course-exercises2/mini-project")
load("data/aggData.rda")
data <- agg_dataNum

# Shiny -------------------------------------------------------------------


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Hospital stats"), #Top
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("quarter_plot"), width = 8),# Plot
    box(selectInput("quarter", "Quarters", c("all", "Q1","Q2","Q3","Q4")), width = 4),
    box(selectInput("gender", "Gender", c("all", "female","male")), width = 4))
)


server <- function(input, output) {
  x <- data %>% filter(subGroup == "gender")
  output$quarter_plot <- renderPlot({ #Plot
    ggplot(data = data[[input$gender]]%>% filter(subGroup == "gender"), aes(x=subGroupVal)) + geom_bar()#Argumenter
    #plot(x$year, x$qual4Gold,xlab = "gg", ylab = "Feature")
    #ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(fill="darkblue",width = 0.6) + theme_classic()
  })

}

# ;) ----------------------------------------------------------------------


shinyApp(ui, server) #Dashboard

#glimpse(data)

x <- data %>% filter(subGroup == "gender")

ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(
  fill = "yellow2", colour="black") + theme_classic()

ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(fill="darkblue",width = 0.6) + theme_classic()
       
