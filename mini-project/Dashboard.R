library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
load("data/aggData.rda")
data <- agg_dataNum

# Shiny -------------------------------------------------------------------


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Hospital stats"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "hospital_stats", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(skin = "black",
    tabItems(
      tabItem(tabName = "hospital_stats",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(
                  title = "Slider",
                  sliderInput("slider", "Antal observationer:", 1, 5000, 100)
                )
              )
      ),
      tabItem(tabName = "widgets",
              h2("Indhold i Widgets-fanen")
      )
    )
  )
)

server <- function(input, output) {
  data <- data %>% filter(subGroup == "gender")


  output$plot1 <- renderPlot({
    data <- data[seq_len(input$slider)]
    plot(data = data, aes(x=subGroupVal)) + geom_bar(fill = "yellow2", colour="black")
  })
}

# ;) ----------------------------------------------------------------------


shinyApp(ui, server) #Dashboard

#glimpse(data)

x <- data %>% filter(subGroup == "gender")

ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(
  fill = "yellow2", colour="black") + theme_classic()

ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(fill="darkblue",width = 0.6) + theme_classic()
       
