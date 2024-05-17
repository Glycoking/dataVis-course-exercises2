library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)

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
                  sliderInput("slider", "Antal observationer:", 1, 100, 50)
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
  set.seed(122)
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
load("data/aggData.rda")
data <- agg_dataNum
glimpse(data)

x<- data$quarter
ggplot(data = data, aes(y=subGroupVal)) + geom_bar(fill = "yellow", colour="black")

       