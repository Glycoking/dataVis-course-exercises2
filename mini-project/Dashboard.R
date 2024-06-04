library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
#setwd("~/Github/dataVis-course-exercises2/mini-project")
#setwd("~/Github/dataVis-course-exercises2/mini-project")
load("data/aggData.rda")
data <- agg_dataNum

# Shiny -------------------------------------------------------------------


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Hospital stats"), #Top
  dashboardSidebar(),
  dashboardBody(
    #box(plotOutput("quarter_plot"), width = 8),# Plot
    #box(selectInput("quarter", "Quarters", c("all", "Q1","Q2","Q3","Q4")), width = 4),
    #box(selectInput("gender", "Gender", c("all", "female","male")), width = 4)),
    box(plotOutput("Quality_indicator")),
    box(selectInput(inputId = "KPI", label = "KPI", choices = c("discharge_any_anticoagulant","door_to_groin", 
                                    "door_to_needle","discharge_any_antiplatelet",
                                    "imaging_done","dysphagia_screening_type", 
                                    "thrombolysis","hospitalized_in"))),
    #box(checkboxGroupInput(inputID = "KPI2", label = "KPI2", choices = c("discharge_any_anticoagulant","door_to_groin", 
    #                                                                    "door_to_needle","discharge_any_antiplatelet",
    #                                                                    "imaging_done","dysphagia_screening_type", 
    #                                                                    "thrombolysis","hospitalized_in"))),
))
# Lav et linjeplot, #Lav en tjekboks

server <- function(input, output) {
  
  output$Quality_indicator <- renderPlot({
    x <- data["QI"] %>% filter(data["QI"] == input$KPI)
    ggplot(data = x, aes(x = input$KPI)) + geom_bar(fill = "darkblue")
  })
  output$Quality_indicator <- renderPlot({
    x <- data["QI"] %>% filter(data["QI"] == input$KPI)
    ggplot(data = x, aes(x = input$KPI)) + geom_bar(fill = "darkblue")
  })
  
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
x <- data["QI"]
x == "door_to_needle"


x <- data %>% filter(QI == "door_to_needle")



x <- data["QI"]
x<- x%>% filter(data["QI"]=="door_to_needle")
ggplot(data = x, aes(x = "door_to_needle")) + geom_bar(fill = "darkblue")



#x <- data %>% filter(subGroup == "gender")
#ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(
#  fill = "yellow2", colour="black") + theme_classic()

#ggplot(data = subset(x,!is.na(QI)), aes(x=QI)) + geom_bar(
#  fill = "yellow2", colour="black") + theme_classic()


#ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(fill="darkblue",width = 0.6) + theme_classic()
       

#QI har mulighederne "door_to_groin", NA, "discharge_any_anticoagulant", "door_to_needle"

#Finding changes in KPIs (use agg_dataNum data frame as a starting point)
#Clinicians currently using this scenario: 50
#Clinicians produce tables on line plots that show indicators for every month/quarter. 
#They use this data to observe whether their care quality is improving or deteriorating. 
#Almost all clinicians (43) do this for only a few selected variables that they identify as 
#most important or that they think their hospital struggles with. Few clinicians (7) also 
#consider whether the change is sudden (e.g. a significant drop in quality since last month)
#or if it has been a slow change occurring over a longer period of time. Many clinicians (24) 
#consider a change more valid to explore if it follows a trend.

