library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
setwd("~/Github/dataVis-course-exercises2/mini-project")
load("data/aggData.rda")
data <- agg_dataNum

# Shiny -------------------------------------------------------------------
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Dashboard for clinicians"), #Top
  dashboardSidebar(),
  dashboardBody(
    box(selectInput(inputId = "KPI", label = "Select a KPI", choices = c("discharge_any_anticoagulant","door_to_groin", 
                                    "door_to_needle","discharge_any_antiplatelet",
                                    "imaging_done","dysphagia_screening_type", 
                                    "thrombolysis","hospitalized_in"))),
    box(checkboxInput("Color", "Colorblind", value = FALSE)),
    #box(sliderInput("Year_slider", "Set year range", value=c(2016,2024), min = 2016, max = 2024)),
    box(selectInput(inputId ="quarter",label = "Quarters", c("Q1","Q2","Q3","Q4")), width = 4),
    #box(dateRangeInput("Year", "YQ", c("all","2016","2017", "2018","2019","2020","2021", "2022", "2023")), width = 4),
    #box(selectInput("gender", "Gender", c("all", "female","male")), width = 4)),
    box(plotOutput("KPI_plot"), width = 6, height = 6),
    box(plotOutput("Scatter")),
    #box(checkboxGroupInput(inputID = "KPI2", label = "KPI2", choices = c("discharge_any_anticoagulant","door_to_groin", 
    #                                                                    "door_to_needle","discharge_any_antiplatelet",
    #                                                                    "imaging_done","dysphagia_screening_type", 
    #                                                                    "thrombolysis","hospitalized_in"))),'
    # data input1 <= ?? <= input2
))
# Lav et linjeplot,
#Plot der viser KPI forskelle fra år til år
server <- function(input, output) {
  output$KPI_plot <- renderPlot({
    color1 <- "blue"
    if (input$Color == TRUE){color1 <- "red"}
    #data <- data[data$quarter == input$quarter, ]
    #data <- data %>% drop_na()
    #data <- data[data$QI == input$KPI ]
    ggplot(data = data, aes(x = paste0(data$year),y = input$KPI)) + geom_bar(fill = color1, stat = "identity") +
      labs( x = "Year", y = "KPI") 
  }
)
output$Scatter <- renderPlot({
  color1 <- "blue"
  if (input$Color == TRUE){color1 <- "red"}
  data <- data[data$QI == input$KPI , ]

  data <- data[complete.cases(data["QI"]), ]
  ggplot(data = data, aes(x = quarter, y = input$KPI )) + geom_bar(stat = "identity", fill = color1, ylab = "KPI")  +
    labs( x = "Quarter", y = "KPI") + theme(axis.title.y = element_blank())
})

}
# ;) ----------------------------------------------------------------------


shinyApp(ui, server) #Dashboard




#x <- data %>% filter(QI == "door_to_needle")
my_color <- c("red","blue", "yellow")

#data <- na.omit(data)
ggplot(data = data, aes(x = qual4Gold, y =qual4Gold)) + geom_line()

condition <- rep(c("qual4Gold" , "qual4Diamond" , "qual4Platinum"))
#ggplot(data=data, aes(fill=condition, y=C_Value, x =))+ geom_bar(position ="stack", stat = "identity")
ggplot(data = data[data$year == 2018,], aes(y = Value, x=QI)) +geom_bar(stat = "identity")
ggplot(data = data[data$year == 2018,], aes( x=nameOfAggr,y = Value,)) +geom_bar(stat = "identity")
#Compare 2 KPI
ggplot(data = data[data$year == 2018,], aes(x=quarter,y = nameOfAggr[])) +geom_point()
ggplot(data = data[data$year == 2018,], aes(x=quarter,y = nameOfAggr["door_to_needle"])) +geom_bar(stat="identity")
#position ="stack", stat = "identity"

#x <- data["QI"]
#x<- x%>% filter(data["QI"]=="door_to_needle")
#ggplot(data = x, aes(x = "door_to_needle")) + geom_bar(fill = "darkblue")



#x <- data %>% filter(subGroup == "gender")
#ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(
#  fill = "yellow2", colour="black") + theme_classic()

#ggplot(data = subset(x,!is.na(QI)), aes(x=QI)) + geom_bar(
#  fill = "yellow2", colour="black") + theme_classic()


#ggplot(data = subset(x,!is.na(subGroupVal)), aes(x=subGroupVal)) + geom_bar(fill="darkblue",width = 0.6) + theme_classic()
       



#Clinicians produce tables on line plots that show indicators for every month/quarter. 
#They use this data to observe whether their care quality is improving or deteriorating. 
#Almost all clinicians (43) do this for only a few selected variables that they identify as 
#most important or that they think their hospital struggles with. Few clinicians (7) also 
#consider whether the change is sudden (e.g. a significant drop in quality since last month)
#or if it has been a slow change occurring over a longer period of time. Many clinicians (24) 
#consider a change more valid to explore if it follows a trend.

