library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
setwd("~/Github/dataVis-course-exercises2/mini-project")
load("data/aggData.rda")
data <- agg_dataNum


# Shiny -------------------------------------------------------------------
ui <- pageWithSidebar(
  headerPanel("Settings"),
    sidebarPanel(fluid = FALSE,          #Sidebar panel for inputs
      checkboxInput("Color", "Colorblind mode (Deuteranomaly)", value = FALSE),
      selectInput(inputId = "KPI", label = "Select a KPI", choices = c("discharge_any_anticoagulant","door_to_groin", "door_to_needle","discharge_any_antiplatelet", "thrombolysis","hospitalized_in")),
      checkboxGroupInput(inputId = "YEAR", label = "Select a year", choices = list("2016"=2016,"2017"=2017, "2018"=2018,"2019"=2019, "2020"=2020,"2021"=2021, "2022"=2022 ), selected = 2022),
      #selectInput(inputId = "KPI", label = "Select a KPI", choices = c("af_p_dis_anticoag","dgt_leq_120", "dgt_leq_90","dnt_leq_45", "dnt_leq_60","door_to_groin","door_to_needle","isp_dis_antiplat","p_ct_mri_first_hosp","p_dys_screen","rec_total_is","sp_hosp_stroke_unit_ICU")),
      #selectInput(inputId = "hospital", label = "Select a hospital", choices = c("General","Progress", "Rose","all","Angelvale","Memorial", "Paradise","Hope", "Mercy", "Samaritan")),
      #selectInput(inputId = "aggregation", label = "Select a aggregation", choices = c("af_p_dis_anticoag","dgt_leq_120", "dgt_leq_90","dnt_leq_45","dnt_leq_60","door_to_groin", "door_to_needle","isp_dis_antiplat", "p_ct_mri_first_hosp", "p_dys_screen", "rec_total_is", "sp_hosp_stroke_unit_ICU")),
      #selectInput(inputId = "subgroup", label = "Select a subgroup", choices = c("gender","imaging_done", "prenotification","stroke_type")),
      #selectInput(inputId = "award", label = "Select award", choices = c("qual4Gold","qual4Diamond", "qual4Platinum")),
  ),
  mainPanel(
    plotOutput("above"),
    plotOutput("Scatter"),
    plotOutput("KPI_plot"),
  )
   
)




server <- function(input, output) {
  output$KPI_plot <- renderPlot({
    color1 <- "blue"
    if (input$Color == TRUE){color1 <- "red"}
    data <- data[data$h_name == input$hospital, ]
    ggplot(data = data, aes(x = paste0(data$year),y = input$KPI)) + geom_bar(fill = color1, stat = "identity") +
      labs( x = "Year", y = "") 
  }
)
  output$Scatter <- renderPlot({
    data <- data[data$QI == input$KPI 
                 & data$h_name != "all" 
                 & data$aggFunc == "pct" 
                 & data$isYearAgg == TRUE
                 & data$year %in% input$YEAR
                 & complete.cases(data["QI"]), ]
    ggplot(data = data, aes(x=h_country, y =C_Value ,fill = h_country)) + 
      geom_boxplot() +
      labs(x = "Country name", y = "Value in %", fill = "Country", 
           title = paste0("Yearly *",input$KPI, "* for country in selected years"), caption = "Source: agg_dataNum") +
      theme_classic() +
      theme(axis.text.x = element_text(face="bold",size = 11)) +
      if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
    
  
    
    
    
    
})
  output$above <- renderPlot({
    data <- data[data$QI == input$KPI 
                 & data$h_name != "all" 
                 & data$aggFunc == "pct" 
                 & data$isYearAgg == TRUE
                 & data$year %in% input$YEAR
                 & complete.cases(data["QI"]), ]
    ggplot(data = data, aes(x=h_name, y =Value ,fill = h_country)) + 
      geom_boxplot() +
      labs(x = "Hospital name", y = "Value in %", fill = "Country", title = paste0("Yearly *",input$KPI, "* for hospital in selected year"), caption = "Source: agg_dataNum") +
      theme_classic() +
      theme(axis.text.x = element_text(face="bold",size = 11)) +
      if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
    
})


  
}
# ;) ----------------------------------------------------------------------




shinyApp(ui, server) #Dashboard


#ggplot(data = data[data$QI == "door_to_groin",], aes(x=year, )) + geom_bar()
#ggplot(data = data[data$QI == "door_to_groin",], aes(x=h_name, y =diffFromC ,fill = h_country)) + 
#  geom_bar(stat="identity", position ="dodge") + 
#  geom_hline(yintercept = 0, linetype= "dashed") +
#  labs(fill = "test", caption = "source: agg_dataNum") +
#  theme(axis.text.x = element_text(angle=, vjust=2))
ggplot(data = data[data$aggFunc == "pct" & data$QI == "door_to_needle" & data$h_name != "all",], aes(x=h_name, y =Value ,fill = h_country)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype= "dashed") +
  labs(fill = "test", caption = "source: agg_dataNum", title = paste0("test",q)) +
  theme(axis.text.x = element_text(angle=, vjust=2, size = 11))

ggplot(data = data[data$aggFunc == "pct" & data$QI == "door_to_needle" & data$h_name != "all",],
       aes(x=h_country, y =C_Value ,fill = h_country)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype= "dashed") +
  labs(fill = "test", caption = "source: agg_dataNum", title = paste0("test",q)) +
  theme(axis.text.x = element_text(angle=, vjust=2, size = 11))


plot_ly(data=data[data$subGroup == "stroke_type" #STROKE PLOT
                  & complete.cases(data["subGroupVal"])
                  ,],
        x = ~subGroupVal,y=~subGroupVal, type ="bar")


ggplot(data=data[data$subGroup == "stroke_type" #STROKE PLOT
                 & complete.cases(data["subGroupVal"])
                 ,],
       aes(x=subGroupVal)) +
  geom_bar()
ggplot(data=data[data$subGroup == "gender" #GENDER plot
                 & complete.cases(data["subGroupVal"])
                 ,],
       aes(x=subGroupVal)) +
  geom_bar()

ggplot(data=data[data$subGroup == "imaging_done" #Imaging done plot
                 & complete.cases(data["subGroupVal"])
                 ,],
       aes(x=subGroupVal)) +
  geom_bar()
ggplot(data=data[data$subGroup == "prenotification" #PRE notification PLOT
                 & complete.cases(data["subGroupVal"])
                 ,],
       aes(x=subGroupVal)) +
  geom_bar()
#

#ggplot(data = data[data$QI == "door_to_groin",], aes(x=year, y = qual4Gold, fill = h_country)) + geom_bar(stat="identity")




























#x <- data %>% filter(QI == "door_to_needle")
#my_color <- c("red","blue", "yellow")

#data <- na.omit(data)
#ggplot(data = data, aes(x = qual4Gold, y =qual4Gold)) + geom_line()

#condition <- rep(c("qual4Gold" , "qual4Diamond" , "qual4Platinum"))
#ggplot(data=data, aes(fill=condition, y=C_Value, x =))+ geom_bar(position ="stack", stat = "identity")
#ggplot(data = data[data$year == 2018,], aes(y = Value, x=QI)) +geom_bar(stat = "identity")
#ggplot(data = data[data$year == 2018,], aes( x=nameOfAggr,y = Value,)) +geom_bar(stat = "identity")
#Compare 2 KPI
#ggplot(data = data[data$year == 2018,], aes(x=quarter,y = nameOfAggr[])) +geom_point()
#ggplot(data = data[data$year == 2018,], aes(x=quarter,y = nameOfAggr["door_to_needle"])) +geom_bar(stat="identity")

#ggplot(data = data, aes(x=, group = (qual4Gold)) + geom_bar()
       
      
#ggplot(data = data, aes(x=QI, y =data_pts )) + geom_point()
#ggplot(data=data, aes(x= data$Value, y = data$data_Pts)) + geom_point()

 
#data <- group_by(data, qual4Gold,qual4Diamond,qual4Platinum)  
  
#summarise(data)
#g<- table(data$h_name)
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

