library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
setwd("~/Github/dataVis-course-exercises2/mini-project")
load("data/aggData.rda")
data <- agg_dataNum
#Usage scenario: Compare current quarter/month to last see improvement

# Shiny -------------------------------------------------------------------
ui <- pageWithSidebar(
  headerPanel("Settings"),
    sidebarPanel(fluid = FALSE,          #Sidebar panel for inputs
      checkboxInput("Color", "Colorblind mode (Deuteranomaly)", value = FALSE),
      selectInput(inputId = "aggregation", label = "Select a KPI", choices = c("af_p_dis_anticoag","dgt_leq_120", "dgt_leq_90","dnt_leq_45","dnt_leq_60","isp_dis_antiplat", "p_ct_mri_first_hosp", "p_dys_screen", "rec_total_is", "sp_hosp_stroke_unit_ICU")),
      checkboxGroupInput(inputId = "YEAR", label = "Select year(s)", choices = list("2016"=2016,"2017"=2017, "2018"=2018,"2019"=2019, "2020"=2020,"2021"=2021, "2022"=2022 ), selected = 2022),
      selectInput(inputId = "hospital", label = "Select a hospital", choices = c("all","General","Progress", "Rose","Angelvale","Memorial", "Paradise","Hope", "Mercy", "Samaritan")),
      #selectInput(inputId = "KPI", label = "Select a KPI", choices = c("af_p_dis_anticoag","dgt_leq_120", "dgt_leq_90","dnt_leq_45", "dnt_leq_60","door_to_groin","door_to_needle","isp_dis_antiplat","p_ct_mri_first_hosp","p_dys_screen","rec_total_is","sp_hosp_stroke_unit_ICU")),
      #selectInput(inputId = "KPI", label = "Select a KPI", choices = c("discharge_any_anticoagulant","door_to_groin", "door_to_needle","discharge_any_antiplatelet", "thrombolysis","hospitalized_in")),
      

  ),
  mainPanel(
    plotlyOutput("angel"),
    plotlyOutput("angel_year"),
    plotlyOutput("first"),
    plotlyOutput("first_year"),
    plotlyOutput("KPI_Hospital"),
    helpText("Prone to missing data"),
    plotlyOutput("KPI_Year"),
    helpText("Prone to missing data"),
    helpText("Hover data from plot above to analyse"),
    plotlyOutput("missing"),
    helpText("Make a selection of data from the plot above to see a regression line of the data"),
    plotlyOutput("missing_click"),
    helpText("Trend of the hospital, are they forgetting to input data?"),
    plotlyOutput("country_value"),
    helpText("Make a selection of data from the plot above to see a regression line of the data"),
    plotlyOutput("country_value_click"),
    
  )
   
)




server <- function(input, output) {
  
  output$KPI_Hospital <- renderPlotly({
    data <- data[data$nameOfAggr == input$aggregation
                 & data$h_name != "all" 
                 & data$aggFunc == "pct" 
                 & data$isYearAgg == TRUE
                 & data$year %in% input$YEAR
                 & complete.cases(data["QI"]), ]
    ggplotly(ggplot(data = data, aes(x = h_name, y = Value ,fill = h_country)) + 
      geom_boxplot() +
      labs(x = "Hospital name", y = "Value in %", fill = "Country", title = paste0("Yearly *",input$KPI, "* for hospital"), caption = "Source: agg_dataNum") +
      theme_classic() +
      theme(axis.text.x = element_text(face="bold",size = 10)) +
      if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
    , source = "kpihospital")
})
  output$KPI_Year <- renderPlotly({
    data <- data[data$nameOfAggr == input$aggregation
                 & data$h_name != "all" 
                 & data$aggFunc == "pct" 
                 & data$isYearAgg == TRUE
                 & data$year %in% input$YEAR
                 & complete.cases(data["QI"]), ]
    ggplotly(ggplot(data = data, aes(x=h_country, y =C_Value ,fill = h_country)) + 
      geom_boxplot() +
      labs(x = "Country name", y = "Value in %", fill = "Country", 
           title = paste0("Yearly *",input$KPI, "* for country in selected years"), caption = "Source: agg_dataNum") +
      theme_classic() +
      theme(axis.text.x = element_text(face="bold",size = 10)) +
      if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
    , source = "KIP_YEAR")
})
  
  output$scatter <- renderPlotly({
    data <- data[data$isYearAgg == FALSE
                 & data$isAngelKPI == TRUE
                 & data$nameOfAggr == input$aggregation
                 & data$year %in% input$YEAR
                 & data$subGroup == "stroke_type"
                 #& data$aggFunc == "pct"
                 & !is.na(data$subGroup)
                 ,] 
      #data <- data$Subgroup <- mutate(Subgroup = factor(data$subGroupVal)) 
      ggplotly(
      #ggplot(data = data, aes(x = YQ, y = Value, size = data_Pts, color = "stroke_type")) +  #SCATTER plot
        ggplot(data = data, aes(x = subGroupVal, fill = subGroupVal)) +
        geom_bar() +theme_classic()

, source = "scatter"      
)
  })
  output$angel <- renderPlotly({
    data <- data[data$h_name == input$hospital
                 & data$nameOfAggr == input$aggregation
                 & data$isYearAgg == FALSE
                 & data$year %in% input$YEAR
                 ,] 

    new_data <- data.frame(
      Category = data$YQ,
      SubCategory = data$h_name,
      Gold = data$qual4Gold,
      Platinum = data$qual4Platinum,
      Diamond = data$qual4Diamond
    )
    long_data <- new_data %>%
      gather(key = "Award", value = "Value", Gold, Platinum, Diamond)
    
    ggplotly(ggplot(long_data, aes(x = Category, y = Value, fill = Award)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ SubCategory) +
        labs(x= "Quarter", y = "Angel awards", title = paste0("Quarterly Angel award in *",input$aggregation, "* for *",  input$hospital, "* hospital")) +
         theme_classic() +
        scale_fill_manual(values = c("darkcyan","gold","lightgrey")) +
        if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
      , source = "angel"
    )
  })  
  output$angel_year <- renderPlotly({
    data <- data[data$h_name == input$hospital
                 & data$nameOfAggr == input$aggregation
                 & data$isYearAgg == TRUE
                 & data$year %in% input$YEAR
                 ,] 
    clickData <- event_data("plotly_hover", source = "angel")
    new_data <- data.frame(
      Category = data$year,
      SubCategory = data$h_name,
      Gold = data$qual4Gold,
      Platinum = data$qual4Platinum,
      Diamond = data$qual4Diamond
    )
    long_data <- new_data %>%
      gather(key = "Award", value = "Value", Gold, Platinum, Diamond)
    
    ggplotly(ggplot(long_data, aes(x = Category, y = Value, fill = Award)) +
               geom_bar(stat = "identity", position = "stack") +
               facet_wrap(~ SubCategory) +
               labs(x= "Year", y = "Angel awards", title = paste0("Yearly Angel award in *",input$aggregation, "* for *",  input$hospital, "* hospital")) +
               theme_classic() +
               scale_fill_manual(values = c("darkcyan","gold","lightgrey")) +
               if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
          
    )
  })    
  output$first <- renderPlotly({
    data <- data[data$h_name == input$hospital
                 & data$nameOfAggr == input$aggregation
                 & data$isYearAgg == FALSE
                 & data$year %in% input$YEAR
                 ,] 
    
    new_data <- data.frame(
      Category = data$YQ,
      SubCategory = data$h_name,
      Gold = data$is1stGold,
      Platinum = data$is1stPlat,
      Diamond = data$is1stDiam
    )
    long_data <- new_data %>%
      gather(key = "Award", value = "Value", Gold, Platinum, Diamond)
    
    ggplotly(ggplot(long_data, aes(x = Category, y = Value, fill = Award)) +
               geom_bar(stat = "identity", position = "stack") +
               facet_wrap(~ SubCategory) +
               labs(x= "Quarter", y = "Angel award", title = paste0("Quarterly is first award in *",input$aggregation, "* for *",  input$hospital, "* hospital")) +
               theme_classic() +
               scale_fill_manual(values = c("darkcyan","gold","lightgrey")) +
               if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
             
    )
  })
  output$first_year <- renderPlotly({
    data <- data[data$h_name == input$hospital
                 & data$nameOfAggr == input$aggregation
                 & data$isYearAgg == TRUE
                 & data$year %in% input$YEAR
                 ,] 
    clickData <- event_data("plotly_hover", source = "angel")
    new_data <- data.frame(
      Category = data$YQ,
      SubCategory = data$h_name,
      Gold = data$is1stGold,
      Platinum = data$is1stPlat,
      Diamond = data$is1stDiam
    )
    long_data <- new_data %>%
      gather(key = "Award", value = "Value", Gold, Platinum, Diamond)
    
    
    ggplotly(ggplot(long_data, aes(x = Category, y = Value, fill = Award)) +
               geom_bar(stat = "identity", position = "stack") +
               facet_wrap(~ SubCategory) +
               labs(x= "Year", y = "Angel awards", title = paste0("Yearly first award in *",input$aggregation, "* for *",  input$hospital, "* hospital")) +
               theme_classic() +
               scale_fill_manual(values = c("darkcyan","gold","lightgrey")) +
               if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
             
    )
  }) 
  output$country <- renderPlotly({
    data <- data[data$h_name == input$hospital
                 & data$nameOfAggr == input$aggregation
                 & data$isYearAgg == FALSE
                 & data$year %in% input$YEAR
                 ,] 
    ggplotly(ggplot(data, aes(x = YQ, y = CSoAboveCountry, fill = Variable)) +
               geom_bar(stat = "identity", position = "stack") +
               facet_wrap(~ SubCategory) +
               labs(x= "Quarter", y = "Angel award", title = paste0("Quarterly is first award in *",input$aggregation, "* for *",  input$hospital, "* in selected year")) +
               theme_classic() +
               scale_fill_manual(values = c("darkcyan","gold","lightgrey")) +
               if (input$Color == TRUE){scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))}
             
    )
  })
  output$KPI_Hospital_Click <- renderPlotly({
    clickData <- event_data("plotly_hover", source = "scatter")

    ggplotly(ggplot(data = clickData  , aes(x=x,y=y)) + geom_point() +
             labs() + theme_classic(),
    
             )
  })
  output$missing <- renderPlotly({
    data <- data[data$isYearAgg == TRUE
                 & data$isAngelKPI == TRUE
                 & data$nameOfAggr == input$aggregation
                 & data$year %in% input$YEAR
                 & data$h_name == input$hospital
                 & !is.na(data$subGroup)
                 ,] 
    ggplotly(
      ggplot(data = data, aes(x=year, y = pct_missing, fill = subGroup, size = 2)) +
        geom_point() + 
        labs(x = "Year", y ="Missing data %") + 
        theme_classic() 
  
    , source = "missing_data")
})
  output$missing_click <- renderPlotly({
    clickData <- event_data("plotly_selected", source = "missing_data")
    if (is.null(clickData)) return(NULL)
    ggplotly(
      ggplot(data = clickData  , aes(x=x,y=y)) + 
        geom_smooth(method = "lm") + 
        geom_point() +
        labs( x = "Year", y ="Amount of missing data points", title = "Select data from plot above to analyse.") + 
        theme_classic(),
             
    )
  })

  output$country_value <- renderPlotly({
    data <- data[data$isYearAgg == FALSE
                 & data$isAngelKPI == TRUE
                 & data$nameOfAggr == input$aggregation
                 & data$year %in% input$YEAR
                 & data$aggFunc == "pct"
                 & data$h_name == input$hospital
                 & !is.na(data$subGroup)
                 ,] 
    ggplotly(
      ggplot(data = data, aes(x=YQ, y = Value, fill = subGroup, size = 2)) +
        geom_point() + 
        labs(x = "Quarters", y ="Value %") + 
        theme_classic() 
      
  ,source = "value_data")
  })
  output$country_value_click <- renderPlotly({
    clickData <- event_data("plotly_selected", source = "value_data")
    if (is.null(clickData)) return(NULL)
    ggplotly(
      ggplot(data = clickData  , aes(x=x,y=y)) + 
        geom_smooth(method = "lm") + 
        geom_point() +
        labs( x = "Quarters", y ="Value %", title = "Select data from plot above to analyse.") + 
        theme_classic(),
      
    )
  })
}
# ;) ----------------------------------------------------------------------


shinyApp(ui, server) #Dashboard








