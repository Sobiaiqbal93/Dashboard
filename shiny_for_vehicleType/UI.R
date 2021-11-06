library(tidyverse)
library(readxl)
library(shiny)
library(ggplot2)
Cali_data=read_excel("datasets.xlsx", sheet = "CaliforniaQ1") 
auto_sum_sale=Cali_data %>% mutate(sum_sale = rowSums(.[4:8]))
automobile_types=auto_sum_sale %>%group_by(Type) %>% summarize(sum(sum_sale)) %>% arrange()
shinyUI(fluidPage(
  titlePanel("Types of Vehicles"),
  sidebarLayout(
    sidebarPanel(("Enter the type of vehicle"),
                 selectInput(inputId = "type_veh",label = "Vehicle Type",c("All Types", automobile_types$Type)),
                 selectInput(inputId = "year_veh",label = "Vehicle Year",c("Whole Time Period", names(auto_sum_sale[1,4:13])))
    ),
    mainPanel(("Sale of Vehicle During Whole Period"),
              tabsetPanel( 
                tabPanel("bar_ggplot",
                         plotOutput(outputId = "ggplot_vehicle_axes")),
                tabPanel("message",br(),
                         p(code(".data[[input$var]]")))
                
              )
    )
  )
)
)
