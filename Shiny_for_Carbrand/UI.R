library(tidyverse)
library(readxl)
library(shiny)
library(ggplot2)
Cali_data=read_excel("datasets.xlsx", sheet = "CaliforniaQ1") 
auto_sum_sale=Cali_data %>% mutate(sum_sale = rowSums(.[4:8]))%>%arrange()
automobile_Brand=auto_sum_sale%>% group_by(Brand)%>%summarize(sum(sum_sale))
shinyUI(fluidPage(
  titlePanel("Most Favourite Brand"),
  sidebarLayout(
  sidebarPanel(("Enter the Brand Name"),
       selectInput(inputId =  "veh_brand",label ="Brand Name",c("mostly sale brands",automobile_Brand$Brand)),
       selectInput(inputId = "year_vehicle",label = "Vehicle year",c("Whole Time period",names(auto_sum_sale[1,4:14]))),
  ),
  mainPanel(("Sale of  Brands during whole period"),
            tabsetPanel( 
              tabPanel("bar_ggplot",
                                  plotOutput(outputId = "ggplot_Brand_axes")),
                         tabPanel("message",br(),
                                  p(code(".data[[input$var]]")))
                     
  )
  )
)
)
)
