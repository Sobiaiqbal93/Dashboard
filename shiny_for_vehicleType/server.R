library(tidyverse)
library(readxl)
library(shiny)
library(ggplot2)
Cali_data=read_excel("datasets.xlsx", sheet = "CaliforniaQ1") 
auto_sum_sale=Cali_data %>% mutate(sum_sale = rowSums(.[4:8]))

shinyServer(function(input,output,session){
 output$ggplot_vehicle_axes=renderPlot({
   #as.numeric(c(input$x_attribute,input$y_attribute,input$y_attribute,input$Type))
   if(input$type_veh == "All Types" & input$year_veh == "Whole Time Period"){
      data_plot <- auto_sum_sale %>% group_by(Type) %>% summarize(sum_sale=sum(sum_sale)) %>% arrange()
      names(data_plot$sum_sale) <- data_plot$Type
      barplot(data_plot$sum_sale, main = "Vehical Sale", xlab = "Vehicle Type", ylab="Aggergated Sale")
      
   }else if(input$type_veh != "All Types" & input$year_veh == "Whole Time Period"){
      data_subset <- subset(Cali_data, Type==input$type_veh)[4:8]
      data_plot <- apply(data_subset, 2, sum) 
      names(data_plot) <- names(data_plot)
      barplot(data_plot, main = paste0("Vehical Sale of ", toString(input$type_veh)), xlab = "Year", ylab="Aggergated Sale")
      
   }else if(input$type_veh == "All Types" & input$year_veh != "Whole Time Period"){
      data_subset <- select(auto_sum_sale, c(input$year_veh, "Type")) 
      data_plot <- data_subset %>% group_by(Type) %>% summarise(sum_sale=sum(!!sym(input$year_veh))) 
      names(data_plot$sum_sale) <- data_plot$Type
      barplot(data_plot$sum_sale, main = paste0("Vehical Sale of ", toString(input$type_veh)), xlab = "Vehicle Type", ylab="Aggergated Sale")
      
   }else if(input$type_veh != "All Types" & input$year_veh != "Whole Time Period"){
      data_subset_type <- subset(Cali_data, Type==input$type_veh)[c(1,4:8)]
      data_subset_year <- select(data_subset_type, c(input$year_veh, "Type")) 
      data_plot <- data_subset_year %>% group_by(Type) %>% summarize(sum_sale=sum(!!sym(input$year_veh)))
      names(data_plot$sum_sale) <- data_plot$Type
      barplot(data_plot$sum_sale, main = "Vehical Sale", xlab = "Vehicle Type", ylab="Aggergated Sale")
      
   }
   
    
   #ggplot(data_plot, aes(x=Type,y=sum_sale))+
#     geom_bar (position = "stack", stat ="identity")+
   #  theme_minimal()+
   #    labs(fill="Maker/Brand",x="Vehicle",y="Sale Sum" ,title="Visualization of Popular Brand")
 }
 )
})