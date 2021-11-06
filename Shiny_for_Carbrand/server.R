library(tidyverse)
library(readxl)
library(shiny)
library(ggplot2)
Cali_data=read_excel("datasets.xlsx", sheet = "CaliforniaQ1") 
auto_sum_sale=Cali_data %>% mutate(sum_sale = rowSums(.[4:8]))%>%arrange()
shinyServer(function(input,output,session){
 output$ggplot_Brand_axes=renderPlot({
   if(input$veh_brand=="mostly sale brands" & input$year_vehicle=="Whole Time period"){
     data_plot= auto_sum_sale%>% group_by(Brand)%>%summarize(sum_sale=sum(sum_sale))%>%arrange()
     names(data_plot$sum_sale)=data_plot$Brand
     
    barplot( tail(sort(data_plot$sum_sale), n=10) ,main="Brand sale",xlab="Brand_Name",ylab="Aggregated Sale", cex.names=0.5)+geom_col(width = 0.5) #want to arrange data
   }else if(input$veh_brand!="mostly sale brands" & input$year_vehicle=="Whole Time period"){
     data_subset <- subset(denmark_data, Brand==input$veh_brand)[4:8]
     data_plot <- apply(data_subset, 2, sum) 
     names(data_plot) <- names(data_plot)
     barplot(tail(sort(data_plot),n=10), main = paste0("Sale of Brand ", toString(input$veh_brand)), xlab = "Year", ylab="Aggergated Sale",cex.names=0.5)+geom_col(width = 0.5)
   }else if(input$veh_brand =="mostly sale brands" & input$year_vehicle !="Whole Time period"){
     data_subset_year= select(auto_sum_sale, c(input$year_vehicle, "Brand"))
     data_plot=data_subset_year%>%group_by(Brand)%>%summarize(sum_sale=sum(!!sym(input$year_vehicle)))
     names(data_plot$sum_sale)=data_plot$Brand
     barplot(tail(sort(data_plot$sum_sale)), main = paste0("Sale of Brand",toString(input$veh_brand)),xlab="Brand Name", ylab="Aggregated Sale",cex.names=0.5)+geom_col(width = 0.5)
   }else if(input$veh_brand !="mostly sale brands" & input$year_vehicle !="Whole Time period"){
     data_subset_type=subset(denmark_data, Brand==input$veh_brand)[c(1,4:8)]
     data_subset_year= select(data_subset_type, c(input$year_vehicle, "Brand"))
     data_plot=data_subset_year%>%group_by(Brand)%>%summarize(sum_sale=sum(!!sym(input$year_vehicle)))
     names(data_plot$sum_sale)=data_plot$Brand
     barplot((data_plot$sum_sale), main = "Brand sale",xlab="Brand_Name",ylab="Aggregated Sale",cex.names=0.5)+geom_col(width = 0.2)
   }
 }
 )
})
