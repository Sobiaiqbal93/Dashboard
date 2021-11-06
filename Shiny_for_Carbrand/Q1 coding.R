library(DT)
library(dplyr)
library(tidyverse)
library(readxl)


denmark_data=read_excel("datasets.xlsx",sheet = "DenmarkQ1")
germany_data=read_excel("datasets.xlsx",sheet = "GermanyQ1")
Viet_data=read_excel("datasets.xlsx",sheet = "VietnamQ1")
Cali_data=read_excel("datasets.xlsx",sheet = "CaliforniaQ1")
Ethopia_data=read_excel("datasets.xlsx",sheet = "EthopiaQ1")
datatable(denmark_data, filter = "top", 
          options = list(pageLength = 5))



# Function to read file

#-------------DENMARK

#Q no 1--------Which brand is sales (fron each type of Vehicles)
auto_sum_sale=denmark_data %>% mutate(sum_sale = rowSums(.[4:13]))
function_for_brand=function(Data_for_func){
  automobile_brand=auto_sum_sale %>% 
    group_by(Brand) %>%
    summarize(sum(sum_sale))
  return(automobile_brand)
}

sol_for_brand=function_for_brand(denmark_data)
#------Table
automobile_brand=auto_sum_sale %>% group_by(Brand) %>%summarize(sum(sum_sale))
sorting.data=automobile_brand%>% top_n(10, `sum(sum_sale)`)
table_denmark=datatable(sorting.data, filter = "top", 
          options = list(pageLength = 10))


#-------------Germany

#Q no 1--------Which brand is sales (fron each type of Vehicles)
auto_sum_sale_G=germany_data%>% mutate(sum_sale = rowSums(.[4:13]))
function_for_brand=function(Data_for_func){
  automobile_brand=auto_sum_sale_G %>% 
    group_by(Brand) %>%
    summarize(sum(sum_sale)) %>% arrange()
  return(automobile_brand)
}
function_for_brand(germany_data)

#------Table
automobile_brand_G=auto_sum_sale_G %>% group_by(Brand) %>%summarize(sum(sum_sale))
sorting.data=automobile_brand_G%>% top_n(10, `sum(sum_sale)`)
table_Germany=datatable(sorting.data, filter = "top", 
                        options = list(pageLength = 10))
#-------------Vietnam

#Q no 1--------Which brand is sales (fron each type of Vehicles)
auto_sum_sale_Vit=Viet_data %>% mutate(sum_sale = rowSums(.[4:13]))
function_for_brand=function(Data_for_func){
  automobile_brand=auto_sum_sale_Vit %>% 
    group_by(Brand) %>%
    summarize(sum(sum_sale)) %>% arrange()
  return(automobile_brand)
}
function_for_brand( Viet_data)

#------Table
automobile_brand_vit=auto_sum_sale_Vit %>% group_by(Brand) %>%summarize(sum(sum_sale))
sorting.data=automobile_brand_vit%>% top_n(10, `sum(sum_sale)`)
table_vit=datatable(sorting.data, filter = "top", 
                        options = list(pageLength = 10))

#-------------California

#Q no 1--------Which brand is sales (fron each type of Vehicles)
auto_sum_sale_Cali=Cali_data %>% mutate(sum_sale = rowSums(.[4:13]))
function_for_brand=function(Data_for_func){
  automobile_brand=auto_sum_sale %>% 
    group_by(Brand) %>%
    summarize(sum(sum_sale)) %>% arrange()
  return(automobile_brand)
}
function_for_brand(Cali_data)
#------Table
automobile_brand_Cali=auto_sum_sale_Vit %>% group_by(Brand) %>%summarize(sum(sum_sale))
sorting.data=automobile_brand_Cali%>% top_n(10, `sum(sum_sale)`)
table_Cali=datatable(sorting.data, filter = "top", 
                    options = list(pageLength = 10))
