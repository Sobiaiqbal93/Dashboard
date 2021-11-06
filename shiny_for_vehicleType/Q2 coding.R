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

#---------------Function for Q1
#---------------germany
auto_sum_Gsale=germany_data %>% mutate(sum_sale = rowSums(.[4:13]))
function_for_Vehicle=function(Data_for_func){
  automobile_types_G=auto_sum_Gsale %>% group_by(Type) %>% summarize(sum_sale=sum(sum_sale)) %>% arrange()
  return(automobile_types_G)
}
function_for_Vehicle(germany_data)
#------Table
automobile_Types_G=auto_sum_Gsale %>% group_by(Type) %>%summarize(sum(sum_sale))
table.G=datatable(automobile_Types_G, filter = "top", 
                        options = list(pageLength = 10))



#---------------denmark
auto_sum_dsale=denmark_data %>% mutate(sum_sale = rowSums(.[4:13]))
function_for_Vehicle=function(Data_for_func){
  automobile_types_d=auto_sum_dsale %>% group_by(Type) %>% summarize(sum_sale=sum(sum_sale)) %>% arrange()
  return(automobile_types_d)
}
function_for_Vehicle(denmark_data)
#------Table
automobile_Types_d=auto_sum_dsale %>% group_by(Type) %>%summarize(sum(sum_sale))
table.d=datatable(automobile_Types_d, filter = "top", 
                  options = list(pageLength = 10))
table.d



#---------------Vietnam
auto_sum_Vsale=Viet_data %>% mutate(sum_sale = rowSums(.[4:13]))
function_for_Vehicle=function(Data_for_func){
  automobile_types_V=auto_sum_Vsale %>% group_by(Type) %>% summarize(sum_sale=sum(sum_sale)) %>% arrange()
  return(automobile_types_V)
}
function_for_Vehicle(Viet_data)
#------Table
automobile_Types_V=auto_sum_Vsale %>% group_by(Type) %>%summarize(sum(sum_sale))
table.V=datatable(automobile_Types_V, filter = "top", 
                  options = list(pageLength = 10))
table.V


#---------------California
auto_sum_Csale=Cali_data %>% mutate(sum_sale = rowSums(.[4:8]))
function_for_Vehicle=function(Data_for_func){
  automobile_types_C=auto_sum_Csale %>% group_by(Type) %>% summarize(sum_sale=sum(sum_sale)) %>% arrange()
  return(automobile_types_C)
}
function_for_Vehicle(Cali_data)
#------Table
automobile_Types_C=auto_sum_Csale %>% group_by(Type) %>%summarize(sum(sum_sale))
table.C=datatable(automobile_Types_C, filter = "top", 
                  options = list(pageLength = 10))
table.C



#---------------Ethopia
auto_sum_Esale=Ethopia_data %>% mutate(sum_sale = rowSums(.[3:11]))
function_for_Vehicle=function(Data_for_func){
  automobile_types_E=auto_sum_Esale %>% group_by(Type) %>% summarize(sum_sale=sum(sum_sale)) %>% arrange()
  return(automobile_types_E)
}
function_for_Vehicle(Ethopia_data)
#------Table
automobile_Types_E=auto_sum_Esale %>% group_by(Type) %>%summarize(sum(sum_sale))
table.E=datatable(automobile_Types_E, filter = "top", 
                  options = list(pageLength = 10))
table.E
