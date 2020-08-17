library(dmm) #need to run this before tidyverse 
library(Hmisc)
library(magrittr)
library(xlsx)
library(tidyverse)

source("Cleaning_Function.R")


#replace the file below with the path/file name of the new file 
weekly_covid_data <- read.csv("Data/TOWN_CovidCases_20200729_modified.csv")

#replace with the number of weeks for which there is data on weekly cases  
#for example, 4/14 - 7/29 is 16 weeks 
numWeeks = 16  

#change the first argument with the most recent date of the weekly cases data 
upDate = as.Date("07/29", "%m/%d") 

#add to the end of this list, 
#the most recent dates in the for the weekly cases and weekly rates respectively 
weekly_cases_dates <- c("4/14", "4/22", "4/29", "5/6", "5/13", "5/20", "5/27", "6/3", "6/10",
                        "6/17", "6/24", "7/1", "7/8", "7/15", "7/22", "7/29")
weekly_rates_dates <- c("4/14", "4/22", "4/29", "5/6", "5/13", "5/20", "5/27", "6/3", "6/10",
                        "6/17", "6/24", "7/1", "7/8")

#call the cleaning function - no need to change anything here 
x <- cleaning_function(weekly_covid_data, numWeeks, upDate, 
                       weekly_cases_dates, weekly_rates_dates)


#### Save all Tables #### 
#saves the data to an excel file 
#can change the path/name and sheet name in file and sheetName arguments respectively 
#however, all changes are optional 
write.xlsx(x[[1]], file="Data/clean_data.xlsx", sheetName = "40 Town Cases,Deaths")
write.xlsx(x[[2]], file="Data/clean_data.xlsx", sheetName="40 Town Pop,Min,Eng", append=TRUE)
write.xlsx(x[[3]], file="Data/clean_data.xlsx", sheetName="351 Town Census,Covid", append=TRUE)
write.xlsx(x[[4]], file="Data/clean_data.xlsx", sheetName="TS Daily Cases,Deaths", append=TRUE)
write.xlsx(x[[5]], file="Data/clean_data.xlsx", sheetName="TS Weekly Conf,Rates", append=TRUE)
write.xlsx(x[[6]], file="Data/clean_data.xlsx", sheetName="Total CS, Census", append=TRUE)























