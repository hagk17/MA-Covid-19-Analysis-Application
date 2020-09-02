library(dmm) #need to run this before tidyverse 
library(Hmisc)
library(magrittr)
library(xlsx)
library(tidyverse)

source("Cleaning_Function.R")


#replace the file below with the path/file name of the new file 
#this file should have weekly covid data from Mass DPH
weekly_covid_data <- read.csv("Raw_Data/TOWN_CovidCases_20200819.csv")

#change the first argument with the most recent date of the weekly cases data 
upDate = as.Date("08/19", "%m/%d") 

#add to the end of this list, 
#the most recent dates in the for the weekly cases and weekly rates respectively 
weekly_cases_dates <- c("4/14", "4/22", "4/29", "5/6", "5/13", "5/20", "5/27", "6/3", "6/10",
                        "6/17", "6/24", "7/1", "7/8", "7/15", "7/22", "7/29", "08/05", "8/12",
                        "8/19")
weekly_rates_dates <- c("4/14", "4/22", "4/29", "5/6", "5/13", "5/20", "5/27", "6/3", "6/10",
                        "6/17", "6/24", "7/1", "7/8")


#call the cleaning function - no need to change anything here 
x <- cleaning_function(weekly_covid_data, upDate, 
                       weekly_cases_dates, weekly_rates_dates)


#### Save all Tables #### 
#saves the data to an excel file 
#can change the path/name and sheet name in file and sheetName arguments respectively 
#however, all changes are optional 
write.xlsx(x[[1]], file="clean_0819_data.xlsx", sheetName = "40 Town Cases,Deaths")
write.xlsx(x[[2]], file="clean_0819_data.xlsx", sheetName="40 Town Pop,Min,Eng", append=TRUE)
write.xlsx(x[[3]], file="clean_0819_data.xlsx", sheetName="351 Town Census,Covid", append=TRUE)
write.xlsx(x[[4]], file="clean_0819_data.xlsx", sheetName="TS Daily Cases,Deaths", append=TRUE)
write.xlsx(x[[5]], file="clean_0819_data.xlsx", sheetName="TS Weekly Conf,Rates", append=TRUE)
write.xlsx(x[[6]], file="clean_0819_data.xlsx", sheetName="Total CS, Census", append=TRUE)

# data specifically saved to be used in the application 
write.xlsx(x[[4]], file="app_0819_data.xlsx", sheetName="TS Daily Cases,Deaths", append=TRUE)
write.xlsx(x[[5]], file="app_0819_data.xlsx", sheetName="TS Weekly Conf,Rates", append=TRUE)
write.xlsx(x[[6]], file="app_0819_data.xlsx", sheetName="Total CS, Census", append=TRUE)





















