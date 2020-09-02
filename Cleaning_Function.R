library(dmm) #need to run this before tidyverse 
library(Hmisc)
library(magrittr)
library(xlsx)
library(tidyverse)



#### Quick Function to appropirately Capitalize Names ####
# source code borrowed from StackOverflow: 
#https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string

simpleCap <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#### Data Cleaning Function ####

#data considered: 
  #Weekly Data from the Massachuessets Department of Public Health on 
  #the cumulative count and rate (per 100,000) of confirmed COVID-19 cases 
  #in 351 Massachusetts cities/towns from starting from January 1, 2020. 

  #Daily Data from the Massachuessets Department of Public Health on 
  #the cumulative daily cases and deaths for 40 Massachusetts cities/towns 
  #from March 23, 2020 to June 1, 2020.

  #Census tract data from the American Community Survey on 
  #various socio-economic aspects of the 351 towns in Massachusetts

#inputs: 
  #town_covid = variable that has read in data file containing weekly cases 
        #****not the name of the actual file****
  #numberofWeeks = integer variable of the number of weeks for which there is data on weekly cases
  #latest_date = date variable of the most recent date of the weekly cases data 
        #example: 07/29 
  #newWeeks_cases = vector of strings that lists the dates of available data for the weekly cases 
        #example: weekly_cases_dates <- c("4/14", "4/22", "4/29", "5/6", "5/13", "5/20")
  #newWeeks_rates = vector of strings that lists the dates of available data for the weekly rates
        #example: weekly_rates_dates <- c("4/14", "4/22", "4/29", "5/6")
#outputs: 
  #list of 6 dataframes to be stored in an excel spreadsheet 
    #first: aggregating daily cases and death data into one table 
    #second: 40 towns' population, min, eng gathered from daily data files 
    #third: calculated totals and proportions for town level socio-economic data, joined with second dataframe 
    #fourth: time series representation of first dataframe 
    #fifth: time series representation of covid data from third data frame 
    #sixth: town level socio-economic data combined with most recent total covid cases 


cleaning_function <- function(town_covid, latest_date, 
                              newWeeks_cases, newWeeks_rates){

  
  #### Reads in  Additional Data ####
  ttown_linkage <- read.csv("Raw_Data/CT_town_linkage.csv")
  census <- read.csv("Raw_Data/CT_ACS2014_2018_vars.csv")
  town_density <- read.csv("Raw_Data/TOWN_pop_density_sqmi.csv")
  daily_cases <- read.csv("Raw_Data/daily_cases.csv")
  daily_deaths <- read.csv("Raw_Data/daily_deaths.csv")
  
  
  #### Inital Cleaning of Linkage ####
  
  #makes all the towns corretly capitalized 
  town_linkage$TOWN <- sapply(town_linkage$TOWN, simpleCap)
  town_linkage %<>% rename(Town = TOWN)
  
 
  #### Collapsing the Census Data ####
  
  # lets join
  full <- left_join(census, town_linkage, by = "GEOID10")
  full %<>% select(GEOID10, Town, TOWN_ID, everything()) %>% 
    group_by(Town) %>% arrange(Town) %>%  na_if(-999)
  
  #gets summations for each town and recalculates proportions 
  countData <- full[,c(1:14, 22:24, 26:28, 42, 47, 51, 52)]
  countProp <- countData %>% group_by(Town)%>%
    summarise(n_pop = sum(N_POP), n_fam = sum(N_FAM), 
              n_house = sum(N_HOUSE), n_empciv = sum(N_EMPCIV), 
              n_200fpl = sum(n_200FPL), n_400fpl = sum(n_400FPL),
              n_black = sum(n_black), n_famChild = sum(n_famchild), 
              n_industry = sum(n_industry), n_rentCost = sum(n_rentcost), 
              n_noIntern = sum(n_noIntern), n_age65 = sum(n_age65),
              n_minority = sum(n_minrty), n_sngpnt = sum(n_sngpnt), 
              n_disability = sum(n_disabl), n_limEng = sum(n_limeng), 
              n_occUnits = sum(n_occ_units), n_withChildren = sum(n_withchildren),
              n_occUnitsZero = sum(n_occunits_equaltozero), n_employed = sum(n_employed), 
              n_age17 = sum(E_AGE17)) %>%
    mutate(p_fam = n_fam/n_pop, p_house = n_house/n_pop, 
           p_empciv = n_empciv/n_pop, p_200fpl = n_200fpl/n_pop, 
           p_400fpl = n_400fpl/n_pop, p_black = n_black/n_pop,
           p_famChild = n_famChild/n_pop, p_industry = n_industry/n_pop, 
           p_rentCost = n_rentCost/n_pop, p_noIntern = n_noIntern/n_pop, 
           p_age65 = n_age65/n_pop, p_minority = n_minority/n_pop, 
           p_sngpnt = n_sngpnt/n_pop, p_disability = n_disability/n_pop, 
           p_limEng = n_limEng/n_pop, p_occUnits = n_occUnits/n_pop, 
           p_withChildren = n_withChildren/n_pop, p_occUnitsZero = n_occUnitsZero/n_pop, 
           p_employed = n_employed/n_pop, p_age17 = n_age17/n_pop) %>%
    select(Town, n_pop, 
           n_fam, p_fam, 
           n_house, p_house, 
           n_empciv, p_empciv, 
           n_200fpl, p_200fpl,
           n_400fpl, p_400fpl,
           n_black, p_black,
           n_minority, p_minority,
           n_age17, p_age17,
           n_age65, p_age65,
           n_disability, p_disability,
           n_famChild, p_famChild,
           n_withChildren, p_withChildren,
           n_sngpnt, p_sngpnt,
           n_limEng, p_limEng,
           n_employed, p_employed,
           n_industry, p_industry,
           n_noIntern, p_noIntern,
           n_rentCost, p_rentCost, 
           n_occUnits, p_occUnits,
           n_occUnitsZero, p_occUnitsZero)
  
  # recalculates data where there's only precentages 
  reverseEngData <- full[,c(1:4,41,43:46,49,50,53)]
  
  newReverse <- reverseEngData %>% 
    mutate(n_asthma = (p_asthma/100)*N_POP, n_kitchenInc = (perc_kitchen_inc/100)*N_POP,
           n_plumbInc = (perc_plumb_inc/100)*N_POP, n_renter = (perc_renter/100)*N_POP,
           n_vacant = (perc_vacant/100)*N_POP, n_SNAP = (perc_foodstamps_snap/100)*N_POP,
           n_familySNAP = (perc_family_foodstamps_snap/100)*N_POP, 
           n_service = (perc_service/100)*N_POP) %>%
    group_by(Town) %>%
    summarize(n_pop = sum(N_POP), n_asthma = sum(n_asthma), 
              n_kitchenInc = sum(n_kitchenInc), n_plumbInc = sum(n_plumbInc), 
              n_renter = sum(n_renter), n_vacant = sum(n_vacant), 
              n_SNAP = sum(n_SNAP), n_familySNAP = sum(n_familySNAP), 
              n_service = sum(n_service)) %>% 
    mutate(p_asthma = n_asthma/n_pop, p_kitchenInc = n_kitchenInc/n_pop, 
           p_plumbInc = n_plumbInc/n_pop, p_renter = n_renter/n_pop, 
           p_vacant = n_vacant/n_pop, p_SNAP = n_SNAP/n_pop, 
           p_familySNAP = n_familySNAP/n_pop, p_service = n_service) %>%
    select(Town,
           n_asthma, p_asthma,
           n_kitchenInc, p_kitchenInc,
           n_plumbInc, p_plumbInc,
           n_renter, p_renter,
           n_vacant, p_vacant,
           n_service, p_service,
           n_SNAP, p_SNAP,
           n_familySNAP, p_familySNAP)
  
  newCensus <- left_join(countProp, newReverse)
  
  
  
  #### Initial Cleaning of Population Density ####
  
  #make town lowercase 
  town_density$TOWN <- sapply(town_density$TOWN, simpleCap)
  
  #rename and select just population density 
  mini_town_density <- town_density %>% rename(Pop_Density = popden19, Town = TOWN) %>%
    select(Town, Pop_Density)
  
  #join to census by town 
  newCensus <- left_join(newCensus, mini_town_density)
  
  
  
  
  
  
  
  #### Initial Cleaning of Weekly Town Covid Cases #### 
  
  #grab some counts of columns we want 
  numberofWeeks = length(newWeeks_cases)
  numberofRates = length(newWeeks_rates)
  tossCols = ncol(town_covid)-1-numberofWeeks-numberofRates

  
  #makes all the towns corretly capitalized 
  town_covid$Town <- sapply(town_covid$Town, simpleCap)
  
  alpha_town_covid <- town_covid[ , order(names(town_covid))]
  new_town_covid <- alpha_town_covid[,-c(1:tossCols)] 

  
  # reorder a bit and replace -999 with NA 
  new_town_covid <- new_town_covid%>% 
    group_by(Town) %>% arrange(Town) %>%  na_if(-999) %>%
    dplyr::select(Town, everything())
  
  #replace <5 with 2.5 
  new_town_covid[new_town_covid =="<5"] <- "2.5"
  new_town_covid2 <- as.data.frame(sapply(new_town_covid[2:ncol(new_town_covid)], as.double))
  
  full_town <- cbind(new_town_covid$Town, new_town_covid2)
  full_town_covid <- full_town %>% rename(Town = `new_town_covid$Town`)
  
  
  
  #### Initial Cleaning of Daily Cases #### 
  
  daily_cases <-  daily_cases %>% rename(Town = X)
  daily_deaths <-  daily_deaths %>% rename(Town = X)
  
  ##### ------ Table 1: Full Daily Data -----  ####
  
  #towns, cases, 74 dates, town ids 
  #need: daily_deaths, daily_cases, town_linkage 
  
  #first lets fix these dates 
  dateNames <- c(paste("Mar", 23:31, sep = "_"), paste("April", 1:30, sep = "_"),
                 paste("May", 1:31, sep = "_"), "June_1")
  colnames(daily_cases) <- c("Town", dateNames, "Population", "Min", "Eng")
  colnames(daily_deaths) <-c("Town", dateNames, "Population", "Min", "Eng")
  
  #join two data sets together 
  full_daily <- left_join(daily_cases[1:72], daily_deaths[1:72], by = "Town", suffix = c(".Cases", ".Deaths"))
  
  full_daily$June_1.Deaths[39] <- "6894"
  full_daily$June_1.Deaths <- as.integer(full_daily$June_1.Deaths)
  
  #now we can make longer 
  full_daily <- pivot_longer(full_daily, cols = -1) 
  
  #split the names so we can tell the difference between cases and deaths 
  savedEntries <- rep(NA, length(full_daily$name))
  for(i in 1:length(full_daily$name)){
    splitEntry <- scan(what = "", text = full_daily$name[i], sep = ".")
    savedEntries[i] <- splitEntry[2]
    full_daily$name[i] <- splitEntry[1]
  }
  full_daily <- cbind(full_daily, savedEntries)
  
  # and now make wider again 
  full_daily <- pivot_wider(full_daily, names_from = name, values_from = value)
  
  #add the town ids for the towns that are there 
  town_intersect <- intersect(full_daily$Town, town_linkage$Town)
  smallLink <- town_linkage %>% dplyr::select(Town, TOWN_ID) %>%
    filter(Town %in% town_intersect) %>%
    unique()
  
  newFull <- left_join(full_daily, smallLink, by = "Town")
  table1 <- newFull %>% dplyr::select(Town, TOWN_ID, everything()) %>% 
    rename(Cases_Deaths = savedEntries)
  
  
  ##### ------ Table 2: Daily with pop,min,eng -----  ####
  
  table2 <- left_join(daily_cases[c(1,73:75)], smallLink, by = "Town")
  table2 <- table2 %>% select(Town, TOWN_ID, everything()) 
  
  
  ##### ------ Table 3: Weekly Cases/Rates and Town Census  ----- ####
  table3 <- left_join(newCensus, full_town_covid, by = "Town")
  
  #### ------ Table 4: Time Series Set Up of Table 1 ----- ####
  
  # gonna need to pivot longer to make it time series =( 
  daily40_ts <- pivot_longer(table1, 4:74, names_to = "Time", values_to = "Count")
  daily40_ts %<>% group_by(Town) %>% arrange(Town)
  
  # and need to make the dates actual dates 
  newDates <- c(paste("3", 23:31, sep = "/"), paste("4", 1:30, sep = "/"),
                paste("5", 1:31, sep = "/"), "6/1")
  oldDates <- unique(daily40_ts$Time)
  for(i in 1:length(daily40_ts$Time)){
    daily40_ts$Time[daily40_ts$Time == oldDates[i]] <- newDates[i]
  }
  table4 <- daily40_ts
  
  
  
  
  ##### ------ Table 5: Time Series Set Up of Weekly Cases----- ####
  
  weekly_cases <- full_town_covid[,c(1:numberofWeeks, numberofWeeks+1)]
  weekly_rates <- full_town_covid[,c(1, (numberofWeeks+2):ncol(full_town_covid))]
  
  
  weekly_cases_ts <- pivot_longer(weekly_cases, -Town, names_to = "Time", values_to = "Count")
  weekly_rates_ts <- pivot_longer(weekly_rates, -Town, names_to = "Time", values_to = "Count")
  
  oldWeeksConf <- unique(weekly_cases_ts$Time)
  oldWeeksRates <- unique(weekly_rates_ts$Time) #lengths not necessarily the same 
  
  
  for(i in 1:length(weekly_cases_ts$Time)){
    weekly_cases_ts$Time[weekly_cases_ts$Time == oldWeeksConf[i]] <- newWeeks_cases[i]
  }
  
  for(j in 1:length(weekly_rates_ts$Time)){
    weekly_rates_ts$Time[weekly_rates_ts$Time == oldWeeksRates[j]] <- newWeeks_rates[j]
  }
  
  weekly_cases_ts$Time <- as.Date(weekly_cases_ts$Time, "%m/%d")
  weekly_rates_ts$Time <- as.Date(weekly_rates_ts$Time, "%m/%d")
  
  Type = rep("Conf", nrow(weekly_cases_ts))
  weekly_cases_ts <- cbind(weekly_cases_ts, Type)
  Type = rep("Rate", nrow(weekly_rates_ts))
  weekly_rates_ts <- cbind(weekly_rates_ts, Type)
  
  table5 <- rbind(weekly_cases_ts, weekly_rates_ts)
  
  
  #### ----- Table 6: Table 3 with Conf Cases Summation ----- #####
  
  new5 <- table5 %>% filter(Type == "Conf") %>% filter(Time == latest_date)
  t6 <- left_join(new5, newCensus, by = "Town")
  
  #add a column for the proportion of cases per population
  table6 <- t6 %>% mutate(p_cases = Count/n_pop) %>%
    rename(Total_Cases = "Count") %>%
    dplyr::select(-Time, -Type) %>%
    dplyr::select(Town, Total_Cases, p_cases, n_pop, Pop_Density,
                  everything())
  
  
 all_data_frames <- list(table1, table2, table3, as.data.frame(table4), table5, table6)
 
 return(all_data_frames)
  
} #end of function 











