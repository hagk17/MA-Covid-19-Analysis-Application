# MA-Covid-19-Analysis-Application

## Cleaning_Function.R
Assistant function to read, clean, aggregate, and organize the raw data to prepare for use in the application. Outputs a list of 6 data frames that can be stored in an excel spreadsheet as specified in Code_to_Clean_Data.R. The 6 data frames are as followed:

	1: Aggregating daily cases and death data into one table
	2: 40 towns' population, min, eng gathered from daily data files 
	3: Calculated totals and proportions for town level socio-economic data, joined with second dataframe 
	4: Time series representation of first dataframe 
	5: Time series representation of covid data from third data frame 
	6: Town level socio-economic data combined with most recent total covid cases 
	
Unless there are changes to the input, there is no need to edit this file. 

## Code_to_Clean_Data.R
R code that runs the Cleaning_Function and allows for input to update weekly covid cases data. Last updated with data from August 19, 2020. 
Outputs:

	clean_0819_data.xlsx - Excel doc with sheets for each of the data frames created by Cleaning_Function.R (not necessary for the application) 
	app_0819_data.xlsx - Excel doc with for the 4-6 dataframes created by Cleaning_Function.R This is necessary for the application and must be in the same directory as fullApp.R

## app_0819_data.xlsx 
This is the data that is needed to run the application. Last updated with data from August 19, 2020. 

	Sheet 1:  Time series representation of daily cases and deaths 
	Sheet 2: Time series representation of weekly covid data regarding confirmed cases and rates. 
	Sheet 3: Town level socio-economic data combined with most recent total covid cases 

## fullApp.R
Code to run the full application. Last updated with data from August 19, 2020. 

## rsconnect/documents/fullApp.R/shinyapps.io/haglichk
This file assists with the publishing of the app - right now to haglichk on shinyapps.io. There is no need to edit this file, and a new one will be created if/when this app is republished to a different url.  

## Raw_Data
This file contains the raw data used for the application. To update the application, place any new raw data into this folder. 

### CT_ACS2014_2018_vars.csv
Census tract level vulnerability data for Massachusetts with variables from ACS 2014-2018. 

### CT_TOWN_linkage.csv
Linkage file between census tract ID and town ID/name for Massachusetts

### TOWN_CovidCases_20200819.csv
Weekly confirmed COVID infections and infection rate for all Massachusetts towns from Jan 1, 2020 to August 19, 2020

	Conf<DATE>: Covid19 case count by town. For populations <50,000, <5 cases are reported as such or suppressed for confidentiality purposes. 

	CRate<Date>: Case rate by town 

	Case<DATE>: Covid19 case count by town (similar to Conf<DATE> except changing <5 to -999). 

### TOWN_pop_density_sqmi.csv
Population, area in square miles, and population density for all Massachusetts towns 

### daily_cases.csv
Daily confirmed COVID cases from March 23 to June 1 for 40 towns 

### daily_deaths.csv 
Daily confirmed COVID deaths from March 23 to June 1 for 40 towns 	

