#### --- libraries --- ####
library(shiny)
library(tidyverse)
library(ggplot2)
library(esquisse)
library(magrittr)
library(shinydashboard)
library(operator.tools)
library(highcharter)
library(lubridate)
library(scales)

`%notin%` <- Negate(`%in%`)


#### --- input/cleaning data --- ####
##NOTE: Change with appropriate file name ## 
daily_data <- readxl::read_excel("app_0819_data.xlsx", sheet = 1)
daily_data %<>% select(-`...1`, -TOWN_ID)

weekly_data <- readxl::read_excel("app_0819_data.xlsx", sheet = 2)
weekly_data %<>% select(-`...1`)

census_data <- readxl::read_excel("app_0819_data.xlsx", sheet = 3)
census_data %<>% select(-`...1`)

### set up for daily section ###
d_town_options <- c("All Towns", unique(daily_data$Town))
d_yaxis_options <- c("Cases", "Deaths")

d_cases_data <- daily_data %>% filter(Cases_Deaths == "Cases")
d_deaths_data <- daily_data %>% filter(Cases_Deaths == "Deaths")

### set up for weekly section ###

w_town_options <- c("All Towns", unique(weekly_data$Town))
w_yaxis_options <- c("Confirmed Cases", "Rates")

w_conf_data <- weekly_data %>% filter(Type == "Conf")
w_rate_data <- weekly_data %>% filter(Type == "Rate")

w_conf_data$Time <- as.character(w_conf_data$Time)
w_rate_data$Time <- as.character(w_rate_data$Time)

### set up for census section ###

c_proper_names <- c("Town", "Total Confirmed Cases", "Proportion of Cases",
                    "Population", "Population Density",
                    "Number of Families", "Proportion of Families",
                    "Number of Households", "Proportion of Households",
                    "Number of Employed Civilians 16 Years Old and Over", "Proportion of Employed Civilians 16 Years Old and Over",
                    "Number of Families w. Income <200% FPL", "Proportion of Families w. Income <200% FPL",
                    "Number of Families w. Income <400% FPL", "Proportion of Families w. Income <400% FPL",
                    "Number of African Americans", "Proportion of African Americans",
                    "Number of Non-White Racial/Ethnic Minorities", "Proportion of Non-White Racial/Ethnic Minorities",
                    "Number of Individuals 17 Years Old and Younger", "Proportion of Individuals 17 Years Old and Younger",
                    "Number of Individuals 65 Years Old and Older", "Proportion of Individuals 65 Years Old and Older",
                    "Number of Individuals with Disabilities", "Proportion of Individuals with Disabilities",
                    "Number of Families w. Own Children < 18",
                    "Proportion of Families w. Own Children < 18",
                    "Number of Units w. Children < 18", "Proportion of Units w. Children <18",
                    "Number of Single Parent Families w. Children < 18", "Proportion of Single Parent Families w. Children < 18",
                    "Number of Families w. Limited English", "Proportion of Families w. Limited English",
                    "n_employed", "p_employed",
                    "Number of Leisure/Hospitality Workers", "Proportion of Leisure/Hospitality Workers",
                    "Number w/o Internet Subscription", "Proportion w/o Internet Subscription",
                    "Number of Households Making < $50,000 w. Rent Costing ≥ 30% Household Income",
                    "Proportion of Households Making < $50,000 w. Rent Costing ≥ 30% Household Income",
                    "Number of Occupied Housing Units", "Proportion of Occupied Housing Units",
                    "Number of Occupied Housing Units that Equal 0", "Proportion of Occupied Housing Units that Equal 0",
                    "Number of People w. Asthma", "Proportion of People w. Asthma",
                    "Number of Housing Units w. Incomplete Kitchens", "Proportion of Housing Units w. Incomplete Kitchens",
                    "Number of Housing Units w. Incomplete Plumbing", "Proportion of Housing Units w. Incomplete Plumbing",
                    "Number of Occupied Units Rented", "Proportion of Occupied Units Rented",
                    "Number of All Vacant Units", "Proportion of All Vacant Units",
                    "Number of Employed in Service Industry", "Proportion of Employed in Service Industry",
                    "Number of Households on Foodstamps/SNAP", "Proportion of Households on Foodstamps/SNAP",
                    "Number of Family Households on Foodstamps/SNAP", "Proportion of Family Households on Foodstamps/SNAP"
)

c_xaxis_options <- c_proper_names[-c(1:3)]
c_yaxis_options <- c_proper_names[2:3]


c_option_key <- as.data.frame(cbind(colnames(census_data), c_proper_names))
colnames(c_option_key) <- c("Variable", "Option")

##### --- User Interface --- ####

full_ui <- dashboardPage(
  
  skin ="red", #sets color 
  
  #title 
  dashboardHeader(title = "Analysis of Covid-19 Cases and Socio-economic Factors for MA Cities/Towns", 
                  titleWidth = 700), 
  
  ###### SIDEBAR ######
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Weekly Cases & Rates", tabName = "weeklyTab"),
      menuItem("Daily Cases and Deaths", tabName = "dailyTab"),
      menuItem("Total Cases & Vulnerabilities", tabName = "censusTab"), 
      menuItem("Census Variable Distributions", tabName = "distributionsTab"),
      menuItem("About Us", tabName = "aboutTab")
    )#ends sidebarMenu 
  ),#ends sidebar 
  
  
  #main body 
  dashboardBody(
    tabItems(
      
      ###### WEEKLY DASHBOARD ######
      tabItem(
        tabName = "weeklyTab",
        
        ### instructions ###
        fluidRow(
          column(1),
          column(11,
                 div(
                   HTML("<h5>The time series graphs below visualize the cumulative count and rate (per 100,000) of confirmed COVID-19 cases in 351 Massachusetts cities/towns from January 1, 2020 to August 19, 2020. For confidentiality purposes, <5 cases are represented as 2.5 cases for towns with populations under 50,000.</h5>"),
                   HTML("<h5><i> Note: Graphs may take a moment to load. Thank you for your patience.</i></h5>")
                   ) #ends div 
                 ) #ends column 11
        ), #ends fluidRow 
        
        
        ### buttons ###
        fluidRow(
          column(1),
          column(11,
                 column(6, radioButtons("w_choice", "What do you want to explore?",
                                        c("Confirmed Cases" = "w_conf","Infection Rates" = "w_rates"))),
                 column(6, radioButtons("w_outliers", "Show Boston?",
                                        c("Keep It In" = "w_yes","Remove It" = "w_no")), 
                        offset = 1.5)
                 )#ends column 11 
        ),#ends fluidRow 
        
        
        ### dropdowns ###
        fluidRow(
          column(1),
          column(11,
                 column(6, selectInput("w_townLeft", "Left Town", w_town_options)),
                 column(6, selectInput("w_townRight", "Right Town", w_town_options), offset = 1.5)
                 ) #ends column 11 
        ),#ends fluidRow
        
        ### hover instructions ###
        fluidRow(column(1), 
                 column(11, 
                        p("Hover the mouse over the graph for information about specific data points.")
                 )#ends column 11
        ),#ends fluidRow 
        
        
        ### graphs ###
        fluidRow(
          column(1),
          column(11,
                 column(6, highchartOutput("weekly_Left")),
                 column(6, highchartOutput("weekly_Right"), offset = 1.5)
                 ) #ends column 11 
        )#ends fluidRow 
        
      ), #ends tabItem for Weekly Dashboard 
      

      
      ###### DAILY DASHBOARD ######
      tabItem(
        tabName = "dailyTab",
        
        ### instructions ###
        fluidRow(
          column(1),
          column(11,
            div(
              HTML("<h5>The time series graphs below visualize the cumulative daily cases and deaths for 40 Massachusetts cities/towns from March 23, 2020 to June 1, 2020.</h5>"),
              HTML("<h5><i> Note: Graphs may take a moment to load. Thank you for your patience.</i></h5>")
              ) #ends div 
          )#ends 11 column 
        ),#ends fluidRow
        
        
        ### radio button options ###
        fluidRow(
          column(1),
          column(11,
                 column(6, radioButtons("d_choice", "What do you want to explore?",
                                        c("Daily Cases" = "d_cases",
                                          "Daily Deaths" = "d_deaths"))),
                 column(6,radioButtons("d_outliers", "Show Boston & Massachusetts Data?",
                                       c("Keep Them In" = "d_yes","Remove Them" = "d_no")),
                        offset = 1.5)
                 )#ends column 11
        ), #ends fluidRow
        
        
        ### variable dropdowns ###
        fluidRow(
          column(1),
          column(11,
                 column(6, selectInput("d_townLeft", "Left Town", d_town_options)),
                 column(6, selectInput("d_townRight", "Right Town", d_town_options), offset  = 1.5)
                 )#ends column 11
        ),#ends fluidRow
        
        ### hover instructions ###
        fluidRow(column(1), 
                 column(11, 
                        p("Hover the mouse over the graph for information about specific data points.")
                 )#ends column 11
        ),#ends fluidRow 
        
        
        ### graphs ###
        fluidRow(
          column(1),
          column(11,
                 column(6, highchartOutput("daily_Left")),
                 column(6, highchartOutput("daily_Right"), offset = 1.5)
                 )#ends column 11
        )#ends fluidRow 
        
      ),#ends tabItem for daily dashboard 
      
      ###### CENSUS DASHBOARD ######
      tabItem(
        
        tabName = "censusTab",

        ### instructions ###
        fluidRow(column(1),
                 column(11,
                        div(
                          HTML("<h5>The following graph explores the relationship between COVID-19 cases and various socio-economic aspects of the 351 towns in Massachusetts. Total cases is defined as the cumulative number of confirmed COVID-19 cases from January 1, 2020 to August 19, 2020 as reported by the Massachusetts Department of Public Health. For confidentiality purposes, <5 cases are represented as 2.5 cases for towns with populations under 50,000. All proportions were calculated by dividing the raw variable counts by the town population.</h5>"),
                          HTML("<h5><i> Note: Graphs may take a moment to load. Thank you for your patience.</i></h5>")
                          )#ends div
                        )#ends column 11
                 ),#ends fluidRow

        ### controls ###
        fluidRow(column(1),
                 column(11,
                        column(3, radioButtons("c_outliers", "Show Outlier (Boston)?",
                                               c("Keep It In" = "c_yes","Remove It" = "c_no")),
                               radioButtons("c_log", "Log Transformations",
                                            c("None" = "c_log_no", "Both" = "c_log_both",
                                              "X Variable Only" = "c_log_x","Y Variable Only" = "c_log_y"))
                               ),#ends column 3
                        
                        column(4, selectInput("c_yvar", "Y Variable", c_proper_names[-1]),
                               selectInput("c_xvar", "X Variable", c_proper_names[-1])
                               ),#end column 4 
                        
                        column(4, textOutput("census_correlation"), textOutput("logWarn"))#ends column 4 
                        ) #ends column 11
                 ),#ends fluidRow
        
        ### hover instructions ###
        fluidRow(column(1), 
                 column(11, 
                        p("Hover the mouse over the graph for information about specific data points.")
                 )#ends column 11
        ),#ends fluidRow 
        
        ### graph ###
        fluidRow( column(1),
                  column(11, highchartOutput("censusContainer")) #ends column 11 
          )#ends fluidRow 
        
        ), #ends tabItem for Census Dashboard 
      
      ###### DISTRIBUTION DASHBOARD ######
      
      tabItem(
        
        tabName = "distributionsTab",
        
        ### instructions ###
        fluidRow(column(1),
                 column(11,
                        div(
                          HTML("<h5> The histogram below visualizes the frequency distribution of COVID-19 cases and socio-economic census counts. This shows how often each different value in the data set occurs, specifically the number of towns whose data value falls between a certain interval. Total cases is defined as the cumulative number of confirmed COVID-19 cases from January 1, 2020 to August 19, 2020 as reported by the Massachusetts Department of Public Health. All proportions were calculated by dividing the raw variable counts by the town population. Summary statistics are provided below the graph. </h5>"),
                          HTML("<h5><i> Note: Graphs may take a moment to load. Thank you for your patience.</i></h5>")
                          ) #ends div
                        )#ends column 11
        ),#ends fluidRow
        
        ### buttons and text ###
        fluidRow(column(1),
                 column(11,
                        column(2, radioButtons("dist_log", "Log Transformations",
                                               c("No" = "log_no", "Yes" = "log_yes"))),
                        column(2, textOutput("d_logWarn")),
                        column(2, radioButtons("dist_out", "Include Outliers",
                                               c("Yes" = "out_yes", "No" = "out_no"))),
                        column(6, textOutput("d_outlier_info")) 
                        ) #ends column 11 
                 ), #ends fluidRow
        
        ### variable selection ###
        fluidRow(column(1), 
                 column(4, selectInput("d_xvar", "Variable", c_proper_names[-1])),
        ),#ends fluid Row 
        
        ### zoom instructions ###
        fluidRow(column(1), 
                 column(11, 
                        p("To zoom, click on the graph and drag the box over the area of interest.")
                        )#ends column 11
        ),#ends fluidRow 
        
        ### graph ###
        fluidRow( column(1),
                  column(11, highchartOutput("densityContainer"))#ends column 11 
        ), #end fluidRow
        
        ### summary info ###
        fluidRow(column(1),
                 column(11, verbatimTextOutput("d_info")) #ends column 11 
        ), #end fluiidRow
        
        ### quartile explanation ### 
        fluidRow(column(1),
                 column(11,
                        div(
                          HTML("<h5> **To determine the outliers, the following standard procedure is implemented: first, the Interquartile Range (IQR) is calculated IQR = 3rd Quartile - 1st Quartile. The lower bound equals 1st Quartile - 1.5*IQR and the upper bound equals 3rd Quartile + 1.5*IQR. Any observation that falls either less than the lower bound or greater than the upper bound is considered an outlier.**</h5>")
                        ) #ends div
                        ) #ends column 11
        ) #end fluidRow 
        
      ),#ends tabItem for Distribution Dashboard 
      
      
      ###### ABOUT US DASHBOARD ######
      
      tabItem(
        
        tabName = "aboutTab",
        
        fluidRow(
          column(1),
          column(10,
                 div(
                   
                   #title 
                   HTML("<br><center><h2> About Us </h2></center>"),
                   
                   #paragraph 1 overview 
                   HTML("<h5>This interactive application is designed to allow users to explore the relationships between Covid-19 Cases and various socio-economic factors of towns and cities as well as time series graphs of confirmed Covid-19 cases, rates, and deaths.<h5>"),
                   
                   #original map story thing  
                   HTML("<h5>It is an expansion of the geospatial mapping project, "),
                   tags$a(href="https://bucas.maps.arcgis.com/apps/MapSeries/index.html?appid=e820a92d6bbc4c9099c59494a4e9367a#", "Vulnerability in Massachusetts During COVID-19 Epidemic"),
                   HTML(" completed by members of the CRESSH research team, students in the Environmental Health Department, and students in the EH811 GIS for Public Health class, led by Patricia Fabian. The goal is to understand which socio-economic factors increase citizens’ vulnerability to Covid-19 and ultimately guide resources towards the most at risk areas in the state.</h5>"),
                   
                   #data sources  
                   HTML("<h5>The "),
                   tags$a(href="https://www.mass.gov/info-details/covid-19-response-reporting#covid-19-cases-by-city/town-", "weekly Covid-19 data"),
                   HTML("and the "),
                   tags$a(href="https://www.mass.gov/info-details/covid-19-response-reporting#covid-19-weekly-public-health-report-", "daily Covid-19 data"),
                   HTML("is provided by the Massachusetts Department of Public Health, and the weekly cases are updated every Wednesday. The socio-economic data is from the "),
                   tags$a(href="https://www.census.gov/programs-surveys/acs", "2014-2018 American Community Survey"),
                   
                   #shameless plug 
                   HTML("<h5>These exploratory visualizations were created by Kathryn Haglich, and the source code can be found on "),
                   tags$a(href="https://github.com/hagk17/MA-Covid-19-Analysis-Application", "Github."),
                   HTML("To inquire or contribute to this effort, please contact <b>cresshbu@gmail.com</b>.")
                   
                 ) #ends div 
                 )#ends column 10
                 
        )#ends fluidRow 
        
      )#ends tabItem for About Us Dashboard 

      
    ) #end tabItems 
  )#end dashboardBody
) #ends dashboarPage 






##### --- Server --- ####

full_server <- function(input, output){
  
  ###### ---- WEEKLY ---- ######
  
  #gets full general data 
  w_general_data <- reactive({
    
    if(input$w_choice == "w_conf"){good_data <- w_conf_data }#weekly_data %>% filter(Type == "Conf")}
    else{good_data <- w_rate_data} #weekly_data %>% filter(Type == "Rate")}
    if(input$w_outliers == "w_no"){ good_data %<>% filter(Town != "Boston")}
    
    return(good_data)
  }) #ends w_general_data reactive 
  
  #gets left data 
  w_getDataForLeft <- reactive({
    l_good_data <- w_general_data()
    if(input$w_townLeft != "All Towns"){l_good_data %<>% filter(Town == input$w_townLeft)}
    return(l_good_data)
  }) #ends w_getDataForLeft reactive
  
  #gets right data 
  w_getDataForRight <- reactive({
    r_good_data <- w_general_data()
    if(input$w_townRight != "All Towns"){r_good_data %<>% filter(Town == input$w_townRight)}
    return(r_good_data)
  }) #ends w_getDataForRight reactive 
  
  #creates left graph 
  w_left_hchart <- reactive({
    
    left_data <- w_getDataForLeft()
    
    lw_hchart <- hchart(left_data, "line", hcaes(x = Time, y = Count, group = Town))%>%
      hc_xAxis(title = list(text = "Week")) %>%
      hc_title(text = input$w_townLeft, margin = 20, align = "left") %>% 
      #hc_subtitle(text = "Covid Data Last Updated August 19, 2020", align = "left") %>%
      hc_legend(enabled = F)
    
    if(input$w_choice == "w_conf"){
      lw_hchart <- lw_hchart %>% hc_yAxis(title = list(text = "Confirmed Cases"))}
    else{lw_hchart <- lw_hchart %>% hc_yAxis(title = list(text = "Infection Rate (Per 100,000)"))}
    
    return(lw_hchart)
  }) #ends w_left_hchart reactive
  
  #creates right graph 
  w_right_hchart <- reactive({
    
    right_data <- w_getDataForRight()
    
    rw_hchart <- hchart(right_data, "line", hcaes(x = Time, y = Count, group = Town))%>%
      hc_xAxis(title = list(text = "Week")) %>%
      hc_title(text = input$w_townRight, margin = 20, align = "left") %>% 
      #hc_subtitle(text = "Covid Data Last Updated August 19, 2020", align = "left") %>%
      hc_legend(enabled = F)
    
    if(input$w_choice == "w_conf"){
      rw_hchart <- rw_hchart %>% hc_yAxis(title = list(text = "Confirmed Cases"))}
    else{rw_hchart <- rw_hchart %>% hc_yAxis(title = list(text = "Infection Rate (Per 100,000)"))}
    
    return(rw_hchart)
    
  }) #ends w_right_hchart reactive 
  
  #outputs 
  
  output$weekly_Left <- renderHighchart({w_left_hchart()})
  output$weekly_Right <- renderHighchart({w_right_hchart()})
  

  
  
  ###### ---- DAILY ---- ######
  
  #get general daily data 
  d_general_data <- reactive({ 
    
    if(input$d_choice == "d_cases"){
      good_data <- daily_data %>% filter(Cases_Deaths == "Cases")}
    else{good_data <- daily_data %>% filter(Cases_Deaths == "Deaths")}
    
    if(input$d_outliers == "d_no"){ 
      good_data %<>% filter(Town != "Massachusetts" & Town != "Boston")}
    
    return(good_data)
    
  }) #ends  d_general_data reactive
  
  #get daily data for left graph 
  d_getDataForLeft <- reactive({
    l_good_data <- d_general_data()
    if(input$d_townLeft != "All Towns"){l_good_data %<>% filter(Town == input$d_townLeft)}
    return(l_good_data)
  }) #ends d_getDataForLeft reactive
  
  #get daily data for right graph 
  d_getDataForRight <- reactive({
    r_good_data <- d_general_data()
    if(input$d_townRight != "All Towns"){r_good_data %<>% filter(Town == input$d_townRight)}
    return(r_good_data)
  }) #ends d_getDataForRight reactive 
  
  #creates left graph 
  d_left_hchart <- reactive({
    
    left_data <- d_getDataForLeft()
    
    dl_chart <- hchart(left_data, "line", hcaes(x = Time, y = Count, group = Town))%>%
      hc_legend(enabled = F) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_title(text = input$d_townLeft, margin = 20, align = "left")  
    
    if(input$d_choice == "d_cases"){
      dl_chart <- dl_chart %>% hc_yAxis(title = list(text = "Daily Cases")) 
    }
    if(input$d_choice == "d_deaths"){
      dl_chart <- dl_chart %>% hc_yAxis(title = list(text = "Daily Deaths")) 
    }
    
    return(dl_chart)
  }) #ends d_left_hchart reactive 
  
  #creates right graph 
  d_right_hchart <- reactive({
    
    right_data <- d_getDataForRight()
    
    dr_chart <- hchart(right_data, "line", hcaes(x = Time, y = Count, group = Town))%>%
      hc_legend(enabled = F) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_title(text = input$d_townRight, margin = 20, align = "left") 
    
    if(input$d_choice == "d_cases"){
      dr_chart <- dr_chart %>% hc_yAxis(title = list(text = "Daily Cases")) 
    }
    if(input$d_choice == "d_deaths"){
      dr_chart <- dr_chart %>% hc_yAxis(title = list(text = "Daily Deaths")) 
    }
    
    return(dr_chart)
    
  }) #ends d_right_hchart reactive
  
  #outputs 
  output$daily_Left <- renderHighchart({d_left_hchart()})
  output$daily_Right <- renderHighchart({d_right_hchart()})
  
  ###### ---- CENSUS ---- ######
  
  #gets the data 
  c_getData <- reactive({
    
    #gets actual variable names
    get_x_variable_name <- c_option_key$Variable[which(c_option_key$Option == input$c_xvar)]
    get_y_variable_name <- c_option_key$Variable[which(c_option_key$Option == input$c_yvar)]
    
    #gets data for that variable 
    get_data <- census_data %>% select(Town, get_x_variable_name, get_y_variable_name) %>% 
      na.omit()
    
    #removes Boston if desired 
    if(input$c_outliers == "c_no"){get_data <- get_data %>% filter(Town != "Boston")}
    
    #if its the same variable, repeat the column so that it acts as Y-Variable 
    if(ncol(get_data) == 2){
      get_data <- as.data.frame(cbind(get_data, select(get_data,get_x_variable_name)))
    }
    
    colnames(get_data) <- c("Town", "X_Value", "Y_Value")
    
    #log transformations 
    if(input$c_log == "c_log_both"){
      get_data$X_Value <- log(get_data$X_Value)
      get_data$Y_Value <- log(get_data$Y_Value)
    }
    
    if(input$c_log == "c_log_x"){get_data$X_Value <- log(get_data$X_Value)}
    if(input$c_log == "c_log_y"){get_data$Y_Value <- log(get_data$Y_Value)}
    
    return(get_data)
    }) #ends c_getData reactive
  
  #finds problematic points (log(0) = -Inf)
  c_find_problem_logs <- reactive({
    if(input$c_log != "c_log_no"){
      bad_data <- c_getData()
      
      if(input$c_log == "c_log_x"){bad_data <- bad_data %>% filter(X_Value == -Inf)}
      if(input$c_log == "c_log_y"){bad_data <- bad_data %>% filter(Y_Value == -Inf)}
      if(input$c_log == "c_log_both"){
        bad_data <- bad_data %>% filter(X_Value == -Inf | Y_Value == -Inf)
      }
      return(bad_data)
    }#ends outter if statement 
    
  }) #ends c_find_problem_logs reactive
  
  #creates the count and warning for problem log 
  log_warning <- reactive({
    
    bad_data <- c_find_problem_logs()
    
    sen1 <- "observations had transformed data values log(0)=-Infinity."
    sen2 <- "These data points are excluded from the visualization and the correlation calculation."
    
    if(input$c_log == "c_log_no"){
      warning <-  paste("*Warning: 0", sen1, sen2, sep = " ")}
    else{
      num <- nrow(bad_data)
      warning <-  paste("Warning:", num, sen1, sen2, sep = " ")}
    
    return(warning)
  }) #ends log_warning reactive 
  
  #calculates and creates correlation message 
  census_correlation <- reactive({
    
    c_data <- c_getData()
    bad_data <- c_find_problem_logs()
    
    if(input$c_log == "c_log_no"){
      c_cor <- signif(cor(c_data$X_Value, c_data$Y_Value, method = "pearson"), digits = 5) 
      string <- paste("Pearson Correlation for ",input$c_xvar, " and ", input$c_yvar, ": ",
                      c_cor, sep = "")}
    else{
      good_data <- c_data %>% filter(Town %notin% bad_data$Town)
      c_cor <- signif(cor(good_data$X_Value, good_data$Y_Value, method = "pearson"), 
                      digits = 5) 
      
      #makes sure the message is right 
      if(input$c_log == "c_log_x"){
        string <- paste("Pearson Correlation for Log of ",input$c_xvar, " and ", input$c_yvar, ": ",
                        c_cor, sep = "")}
      if(input$c_log == "c_log_y"){
        string <- paste("Pearson Correlation for ",input$c_xvar, " and Log of ", input$c_yvar, ": ",
                        c_cor, sep = "")}
      if(input$c_log == "c_log_both"){
        string <- paste("Pearson Correlation for Log of ",input$c_xvar, " and Log of ", input$c_yvar, ": ",
                        c_cor, sep = "")}
      
    } #ends else statement 
    return(string)
  }) #ends census_correlation reactive
  
  #creates graph 
  census_hchart <- reactive({
    
    graph_data <- c_getData()
    
    c_hchart <- hchart(graph_data, "scatter", hcaes(x = X_Value, y = Y_Value, group = Town)) %>%
      hc_legend(enabled = F)
    
    #adds the axis titles based on variable and log transformation
    if(input$c_log == "c_log_x"){
      c_hchart <- c_hchart %>%
        hc_title(text = paste(input$c_yvar, "vs Log of", input$c_xvar, sep = " "), margin = 20, align = "left") %>% 
        hc_xAxis(title = list(text = paste("Log of ", input$c_xvar))) %>%
        hc_yAxis(title = list(text = input$c_yvar))
    }
    
    if(input$c_log == "c_log_y"){
      c_hchart <- c_hchart%>%
        hc_title(text = paste("Log of", input$c_yvar, "vs", input$c_xvar, sep = " "), 
                 margin = 20, align = "left") %>% 
        hc_xAxis(title = list(text = input$c_xvar)) %>%
        hc_yAxis(title = list(text = paste("Log of ", input$c_yvar)))
    }
    if(input$c_log == "c_log_no"){
      c_hchart <- c_hchart %>% 
        hc_title(text = paste(input$c_yvar, "vs", input$c_xvar, sep = " "), 
                 margin = 20, align = "left") %>% 
        hc_xAxis(title = list(text = input$c_xvar)) %>%
        hc_yAxis(title = list(text = input$c_yvar))
    }
    if(input$c_log == "c_log_both"){
      c_hchart <- c_hchart%>%
        hc_title(text = paste("Log of", input$c_yvar, "vs Log of", input$c_xvar, sep = " "), 
                 margin = 20, align = "left") %>% 
        hc_yAxis(title = list(text = paste("Log of ", input$c_yvar))) %>%
        hc_xAxis(title = list(text = paste("Log of ", input$c_xvar))) 
    }
    
    return(c_hchart)
  }) #ends census_hchart reactive 
  
  #outputs 
  output$censusContainer <- renderHighchart(census_hchart())
  output$logWarn <- renderText({log_warning()})
  output$census_correlation <- renderText({census_correlation()})
  
  
  
  ###### ---- DISTRIBUTION ---- ######
  
  #get the data
  d_getData <- reactive({
    
    #gets actual variable names
    get_x_variable_name <- c_option_key$Variable[which(c_option_key$Option == input$d_xvar)]
    
    #gets data for that variable 
    get_data <- census_data %>% select(Town, get_x_variable_name) %>% 
      na.omit()
    
    colnames(get_data) <- c("Town", "X_Value")
    
    #performs natural log transformation if desired 
    if(input$dist_log == "log_yes"){
      get_data$X_Value <- log(get_data$X_Value)
    }
    
    return(get_data)
  }) #ends d_getData reactive
  
  #removes the problematic logs 
  d_log_removals <- reactive({
    data <- d_getData()
    if(input$dist_log == "log_yes"){data <- data %>% filter(X_Value != -Inf)}
    return(data)
  }) #ends d_log_removals reactive 
  
  #creates the log warning
  d_log_warning <- reactive({
    
    bad_data <- d_getData()
    if(input$dist_log == "log_yes"){bad_data <- bad_data %>% filter(X_Value == -Inf)}
    
    sen1 <- "observations had transformed data values log(0)=-Infinity."
    sen2 <- "These data points are excluded from the visualization and summary statistics."
    
    if(input$dist_log == "log_no"){warning <-  paste("*Warning: 0", sen1, sen2, sep = " ")}
    else{warning <-  paste("Warning:", nrow(bad_data), sen1, sen2, sep = " ")}
    
    return(warning)
    
  }) #ends d_log_warning reactive
  
  #finds the outliers 
  d_id_outlier_towns <- reactive({
    
    og_data <- d_log_removals()
    
    #identify outlier bounds 
    lower_bound <- quantile(og_data$X_Value, 0.25)-IQR(og_data$X_Value)
    upper_bound <- quantile(og_data$X_Value, 0.75)+IQR(og_data$X_Value)
    
    outlier_data <- og_data %>% filter(X_Value < lower_bound | X_Value > upper_bound) %>%
      select(Town)
    
    return(outlier_data)
  }) #ends d_idOutliers reactive 
  
  #removes the outliers 
  d_outlier_removal <- reactive({
    
    dataK <- d_log_removals()
    bad_towns <- as.vector(t(d_id_outlier_towns()))
    
    if(input$dist_out == "out_no"){dataK <- dataK %>% filter(Town %notin% bad_towns)}
    
    return(dataK)
  })#ends d_outlier_removal reactive
  
  #creates warning message about the outliers 
  d_outlier_message <- reactive({
    bad_towns <-  as.vector(t(d_id_outlier_towns()))
    sen1 <- "The following towns are outliers:"
    sen2 <- paste(bad_towns, sep = " ", collapse = ", ")
    message <- paste(sen1, sen2, sep = " ")
    return(message)
  }) #ends d_outlier_message reactive 
  
  #creates distribution graph 
  distribution_hchart <- reactive({
    
    graph_data <- d_outlier_removal()
    
    dist_hchart <- hchart(graph_data$X_Value, name = input$d_xvar, color = "#CC0000") %>%
      hc_yAxis(title = list(text = "Frequency")) %>%
      hc_title(text = input$d_xvar, margin = 20, align = "left")# %>% 
    #hc_subtitle(text = "Covid Data Last Updated August 19, 2020", align = "left")
    
    if(input$dist_log == "log_yes"){
      dist_hchart <- dist_hchart %>%
        hc_xAxis(title = list(text = paste("Log of ", input$d_xvar)))
    }else{
      dist_hchart <- dist_hchart %>%
        hc_xAxis(title = list(text = input$d_xvar))}
    
    return(dist_hchart)
  }) #ends distribution_hchart reactive
  

  #outputs 
  output$d_logWarn <- renderText({d_log_warning()})
  output$d_outlier_info <- renderText({d_outlier_message()})
  output$densityContainer <- renderHighchart({distribution_hchart()})
  output$d_info <- renderPrint({
    graph_data <- d_outlier_removal()
    summary(graph_data$X_Value)
  }) #ends renderPrint output$c_info

  
} #ends full_server function 





##### --- Run the App --- ####

shinyApp(full_ui, full_server)






































