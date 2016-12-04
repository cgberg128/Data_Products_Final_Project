
library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales) #For labeling on pie charts
library(stringr)

myfile1 <- file.path("data","for_analysis_2015_2016.csv")
for_analysis_2015_2016 <- read.csv(myfile1,stringsAsFactors = FALSE)


myfile2 <- file.path("data","for_analysis_2016_2017.csv")
for_analysis_2016_2017 <- read.csv(myfile2,stringsAsFactors = FALSE)

myfile3 <- file.path("data","daily_data.csv")
daily_data <- read.csv(myfile3,stringsAsFactors = FALSE)

myfile4 <- file.path("data","opponents_2015_2016.csv")
opponents_2015_2016 <- read.csv(myfile4,stringsAsFactors = FALSE)

myfile5 <- file.path("data","opponents_2016_2017.csv")
opponents_2016_2017 <- read.csv(myfile5,stringsAsFactors = FALSE)



shinyUI(dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
            sidebarMenu(
                  menuItem("User's Guide",tabName = "guide", icon = icon("question")),
                  menuItem("Game Logs", tabName = "dashboard", icon = icon("table")),
                  menuItem("Opponent Data", tabName = "opponents", icon= icon("table")),
                  menuItem("Plots 2015-2016", tabName = "plots", icon = icon("line-chart")),
                  menuItem("Plots 2016-2017", tabName = "plots_curr", icon = icon("line-chart")),
                  menuItem("Daily Projections",tabName = "daily_proj", icon = icon("arrows-v"))
                  
            )
            
      ),
      dashboardBody(
            
            
            tabItems(
                  
                  tabItem(tabName = "guide",
                          
                          h2("User's Guide"),
                          h4("There are 5 tabs with content in this dashboard:"),
                          h5("1. Game Logs -- includes all basic basketball statistics at individual player level for both 2016-2017 season and 
                             2015-2016 season"),
                          h5("2. Opponent Data -- includes summaries of basic basketball statistics by opponent for both 2016-2017 season and 
                             2015-2016 season"),
                          h5("3. Plots 2015-2016 -- for 2015-2016 season, contains plots of individual player performance (minutes, fantasy points per minute,
                             total fantasy points, as well as a pie chart breakdown of how they get their fantasy points)"),
                          h5("4. Plots 2016-2017 -- for 2016-2017 season, contains plots of individual player performance (minutes, fantasy points per minute,
                             total fantasy points, as well as a pie chart breakdown of how they get their fantasy points)"),
                          h5("5. Daily Projections -- shows key metrics in evalauting a player's daily fantasy basketball value"),
                          h6("All 2016-2017 data courtesy of www.bigdataball.com.  2015-2016 data scraped from www.nba.com")
                          )
                  ,
                  
                  
                  # First tab content
                  tabItem(tabName = "dashboard",
                          #2 datasets in this tab
                          h2("Dashboard"),
                          
                          
                          
                          
                          tabsetPanel(
                                
                                tabPanel('current_season_data',
                                         dataTableOutput("mytable1")),
                                
                                tabPanel('last_season_data',
                                         dataTableOutput("mytable2"))
                                
                          )
                          
                  ),
                  
                  #Second tab content
                  tabItem(tabName = "opponents",
                          #2 datasets in this tab
                          h2("Opponents"),
                          
                          
                          
                          
                          tabsetPanel(
                                
                                tabPanel('current_season_opp',
                                         dataTableOutput("mytable4")),
                                
                                tabPanel('last_season_opp',
                                         dataTableOutput("mytable3"))
                                
                          )
                          
                  ),
                  
                  #Third tab content
                  tabItem(tabName = "plots",
                          h2("Plots Last Season"),
                          selectInput('x','Player_ID',sort(unique(for_analysis_2015_2016$player_id)),for_analysis_2015_2016$player_id[1]), #I want a filter value that is dependent on which variable I want to filter on
                          selectInput('y','Number of Games To Plot',1:82,82), # Would like to make the possible values be specific to player, but struggling with reactive expressions
                          htmlOutput("z"), #Depends on other inputs
                          htmlOutput("z_3last"), #Depends on other inputs
                          htmlOutput("z_4last"), #Depends on other inputs,
                          htmlOutput("z_5last"), #Depends on other inputs,
                          plotOutput('plot_1'),
                          plotOutput('plot_2'),
                          plotOutput('plot_3'),
                          plotOutput('plot_7')
                          
                          
                          
                          
                  ),
                  
                  #Fourth tab content
                  tabItem(tabName = "plots_curr",
                          h2("Plots Current Season"),
                          selectInput('x_2','Player_ID',sort(unique(for_analysis_2016_2017$player_id)),for_analysis_2016_2017$player_id[1]), #I want a filter value that is dependent on which variable I want to filter on
                          selectInput('y_2','Number of Games To Plot',1:82,82), # Would like to make the possible values be specific to player, but struggling with reactive expressions
                          htmlOutput("z_2"),
                          htmlOutput("z_3"),
                          htmlOutput("z_4"),
                          htmlOutput("z_5"),
                          plotOutput('plot_4'),
                          plotOutput('plot_5'),
                          plotOutput('plot_6'),
                          plotOutput('plot_8')
                          
                          
                          
                  ),
                  
                  #Fifth tab content
                  tabItem(tabName = "daily_proj",
                          h2("Daily Projections"),
                          dataTableOutput("mytable5")
                          
                          
                          
                          
                  )
                  )
)
            )
)


