
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

shinyServer(function(input, output) { 



      output$z <- renderUI({
            
            available <- filter(for_analysis_2015_2016,player_id==input$x) 
            
            available <- filter(available,running_gp > (max(available$running_gp) - as.numeric(input$y)))
            
            selectInput("z",'Opponent',c("All",unique(available$opponent)),"All")
            
      })
      
      output$z_3last <- renderUI({
            
            available <- filter(for_analysis_2015_2016,player_id==input$x) 
            
            available <- filter(available,running_gp > (max(available$running_gp) - as.numeric(input$y)))
            
            selectInput("z_3last",'Team Rest Situation',c("All",unique(available$team_rest_sit)),"All")
            
      })
      
      output$z_4last <- renderUI({
            
            available <- filter(for_analysis_2015_2016,player_id==input$x) 
            
            available <- filter(available,running_gp > (max(available$running_gp) - as.numeric(input$y)))
            
            selectInput("z_4last",'Opponent Rest Situation',c("All",unique(available$opp_rest_sit)),"All")
            
      })
      
      
      output$z_5last <- renderUI({
            
            available <- filter(for_analysis_2015_2016,player_id==input$x) 
            available <- filter(available,running_gp > (max(available$running_gp) - as.numeric(input$y)))
            
            selectInput("z_5last",'Player DNP',c("All",sort(unique(for_analysis_2015_2016$player_id))),"All")
            
      })
      
      
      
      
      
      output$z_2 <- renderUI({
            
            available_2 <- filter(for_analysis_2016_2017,player_id==input$x_2) 
            available_2 <- filter(available_2,running_gp > (max(available_2$running_gp) - as.numeric(input$y_2)))
            
            selectInput("z_2",'Opponent',c("All",unique(available_2$Opp)),"All")
            
      })
      
      
      output$z_3 <- renderUI({
            
            available_2 <- filter(for_analysis_2016_2017,player_id==input$x_2) 
            available_2 <- filter(available_2,running_gp > (max(available_2$running_gp) - as.numeric(input$y_2)))
            
            selectInput("z_3",'Team Rest Situation',c("All",unique(available_2$team_rest_sit)),"All")
            
      })
      
      
      
      output$z_4 <- renderUI({
            
            available_2 <- filter(for_analysis_2016_2017,player_id==input$x_2) 
            available_2 <- filter(available_2,running_gp > (max(available_2$running_gp) - as.numeric(input$y_2)))
            
            selectInput("z_4",'Opponent Rest Situation',c("All",unique(available_2$opp_rest_sit)),"All")
            
      })
      
      output$z_5 <- renderUI({
            
            available_2 <- filter(for_analysis_2016_2017,player_id==input$x_2) 
            available_2 <- filter(available_2,running_gp > (max(available_2$running_gp) - as.numeric(input$y_2)))
            
            selectInput("z_5",'Player DNP',c("All",sort(unique(for_analysis_2016_2017$player_id))),"All")
            
      })
      
      
      
      # I AM SURE THERE IS A BETTER TO DEAL WITH output$z and output$z_2....
      # I AM TRYING TO LEARN HOW BEST TO INCORPORATE AN INPUT LIST THAT DEPENDS ON THE SELECTION FROM ANOTHER INPUT LIST
      # Approach I used was based on this link:
      # http://stackoverflow.com/questions/33086987/filter-one-selectinput-based-on-selection-from-another-selectinput
      
      
      
      single_player_data <- reactive({
            
            sp_data <- filter(for_analysis_2015_2016,player_id==input$x) 
            
            #Filtering on a single player ID and # of games based off user input
            
            
            sp_data <- filter(sp_data,running_gp > (max(sp_data$running_gp) - as.numeric(input$y)))
            
            
            
            if(input$z != "All"){sp_data <- filter(sp_data,opponent==input$z)}
            
            if(input$z_3last != "All"){sp_data <- filter(sp_data,team_rest_sit==input$z_3last)}
            
            if(input$z_4last != "All"){sp_data <- filter(sp_data,opp_rest_sit==input$z_4last)}
            
            if(input$z_5last != "All"){sp_data <- filter(sp_data,DNP_Player_1 == input$z_5last |
                                                               DNP_Player_2 == input$z_5last |
                                                               DNP_Player_3 == input$z_5last |
                                                               DNP_Player_4 == input$z_5last |
                                                               DNP_Player_5 == input$z_5last |
                                                               DNP_Player_6 == input$z_5last |
                                                               DNP_Player_7 == input$z_5last |
                                                               DNP_Player_8 == input$z_5last |
                                                               DNP_Player_9 == input$z_5last |
                                                               DNP_Player_10 == input$z_5last |
                                                               DNP_Player_11 == input$z_5last |
                                                               DNP_Player_12 == input$z_5last |
                                                               DNP_Player_13 == input$z_5last |
                                                               DNP_Player_14 == input$z_5last |
                                                               DNP_Player_15 == input$z_5last |
                                                               DNP_Player_16 == input$z_5last |
                                                               DNP_Player_17 == input$z_5last |
                                                               DNP_Player_18 == input$z_5last |
                                                               DNP_Player_19 == input$z_5last |
                                                               DNP_Player_20 == input$z_5last |
                                                               DNP_Player_21 == input$z_5last |
                                                               DNP_Player_22 == input$z_5last |
                                                               DNP_Player_23 == input$z_5last |
                                                               DNP_Player_24 == input$z_5last |
                                                               DNP_Player_25 == input$z_5last |
                                                               DNP_Player_26 == input$z_5last |
                                                               DNP_Player_27 == input$z_5last |
                                                               DNP_Player_28 == input$z_5last |
                                                               DNP_Player_29 == input$z_5last |
                                                               DNP_Player_30 == input$z_5last |
                                                               DNP_Player_31 == input$z_5last |
                                                               DNP_Player_32 == input$z_5last 
                                                         
            )}
            
            # print(sp_data)
            
            return(sp_data)
            
            
      })
      
      
      
      single_player_data_2 <- reactive({
            
            sp_data_2 <- filter(for_analysis_2016_2017,player_id==input$x_2) 
            
            #Filtering on a single player ID and # of games based off user input
            
            
            sp_data_2 <- filter(sp_data_2,running_gp > (max(sp_data_2$running_gp) - as.numeric(input$y_2)))
            
            
            
            
            
            if(input$z_2 != "All"){sp_data_2 <- filter(sp_data_2,Opp==input$z_2)}
            
            
            if(input$z_3 != "All"){sp_data_2 <- filter(sp_data_2,team_rest_sit==input$z_3)}
            
            if(input$z_4 != "All"){sp_data_2 <- filter(sp_data_2,opp_rest_sit==input$z_4)}
            
            #  print(sp_data_2)
            
            if(input$z_5 != "All"){sp_data_2 <- filter(sp_data_2,DNP_Player_1 == input$z_5 |
                                                             DNP_Player_2 == input$z_5 |
                                                             DNP_Player_3 == input$z_5 |
                                                             DNP_Player_4 == input$z_5 |
                                                             DNP_Player_5 == input$z_5 |
                                                             DNP_Player_6 == input$z_5 |
                                                             DNP_Player_7 == input$z_5 |
                                                             DNP_Player_8 == input$z_5 |
                                                             DNP_Player_9 == input$z_5 |
                                                             DNP_Player_10 == input$z_5 |
                                                             DNP_Player_11 == input$z_5 )}
            
            
            
            
            
            return(sp_data_2)
            
            
            
      })
      
      
      single_player_data_3 <- reactive({
            
            sp_data_3 <- filter(for_analysis_2015_2016,player_id==input$x) 
            
            #Filtering on a single player ID and # of games based off user input
            
            
            sp_data_3 <- filter(sp_data_3,running_gp > (max(sp_data_3$running_gp) - as.numeric(input$y)))
            
            
            
            sp_data_3 <- sp_data_3  %>% 
                  group_by(player_id) %>% 
                  summarize(dk_pts = sum(dk_pts),
                            dk_pts_ftm = sum(dk_pts_ftm),
                            dk_pts_2pts = sum(dk_pts_2pts),
                            dk_pts_3pts = sum(dk_pts_3pts),
                            dk_pts_reb = sum(dk_pts_reb),
                            dk_pts_ast = sum(dk_pts_ast),
                            dk_pts_st = sum(dk_pts_st),
                            dk_pts_blk = sum(dk_pts_blks),
                            dk_pts_to = sum(dk_pts_to),
                            dk_pts_doub_doub = sum(dk_pts_doub_doub),
                            dk_pts_trip_doub = sum(dk_pts_trip_doub)) %>% 
                  mutate(FTM = 100*dk_pts_ftm/dk_pts,
                         PTS2 = 100*dk_pts_2pts/dk_pts,
                         PTS3 = 100*dk_pts_3pts/dk_pts,
                         REB = 100*dk_pts_reb/dk_pts,
                         AST = 100*dk_pts_ast/dk_pts,
                         ST = 100*dk_pts_st/dk_pts,
                         BLK = 100*dk_pts_blk/dk_pts,
                         TO = 100*dk_pts_to/dk_pts,
                         DOUB_DOUB = 100*dk_pts_doub_doub/dk_pts,
                         TRIP_DOUB = 100*dk_pts_trip_doub/dk_pts) %>% 
                  select(FTM,PTS2,PTS3,REB,AST,ST,BLK,TO,DOUB_DOUB,TRIP_DOUB) 
            
            
            
            sp_data_3 <- gather(sp_data_3)
            
            names(sp_data_3) = c("CATEGORY","DK_PTS_PCT")
            
            #  print(sp_data_3)
            
            return(sp_data_3)
            
            
      })
      
      
      single_player_data_4 <- reactive({
            
            sp_data_4 <- filter(for_analysis_2016_2017,player_id==input$x_2) 
            
            #Filtering on a single player ID and # of games based off user input
            
            
            sp_data_4 <- filter(sp_data_4,running_gp > (max(sp_data_4$running_gp) - as.numeric(input$y_2)))
            
            
            
            sp_data_4 <- sp_data_4  %>% 
                  group_by(player_id) %>% 
                  summarize(dk_pts = sum(dk_pts),
                            dk_pts_ftm = sum(dk_pts_ftm),
                            dk_pts_2pts = sum(dk_pts_2pts),
                            dk_pts_3pts = sum(dk_pts_3pts),
                            dk_pts_reb = sum(dk_pts_reb),
                            dk_pts_ast = sum(dk_pts_ast),
                            dk_pts_st = sum(dk_pts_st),
                            dk_pts_blk = sum(dk_pts_blks),
                            dk_pts_to = sum(dk_pts_to),
                            dk_pts_doub_doub = sum(dk_pts_doub_doub),
                            dk_pts_trip_doub = sum(dk_pts_trip_doub)) %>% 
                  mutate(FTM = 100*dk_pts_ftm/dk_pts,
                         PTS2 = 100*dk_pts_2pts/dk_pts,
                         PTS3 = 100*dk_pts_3pts/dk_pts,
                         REB = 100*dk_pts_reb/dk_pts,
                         AST = 100*dk_pts_ast/dk_pts,
                         ST = 100*dk_pts_st/dk_pts,
                         BLK = 100*dk_pts_blk/dk_pts,
                         TO = 100*dk_pts_to/dk_pts,
                         DOUB_DOUB = 100*dk_pts_doub_doub/dk_pts,
                         TRIP_DOUB = 100*dk_pts_trip_doub/dk_pts) %>% 
                  select(FTM,PTS2,PTS3,REB,AST,ST,BLK,TO,DOUB_DOUB,TRIP_DOUB) 
            
            
            
            sp_data_4 <- gather(sp_data_4)
            
            names(sp_data_4) = c("CATEGORY","DK_PTS_PCT")
            
            # print(sp_data_4)
            
            
            return(sp_data_4)
            
      })
      
      
      
      
      
      
      
      output$plot_1 <- renderPlot({
            
            
            p <- ggplot(single_player_data(),
                        aes_string(x=single_player_data()$running_gp,y=single_player_data()$MIN)) +  geom_line() + geom_point(aes(size=margin)) +
                  stat_smooth(method="lm",formula=y~x) + ggtitle("Minutes Played Over Time") + 
                  xlab("Running GP") + ylab("Minutes Played") + scale_y_continuous(limits=c(min(single_player_data()$MIN)-5,max(single_player_data()$MIN)+5))
            print(p)
            
            #Would like to add a third dimension (blowouts based on margin of victory)
            #Color points (maybe in red) IF blowout, so we know not to rely on that data point too much!
            
      })
      
      output$plot_2 <- renderPlot({
            
            
            p <- ggplot(single_player_data(),
                        aes_string(x=single_player_data()$running_gp,y=single_player_data()$dk_pts_per_min)) +  geom_line() + geom_point(aes(size=margin)) +
                  stat_smooth(method="lm",formula=y~x) + ggtitle("DK Pts Per Min Over Time") + 
                  xlab("Running GP") + ylab("DK Pts Per Min") + scale_y_continuous(limits=c(min(single_player_data()$dk_pts_per_min)-0.05,max(single_player_data()$dk_pts_per_min)+0.05))
            print(p)
            
      })
      
      output$plot_3 <- renderPlot({
            
            
            p <- ggplot(single_player_data(),
                        aes_string(x=single_player_data()$running_gp,y=single_player_data()$dk_pts)) +  geom_line() + geom_point(aes(size=margin)) +
                  stat_smooth(method="lm",formula=y~x) + ggtitle("Total DK Pts Over Time") + 
                  xlab("Running GP") + ylab("Total DK Pts") + scale_y_continuous(limits=c(min(single_player_data()$dk_pts)-5,max(single_player_data()$dk_pts)+5))
            print(p)
            
      })
      
      
      
      
      
      output$plot_4 <- renderPlot({
            
            
            p <- ggplot(single_player_data_2(),
                        aes_string(x=single_player_data_2()$running_gp,y=single_player_data_2()$MIN)) +  geom_line() + geom_point(aes(size=margin)) +
                  stat_smooth(method="lm",formula=y~x) + ggtitle("Minutes Played Over Time") + 
                  xlab("Running GP") + ylab("Minutes Played") + scale_y_continuous(limits=c(min(single_player_data_2()$MIN)-5,max(single_player_data_2()$MIN)+5))
            print(p)
            
            
            
      })
      
      output$plot_5 <- renderPlot({
            
            
            p <- ggplot(single_player_data_2(),
                        aes_string(x=single_player_data_2()$running_gp,y=single_player_data_2()$dk_pts_per_min)) +  geom_line() + geom_point(aes(size=margin)) +
                  stat_smooth(method="lm",formula=y~x) + ggtitle("DK Pts Per Min Over Time") + 
                  xlab("Running GP") + ylab("DK Pts Per Min")  + scale_y_continuous(limits=c(min(single_player_data_2()$dk_pts_per_min)-0.05,max(single_player_data_2()$dk_pts_per_min)+0.05))
            print(p)
            
      })
      
      output$plot_6 <- renderPlot({
            
            p <- ggplot(single_player_data_2(),
                        aes_string(x=single_player_data_2()$running_gp,y=single_player_data_2()$dk_pts)) +  geom_line() + geom_point(aes(size=margin)) +
                  stat_smooth(method="lm",formula=y~x) + ggtitle("Total DK Pts Over Time") + 
                  xlab("Running GP") + ylab("Total DK Pts") + scale_y_continuous(limits=c(min(single_player_data_2()$dk_pts)-5,max(single_player_data_2()$dk_pts)+5))
            print(p)
            
            
            
      })
      
      
      
      
      
      output$plot_7 <- renderPlot({
            
            
            bp <- ggplot(single_player_data_3(),aes(x="",y=DK_PTS_PCT,fill=CATEGORY)) + 
                  geom_bar(width = 1, stat = "identity")
            
            pie <- bp + coord_polar("y",start=0) +
                  scale_fill_manual(values=c("AST" = "green",
                                             "DOUB_DOUB"= "pink",
                                             "FTM" = "yellow",
                                             "PTS2" = "black",
                                             "PTS3" = "purple",
                                             "REB" = "blue",
                                             "ST" = "orange",
                                             "BLK" = "white",
                                             "TO" = "red",
                                             "TRIP_DOUB" = "brown"))
            
            
            print(pie)
            
            
            
      })
      
      output$plot_8 <- renderPlot({
            
            
            bp_2 <- ggplot(single_player_data_4(),aes(x="",y=DK_PTS_PCT,fill=CATEGORY)) + 
                  geom_bar(width = 1, stat = "identity")
            
            pie_2 <- bp_2 + coord_polar("y",start=0) +
                  scale_fill_manual(values=c("AST" = "green",
                                             "DOUB_DOUB"= "pink",
                                             "FTM" = "yellow",
                                             "PTS2" = "black",
                                             "PTS3" = "purple",
                                             "REB" = "blue",
                                             "ST" = "orange",
                                             "BLK" = "white",
                                             "TO" = "red",
                                             "TRIP_DOUB" = "brown"))
            
            
            print(pie_2)
            
            
            
      })
      
      
      
      
      
      
      
      output$mytable1 = renderDataTable({
            for_analysis_2016_2017
      })
      
      output$mytable2 = renderDataTable({ #Another dataset that we can examine in a different panel
            for_analysis_2015_2016
      })
      
      output$mytable3 = renderDataTable({ #Another dataset that we can examine in a different panel
            opponents_2015_2016
      },options = list(lengthMenu = c(30),pageLength=30))
      
      
      output$mytable4 = renderDataTable({ #Another dataset that we can examine in a different panel
            opponents_2016_2017
      },options = list(lengthMenu = c(30),pageLength=30))
      
      
      output$mytable5 = renderDataTable({ #Another dataset that we can examine in a different panel
            daily_data
      },options = list(lengthMenu = c(50,100,200,400),pageLength=400))
      
      
      
})



