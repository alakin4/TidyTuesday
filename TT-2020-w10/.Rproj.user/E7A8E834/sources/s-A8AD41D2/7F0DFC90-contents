# load packages
library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggpmthemes)
library(patchwork)
library(here)

# colors
# https://www.jordanprindledesigns.com/blog/15-more-color-palettes
#TASK:
#when do hockey players reach their prime?
#Use a measure of goals per game. consider the top 250?
#rank players and seasons based on goals per game. 
#Average age for player's prime (how many seasons into the NBA?)
# earley boomer vs late boomers
# what is the most productive age
# Correlation between age and prime?

#load data
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

#variables of interest: rank, position, player,
#years, yr_start, league (NHL) , team, age, season_games, goals, assists
#
#Sum goals per season, sum games per season, ger average age
season_goals_age <- season_goals %>%
                    group_by(rank,player,season,yr_start)%>%
                    summarize(season_age = mean(age),
                           season_goals = sum(goals))%>%
                    mutate(goals_per_game = season_goals/season_age,
                           season_yr = as.numeric(paste0(str_sub(season,1,2),str_sub(season,-2))),
                           season_yr_edit = if_else(season_yr==1900, 2000,season_yr),
                           years_into_season = season_yr_edit-yr_start)


#First Join in on the debate between Alex and Wayne
#Wayne first played in last year of the World Hockey League before joining NHL
ggplot()+
  geom_line(data = season_goals_age%>% 
              filter(player %in% c("Alex Ovechkin", "Wayne Gretzky")), 
            mapping = aes(x = years_into_season, 
                          y = goals_per_game, 
                          color = player)
  )

ggplot()+
  geom_line(data = season_goals_age%>% 
              filter(player %in% c("Alex Ovechkin", "Wayne Gretzky")), 
            mapping = aes(x = season_age, 
                          y = goals_per_game, 
                          color = player))

#plot
#1. line plot gpg at every player's age
#::x= age, y= av_goals_per_game. lines for each player
ggplot() +
  geom_line(data = season_goals_age, 
            mapping = aes(x = season_age, y = goals_per_game, group = player),
            color = "gray",
            alpha = 0.3)

#Note: The line graph is a bit messy so I go to the bar plot
#If you plot the data as is in season_goals_age, the bars show smaller bars, 
# one for each play. You could avoid this by getting the best season 
# for each player
best_season_goals_age<-season_goals_age%>%
                      group_by(player)%>%
                      slice(which.max(goals_per_game))

# Average age for player's prime
mean(best_season_goals_age$season_age)
# How long does it take for players to reach their prime
mean(best_season_goals_age$years_into_season)

theme_set(theme_light_modified(base_family = "IBM Plex Sans"))

heading <-'When do Hockey Players Hit their Prime?'
sub_heading<-"A player's prime is determined as their highest number of goals per game.\nThe graphs show the number of players hitting their primes by age, and by the number of seasons it takes."

#The two plots
plot_age<-ggplot()+
  geom_bar(data = best_season_goals_age, 
            mapping = aes(x = season_age),
            fill = "#b8926a",
            width =  0.8) +
  xlab('Age') +
  ylab(NULL)+ 
  scale_y_continuous( breaks = seq(0, 45, 5), expand = c(0,0))+
  scale_x_continuous( breaks = seq(0, 50, 2), expand = c(0,0))+
  annotate(
    geom = "curve", x = 18, y = 1, xend = 18, yend = 10, 
    curvature = -0.1, arrow = arrow(length = unit(1.2, "mm")))+
  annotate(geom = "text", x = 18, y = 11, label = "Wayne\nGretzky",hjust = "left", size = 4,fontface = "bold")+
  annotate(geom = "label", 
           x = 27, 
           y =24, 
           label = "Most NHL players hit their prime either\nat the age of 23 or 25. The average prime\nage for hockey players is 24.9 years. The\nlegend Wayne Gretzky at hit his career\nprime at 18 years while playing World\nHockey League before joining NHL,\nwhere he hit his prime at 21.\nAlex Ovechkin hit his prime at\n22 years.", 
           color = "black",
           hjust = 0, 
           size = 4.3,
           fill="#eae6df")+
  theme(
    text = element_text(color = "black", size = 14, family = "IBM Plex Sans Medium"),
    plot.background = element_rect(fill = "#eae6df",
                                   color = "#eae6df"),
    panel.background = element_rect(fill = "#eae6df"),
    axis.ticks.y = element_line(color = "black", size = 0.05),
    axis.text.y = element_text(color = "black"),
    axis.line.x = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", margin = margin(t = 10)),
    axis.title.x = element_text(margin = margin(t = 10),face="bold"),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.05)
  )



#2. bar plot plot::
# x= years into the league, y= av_goals_per_game.
plot_seasons<-ggplot()+
  geom_bar(data = best_season_goals_age, 
           mapping = aes(x = years_into_season),
           fill = "#b8926a",
           width =  0.8) +
    xlab('Number of Seasons Played') +
    ylab(NULL)+
  scale_y_continuous( breaks = seq(0, 45, 5), expand = c(0,0))+
  scale_x_continuous( breaks = seq(0, 30, 2), expand = c(0,0))+
  annotate(
    geom = "curve", x = 0, y = 1, xend = 0, yend = 8, 
    curvature = -0.1, arrow = arrow(length = unit(1.2, "mm")))+
  annotate(geom = "text", x = 0, y = 9, label = "Wayne\nGretzky",hjust = "left", size = 4,fontface = "bold")+
  annotate(geom = "label", 
           x = 8, 
           y =25, 
           label = "Most NHL players hit their prime after\n6 seasons, and it takes 6.2 seasons on\naverage for players to hit their prime.\nWayne Gretzky had his best season while\nplaying World Hockey League. In the NHL,\nit took him 3 seasons to have his prime\nseason. Alex Ovechkin also hit his prime\nin the third season of playing.", 
           color = "black",
           hjust = 0, 
           size = 4.3,
           fill = "#eae6df")+
  theme(
      text = element_text(color = "black", size = 14, family = "IBM Plex Sans Medium"),
      plot.background = element_rect(fill = "#eae6df",
                                     color = "#eae6df"),
      panel.background = element_rect(fill = "#eae6df"),
      axis.ticks.y = element_line(color = "black", size = 0.05),
      axis.line.x = element_line(colour = "black"),
      axis.text.x = element_text(color = "black", margin = margin(t = 10)),
      axis.text.y = element_text(color = "black"),
      axis.title.x = element_text(margin = margin(t = 10),face="bold"),
      panel.grid = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(color = "black", size = 0.05)
    )

wrap_plots(plot_age, plot_seasons, 
                   ncol = 2) +
  plot_annotation(
  title = heading,
  subtitle = sub_heading,
  caption = "Tidytuesday week #10 | Data: hockey-reference.com  | @kinenealan",
  theme = theme(
    text = element_text(color = "black", size = 14),
    plot.background = element_rect(fill = "#eae6df"),
    panel.background = element_rect(fill = "#eae6df"),
    plot.title = element_markdown(hjust = 0, family = "IBM Plex Sans Bold", size = 28),
    plot.subtitle = element_text(hjust = 0 , size = 13, family = "IBM Plex Sans Medium Italic"),
    plot.caption = element_text(color = "gray50", size = 10)
  )
)+
  ggsave(here::here("plots", "hockey_primes.png"), dpi = 320, width = 14, height = 10, scale = 1)

