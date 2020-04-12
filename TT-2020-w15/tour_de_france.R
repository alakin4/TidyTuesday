library(tidyverse)
library(janitor)
library(here)
library(ggraph)
library(tidygraph)
library(paletteer)
library(patchwork)
library(lubridate)

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')%>%
  mutate(year = year(start_date),
         previous_winner_name = lag(winner_name, n=1))%>%
  filter(year>=1950)
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')
stage_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/stage_data.csv')

#plots
#stage_wins vs stages_led
ggplot(data = tdf_winners)+
  geom_point(mapping = aes(x=stage_wins, 
                           y =stages_led))
#number of stages won by eventual winners
#who wins with out winning any stage?
#plot
ggplot(data = tdf_winners)+
  geom_point(mapping = aes(x=stage_wins, 
                           y =stages_led))
#1919
#prevent more than one holder...start from 1940
#totalling the time each rider takes on the daily stages
#Time can be added or subtracted from this total time as 
#bonuses or penalties for winning individual stages or being first to the top of a climb or for infractions of the rules
# Add: winning individual stages, being first to the top of a climb 
#subtract: for infractions of the rules
#--------------
#The rider with the lowest overall time at 
#the end of each stage receives a ceremonial yellow 
#bicycling jersey and the right to start the next stage, 
#usually the next day, of the Tour in the yellow jersey
winners_stage_data<-stage_data%>%
  filter(rank ==1 & year>=1950)

tdf_stages<-tdf_stages%>%
  mutate(year = year(Date))%>%
  arrange(year)%>%
  filter(year>=1950)%>%
  left_join(tdf_winners%>%
              select(year,
                     winner_name,
                     previous_winner_name),
            by = c('year'='year'))%>%
  remove_empty("rows")%>%
  mutate(yellow_jersey = ifelse(Stage == '1',
                               previous_winner_name,
                               lag(Winner, n=1)),
         winner_yellow = ifelse(winner_name == yellow_jersey,
                                'yes',
                                'no'))
  
  


# Indicate overall winner
# for each winner: indicate when they wore the yellow jersey,
# that is, +1 stage do by year.
# 
ggplot(data = tdf_winners)+
  geom_line(mapping = aes(x=year,
                          y=time_margin*60))
Add previous winner:
  if stage ==1 and winner is previous winner, then wear yellow
  else: if winner is stage winner:
         increase
