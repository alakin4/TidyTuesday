library(tidyverse)
library(janitor)
library(here)
library(ggraph)
library(tidygraph)
library(paletteer)
library(patchwork)
library(lubridate)
#data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')%>%
  mutate(year = year(start_date),
         previous_winner_name = lag(winner_name, n=1))%>%
  filter(year>=1950)
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')
jerseys_2006_2019<-readr::read_csv(file = 'jerseys_2006_2019.csv')


tdf_stages<-tdf_stages%>%
  mutate(yellow_jersey = ifelse(Stage == '1',
                                previous_winner_name,
                                lag(Winner, n=1)
tdf_stages<-tdf_stages%>%
  mutate(year = year(Date),
         Winner= gsub("\\s*\\[[^\\)]+\\]","",as.character(Winner)))%>%
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
                                'no'))%>%
  filter(year>=2006 & Stage !='P')%>%
  mutate(stage = as.numeric(Stage))

jerseys_2006 <- read.csv(file = 'jerseys_2006.csv', sep="\t",header = TRUE)%>%mutate(year = 2006)
jerseys_2007 <- read.csv(file = 'jerseys_2007.csv', sep="\t",header = TRUE)%>%mutate(year = 2007)
jerseys_2008 <- read.csv(file = 'jerseys_2008.csv', sep="\t",header = TRUE)%>%mutate(year = 2008)
jerseys_2009 <- read.csv(file = 'jerseys_2009.csv', sep="\t",header = TRUE)%>%mutate(year = 2009)
jerseys_2010 <- read.csv(file = 'jerseys_2010.csv', sep="\t",header = TRUE)%>%mutate(year = 2010)
jerseys_2011 <- read.csv(file = 'jerseys_2011.csv', sep="\t",header = TRUE)%>%mutate(year = 2011)
jerseys_2012 <- read.csv(file = 'jerseys_2012.csv', sep="\t",header = TRUE)%>%mutate(year = 2012)
jerseys_2013 <- read.csv(file = 'jerseys_2013.csv', sep="\t",header = TRUE)%>%mutate(year = 2013)
jerseys_2014 <- read.csv(file = 'jerseys_2014.csv', sep="\t",header = TRUE)%>%mutate(year = 2014)
jerseys_2015 <- read.csv(file = 'jerseys_2015.csv', sep="\t",header = TRUE)%>%mutate(year = 2015)
jerseys_2016 <- read.csv(file = 'jerseys_2016.csv', sep="\t",header = TRUE)%>%mutate(year = 2016)
jerseys_2017 <- read.csv(file = 'jerseys_2017.csv', sep="\t",header = TRUE)%>%mutate(year = 2017)
jerseys_2018 <- read.csv(file = 'jerseys_2018.csv', sep="\t",header = TRUE)%>%mutate(year = 2018)
jerseys_2019 <- read.csv(file = 'jerseys_2019.csv', sep="\t",header = TRUE)%>%mutate(year = 2019)

jerseys_2006_2019<-rbind(jerseys_2006,
                         jerseys_2007,
                         jerseys_2008,
                         jerseys_2009,
                         jerseys_2010,
                         jerseys_2011,
                         jerseys_2012,
                         jerseys_2013,
                         jerseys_2014,
                         jerseys_2015,
                         jerseys_2016,
                         jerseys_2017,
                         jerseys_2018,
                         jerseys_2019)
colnames(jerseys_2006_2019)<-c("stages",
                               "yellow_jersey",
                               "green_jersey",
                               "polkadot_jersey",
                               "white_jersey",
                               "year")
jerseys_2006_2019<-jerseys_2006_2019%>%
  mutate(yellow_jersey=str_to_title(yellow_jersey),
         green_jersey=str_to_title(green_jersey),
         polkadot_jersey=str_to_title(polkadot_jersey),
         white_jersey =str_to_title(white_jersey))%>%
  write.csv("jerseys_2006_2019.csv", row.names = FALSE)

                           
ggplot()+
  geom_point(data = tdf_stages,
             mapping = aes(x=year,
                          y=stage,
             color = winner_yellow
             ),
             size=10)
  
mystages<-tdf_stages%>%
count(Stage)%>%
  arrange(as.numeric(Stage))

# remove accents
#iconv(in_text,from="UTF-8",to="ASCII//TRANSLIT")
##if(jerseys_2006_2019$y<10){element_text(hjust =-2.5)} else {element_text(hjust =8)}
#as.vector(rbind(rep('',22),
#               seq(0, 21,1)))
#as.vector(t(cbind(v_1,v_2)))
#axis.text.x = element_text( hjust = 1, vjust = 0.5)
#c(0, sprintf("%d min.", seq(2, 14, 2)))
# geom_chicklet(radius = grid::unit(8, 'mm') creates some interesting shapes
