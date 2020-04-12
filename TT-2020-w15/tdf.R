library(tidyverse)
library(janitor)
library(here)
library(ggtext)
#library(tidygraph)
library(lubridate)
library(glue)
#install.packages("ggchicklet", repos = "https://cinc.rud.is")
library(ggchicklet)
#data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')%>%
  mutate(year = year(start_date),
         previous_winner_name = lag(winner_name, n=1))%>%
  filter(year>=1950)
jerseys_2006_2019<-readr::read_csv(file = 'jerseys_2006_2019.csv')


jerseys_2006_2019<-jerseys_2006_2019%>%
  left_join(tdf_winners%>%
              select(year,
                     winner_name)%>%
              filter(year>=2006),
            by = c('year'='year'))%>%
  mutate(winner_name = ifelse(winner_name == "Óscar Pereiro", 
                              "Oscar Pereiro Sio", 
                              winner_name),
         winner_yellow = ifelse(winner_name == yellow_jersey,
                                'yes',
                                'no'),
         year_winner = glue::glue("{winner_name} ({year})"),
         y = 1)%>%
  left_join(jerseys_2006_2019%>%
              group_by(year)%>%
              summarize(min_stage = min(stages)),
            by=c('year'='year'))

jerseys_2006_2019$year_winner <- fct_reorder(factor(jerseys_2006_2019$year_winner), 
                                             jerseys_2006_2019$year,
                                               .desc = FALSE)

#A little hack since some races include stage zero and other don't,
#Also some have stage 21 (those with no stage zero)
pad_0_stages<-jerseys_2006_2019%>%
  select(year_winner, winner_yellow,stages, y,min_stage)%>%
  filter(min_stage == 0)%>%
  group_by(year_winner)%>%
  slice(1)%>%
  mutate(stages = 21,
         winner_yellow = 'zero data')
# a: if zero, add extra stage 21
# if one, add extra stage 0
pad_21_stages<-jerseys_2006_2019%>%
  select(year_winner, winner_yellow,stages, y,min_stage)%>%
  filter(min_stage == 1)%>%
  group_by(year_winner)%>%
  slice(1)%>%
  mutate(stages = 0,
         winner_yellow = 'zero data')
padded_df <-jerseys_2006_2019%>%
  select(year_winner, winner_yellow, stages, y,min_stage)%>%
  bind_rows(pad_0_stages)%>%
  bind_rows(pad_21_stages)


#plot
#colors
#black: #303030
#yellow: #F7DA00
#-----
#new colors
#black::#121212
#white: fdfdfd
#yellow: f5db2e

my_title <-"THE QUEST FOR THE <span style='color:#f5db2e;'>YELLOW JERSEY</span> IN<br>TOUR de FRANCE"
my_subtitle = glue::glue("Exploring the stages when the year's (2006-2019) general classification winner took-over, and endorsed the yellow jersey.<br>
                          The races with a prologue (0) stage do not have stage 21. The winners are often dominant in the last stages of the race.<br>
                          However in 2018, Geramt Thomas was instead dominat in the first 4 stages.")

ggplot(data = padded_df,
       aes(y=y, 
           x=factor(year_winner), group = factor(stages), fill = winner_yellow))+
  geom_chicklet(radius = grid::unit(4.5, 'mm'),
                color = '#121212')+
  xlab(NULL)+
  ylab('Stage')+
  labs(
    title= my_title,
    subtitle = my_subtitle,
    caption = "Tidytuesday week #15 |Data: tdf R package, kaggle, & www.letour.fr| @kinenealan")+
  scale_y_continuous(expand = c(0, 0.0625),
    breaks = seq(0, 22, 0.5),
    labels = lag(seq(0, 22, 0.5))%>%
      replace(., is.na(.) |(!.%in%c(5.0,15.0,0) &(str_ends(as.character(.), '[.5]'))),
              '')
  )+
  scale_fill_manual(
    values = c('#ffffff', '#f5db2e','#121212'))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = 'none',
        plot.margin = margin(10, 10, 10, 10),
        text = element_text(family = "Roboto-Regular",
                                 color = "#fdfdfd",
                            size = 13,
                            face = 'bold'),
        axis.text = element_text(color = "#fdfdfd", 
                                 size = 13,
                                 face = 'bold'),
        panel.grid = element_blank(),
        axis.title.x = element_text(family = "Roboto-Regular",
                                    margin = margin(t = 10),
                                    face="bold"),
        plot.title = element_markdown(hjust = 0, family = "Asul Bold", size = 28),
        plot.subtitle = element_markdown(hjust = 0, 
                                         size = 14, 
                                         family = "Roboto-Regular",
                                         lineheight=1),
        plot.caption = element_text(color = "#fdfdfd", size = 8),
        plot.background = element_rect(fill = "#121212",
                                       color = "#121212"),
        panel.background = element_rect(fill = "#121212",
                                        color = "#121212"))+
  ggsave(here::here("plots", "tdf.png"), dpi = 320, width = 14, height = 10, scale = 1)




