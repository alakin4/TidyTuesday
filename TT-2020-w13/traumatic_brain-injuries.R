library(tidyverse)
library(ggplot2)
library(ggtext)
library(glue)
library(ggpmthemes)
library(here)

# Data
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')


# Exploration 
tbi_year%>%
  distinct(year)

tbi_year%>%
  distinct(injury_mechanism)

#-------------------------------
causes_recode<-c('Motor vehicle crashes' = 'Motor vehicle',
                 'Unintentional falls' = 'Falls',
  'Unintentionally struck by or against an object'= 'Struck By/Against', 
  'Other unintentional injury, mechanism unspecified'= 'Unknown/Other',
  'Intentional self-harm' = 'Unknown/Other',                         
  'Assault'= 'Assaults',                                    
  'Other or no mechanism specified'= 'Unknown/Other',
'Total'= 'Unknown/Other')

color_recode<-c( 'Motor vehicle' = 'gray30',
                  'Falls'= '#d7433f',
                 'Struck By/Against' = 'gray30', 
                  'Unknown/Other' = 'gray30',                        
                 'Assaults'= 'gray30')



tbi_year<-tbi_year%>%
  mutate(cause = recode(injury_mechanism, !!!causes_recode))
         
tbi_year_annual<-tbi_year%>%
  group_by(year)%>%
  summarise(sum_est = sum(number_est, na.rm = TRUE))

#Abit of cheating from the plot: Falls are the commonest cause of tbi
#which age group comprises of this number?
tbi_age%>%
  distinct(injury_mechanism)

tbi_age<-tbi_age%>%
  mutate(injury_mechanism_2=str_to_sentence(injury_mechanism),
         cause = recode(injury_mechanism_2, !!!causes_recode))%>%
  filter(cause ==  'Falls')%>%
  group_by(age_group)%>%
  summarise(sum_est = sum(number_est, na.rm = TRUE))%>%
  filter(age_group != 'Total')%>%
  arrange(sum_est)%>%
  mutate(sum_percent = (sum_est/sum(sum_est))*100)

#plot 
my_title <-'Traumatic Brain Injuries (2006-2014)'
my_subtitle = glue::glue("The total number of Traumatic Brain Injuries (TBIs) increased steadily. The increase is strongly attributed to
the increase in the number of<br>injuries caused by Falls. Other causes of TBIs include: Struck By/Against, Unknown/Other, Assaults, Motor vehicle. Most of the injuries 
<br>caused by falls occur in the age groups 0-17 (**22.7%**) and 75+ (**20.7%**).")
theme_set(theme_light_modified(base_family = "JetBrains Mono"))
#-----
tbi_year_annual_cause<-tbi_year%>%
  filter(injury_mechanism != 'Total')%>%
  group_by(year,cause)%>%
  summarise(sum_est = sum(number_est, na.rm = TRUE))%>%
  mutate(used_color = recode(cause, !!!color_recode))

# where to place the in-plot label
year_final <- tbi_year_annual_cause%>%
  filter(year == 2014)

#plot 2 all injuries
ggplot(data = tbi_year_annual_cause,
       mapping = aes(x = year, y = sum_est/100000))+
  geom_line(aes(color = cause),
            size=1)+
  scale_color_manual(
    values = color_recode)+
  geom_line(data = tbi_year_annual,
            mapping = aes(x = year, y = sum_est/100000),
            color="#4d9cef",
             size=1) +
  labs(
    title= my_title,
    subtitle = my_subtitle,
    caption = "Tidytuesday week #13 | Data: CDC | @kinenealan"
  )+
  xlab('Year')+
  ylab("Injuries ('00,000)")+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(2006, 2007, 
               2008, 2009,
               2010, 2011,
               2012, 2013,
               2014),
    minor_breaks=NULL
  )+scale_y_continuous(
    limits = c(0, 30), 
    expand = c(0, 0),
    breaks = c(0,5,10,15,20,25,30),
    labels  = c('0','5','10','15','20','25','30')
  )+
  annotate(geom = "richtext", 
           x = 2014.2, 
           y =2900000/100000, 
           label = "<span style='color:#4d9cef;'>**Total**</span>",
           fill = NA, label.color = NA,
           hjust = 0, 
           size = 5) +
  annotate(geom = "richtext", 
           x = 2014.2, 
           y =1380000/100000, 
           label = "<span style='color:#d7433f;'>**Falls**</span>", 
           fill = NA, label.color = NA,
           hjust = 0, 
           size = 5) +
  annotate(geom = "richtext", 
             x = 2014.2, 
             y =350000/100000, 
             label = "**Struck By/Against,<br>Unknown/Other,<br>Assaults,<br>Motor vehicle**",
            fill = NA, label.color = NA,
             color = "gray30",
             hjust = 0, 
             size = 5,
            lineheight=1) +
  coord_cartesian(xlim = c(2006, 2015.4), # This focuses the x-axis on the range of interest such that the annotations are not cut off
                  clip = 'off')+
  theme(legend.position = 'none',
        text = element_text(color = "#fcfcfc", size = 14),
        plot.margin = margin(10, 55, 10, 10),
        plot.background = element_rect(fill = "#262626"),
        panel.background = element_rect(fill = "#262626", linetype = 'blank'),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(color = "#fcfcfc", margin = margin(t = 10, b = 10)),
        axis.text.y = element_text(color = "#fcfcfc", vjust = -0.5, hjust = 2.38),
        axis.title = element_text(margin = margin(t = 10),face="bold"),
        axis.ticks.y = element_line(colour="#fcfcfc",size = 0.3),
        axis.ticks.length.y = unit(1, "cm"),
        plot.title = element_markdown(hjust = 0, ,color = "#fcfcfc", family = "JetBrains Mono Bold", size = 28),
        plot.subtitle = element_markdown(hjust = 0 , size = 14, family = "IBM Plex Sans",margin = margin(b = 30),lineheight=1),
        plot.caption = element_text(hjust = 1 ,color = "gray50", size = 10))+
  geom_segment(aes(x=2005.7,xend=2014.2,y=0,yend=0), color="#fcfcfc", size = 0.05)+# Doing a manual hack because I don't know a smart way so solve long axis grids caused by annotation
  ggsave(here::here("plots", "traumatic_brain_injuries.png"), dpi = 320, width = 14, height = 10, scale = 1)

#-----

                                                                              