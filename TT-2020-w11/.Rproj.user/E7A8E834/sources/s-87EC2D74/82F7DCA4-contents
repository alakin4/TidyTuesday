library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggpmthemes)
library(osmdata)
library(sf)
library(maps)
#library(mapproj)
library(here)

#Data for massachusets state
# measles data, only positive mmr values
# Below 98.3, the state average
# source: https://www.cdc.gov/vaccines/imz-managers/coverage/childvaxview/data-reports/mmr/trend/index.html
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')
massachusetts_measles <- measles%>%
                        filter(state=='Massachusetts' & mmr >=0 & mmr < 98.3)

#
# massachusets county data
us_mass_counties <- map_data("county") %>% 
  filter(region=='massachusetts')%>%
  mutate(state_county = str_c(region, subregion, sep = "_"))
# private and Public schools with the minimum mmr 
lowest_rate <-massachusetts_measles%>%
  group_by(type)%>%
  slice(which.min(mmr))%>%
  select(name, type, city, county, mmr,lng,lat)

# proportion of each school type
all_mmr_schools <-measles%>%
  filter(state=='Massachusetts' & mmr >=0)%>%
  group_by(type)%>%summarise(count = n())

below_average_mmr_schools<-massachusetts_measles%>%
  group_by(type)%>%summarise(count = n())
# 25/54*100 = 46.3%
# 444/900*100 = 49.3%
# 
# Plot
my_title <- "Measles, Mumps, and Rubella (MMR) vaccination rate in Massachusetts, US"
my_subtitle <- "***A look at <b><span style = 'color:#ff6347;'>Private</span></b> and <b><span style = 'color:#4E737B;'>Public</span></b> schools with MMR vaccination rate below the state average (<b>98.3%</b>)***"
theme_set(theme_light_modified(base_family = "JetBrains Mono Bold"))
# 50%, 25%
ggplot() + 
  geom_polygon(data = us_mass_counties,
               mapping = aes(x = long, y = lat, group = group),
               size = 0.2, 
               fill = "#f5f5f2", 
               color = "black") +
  geom_point(data = massachusetts_measles,
             mapping = aes(x = lng, y = lat, color = type),
             size = 2)+
  guides(fill = FALSE)+
  xlab(NULL) +
  ylab(NULL) +
  labs(
    title= my_title,
    subtitle = my_subtitle,
    caption = "Tidytuesday week #9 | Data: WSJ | @kinenealan"
  )+ 
  annotate(
    geom = "curve", x = -73.6, y = 41.92, xend = -73.42, yend = 42.19, 
    curvature = -0.1, arrow = arrow(length = unit(1.2, "mm")))+
  annotate(geom = "text", x = -73.6, y = 41.88, label = "Rudolf Steiner\nschool", color = "#ff6347",hjust = "left", size = 5)+
  annotate(
    geom = "curve", x = -70.5, y = 42.49, xend = -71.1, yend = 42.4, 
    curvature = -0.1, arrow = arrow(length = unit(0.7, "mm")))+
  annotate(geom = "text", x = -70.3, y = 42.49, label = "Cambridgeport\nschool", color = "#4E737B",vjust = "bottom", size = 5)+
  theme_void()+
  theme(
    legend.position = "none",
    text = element_text(color = "black", size = 14),
    plot.background = element_rect(fill = "#f5f5f3"),
    panel.background = element_rect(fill = "#f5f5f3", linetype = 'blank'),
    plot.margin = margin(10, 40, 20, 80),
    plot.title = element_markdown(hjust = 0, family = "JetBrains Mono", size = 21, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0, family = "JetBrains Mono", margin = margin( t = 10, b = 20), size = 15),
    plot.caption = element_text(color = "black", family = "JetBrains Mono", size = 8)
  )+
  scale_color_manual(values = c('#ff6347', '#66949e'))+
  annotate("text",
           x = -73.3, 
           y = 41.48, 
           label = "46.3% and 49.3% of the private and public schools, \nrespectively, have MMR vaccination rate below the \nstate average. Rudolf Steiner school in Berkshire \ncounty has the lowest MMR vaccination rate (3%) \namong private schools whereas Cambridgeport school \nin Middlesex county has the lowest MMR vaccination \nrate (48%) among public schools.", 
           hjust = 0, family = "JetBrains Mono", size = 5)+
  ggsave(here::here("plots", "measles_mass.png"), dpi = 320, width = 14, height = 10, scale = 1)
