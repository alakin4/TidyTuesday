library(tidyverse)

trial_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

#tifyverse functions
streets <- trial_data %>% 
  filter(latitude < 40 & latitude > 37.7)%>% 
  mutate(address = str_replace(address, "@", " "))%>% 
  separate(address, sep = "^\\d+[Xx-]* *(- *\\d+ )*", c("number", "street"), remove = FALSE)%>% 
  add_count(street) %>% 
  filter(n > 1000)

#mapping function
library(osmdata)
san_francisco <- getbb("San Francisco")%>%
  opq()%>%
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = san_francisco$osm_lines, color = "grey10", size = 0.1, alpha = 0.8) +
  geom_point(data = streets, aes(longitude, latitude, color = street, size = n), size = 0.15)+
  scale_colour_viridis_d(guide = guide_legend(title = "Streets in San Francisco with\nthe most DPW-maintained trees", override.aes = list(size = 2))) +
  coord_sf(xlim = c(-122.51, -122.34), ylim = c(37.7, 37.81))+
  labs(
    caption = "Source: DataSF | Graphic: Alan Kinene"
  )+ theme_void(base_family = "JetBrains Mono Bold")+
  theme(
    legend.title = element_text(size = 13, color = "grey10"),
    legend.text = element_text(size = 12, color = "grey70"),
    plot.background = element_rect(colour = NA, fill = "grey45"),
    plot.margin = margin(10, 40, 20, 80),
    plot.caption = element_text(family = "JetBrains Mono", margin = margin(10, 0, 0, 0))
  )
#------------------------------------