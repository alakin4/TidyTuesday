library(tidyverse)
#remotes::install_github("wilkelab/ggtext", force = TRUE)
#install.packages("devtools")
#devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
library(ggtext)
#remotes::install_github("PMassicotte/ggpmthemes", force = TRUE)
library(ggpmthemes)
library(here)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

animal_products <- c('Pork', 'Poultry','Beef', 'Lamb & Goat','Fish', 'Eggs', 'Milk - inc. cheese')
non_animal_products <- c('Wheat and Wheat Products', 'Rice', 'Soybeans', 'Nuts inc. Peanut Butter')

# Getting the food categories
#food_consumption %>% filter(!is.na(food_category)) %>% count(food_category , sort = TRUE)

country_co2_per_kg <- food_consumption %>% 
  mutate(animal_or_non = ifelse(food_category %in% 
                                  animal_products, 'animal_products','non_animal_products'))%>%
  group_by(country, animal_or_non) %>%
  dplyr::summarise(total_consumption = sum(consumption, na.rm = TRUE),
         total_co2_emmission = sum(co2_emmission, na.rm = TRUE),
         co2_per_kg = total_co2_emmission/total_consumption
         )

world_average<-food_consumption %>% 
  mutate(animal_or_non = ifelse(food_category %in% 
                                  animal_products, 'animal_products','non_animal_products'))%>%
  group_by(animal_or_non) %>%
  dplyr::summarise(total_consumption = sum(consumption, na.rm = TRUE),
                   total_co2_emmission = sum(co2_emmission, na.rm = TRUE),
                   co2_per_kg = total_co2_emmission/total_consumption
  )%>% 
  mutate(country ='World average')

country_co2_per_kg_wide <- country_co2_per_kg %>% 
  select(country,animal_or_non, co2_per_kg) %>% 
  spread(animal_or_non, co2_per_kg)%>% 
  mutate(diff_animal_or_non = animal_products-non_animal_products)%>% 
  arrange(desc(diff_animal_or_non))

#Largest and smallest differences in co2 per kg of animal products and non-animal products
lower_part <- tail(country_co2_per_kg_wide)
upper_part <- head(country_co2_per_kg_wide)

biggest_smallest <-c('Rwanda','Swaziland','Argentina' ,'Zambia','Bolivia', 'Malawi',
                     'Myanmar','Nicaragua','Maldives','Thailand','India' , 'Sri Lanka')

df_biggest_smallest<-country_co2_per_kg%>%
                    filter(country %in% biggest_smallest)%>%
                    arrange(match(country, biggest_smallest))
df_biggest_smallest<-bind_rows(list(df_biggest_smallest, world_average))

# make an ordered factor
df_biggest_smallest$country <- fct_rev(factor(df_biggest_smallest$country, levels = c('Rwanda','Swaziland','Argentina' ,'Zambia','Bolivia', 'Malawi', 'World average',
                                                                              'Myanmar','Nicaragua','Maldives','Thailand','India' , 'Sri Lanka')))
# visualisation  591527 + 90% saturation -->#68051f
my_title <- "**<span style='font-size:19pt'> CO<sub>2</sub> Emission from comsuming 1 Kg of <span style='color:#6c011d;font-size:20pt'>Animal</span> and <span style='color:#4E737B'>Non-Animal</span> food products</span>**"

my_subtitle <- "***A selection of the countries with the highest and lowest difference in  CO<sub>2</sub> emission from animal and non-animal <br>food products.***"
theme_set(theme_light_modified(base_family = "JetBrains Mono Bold"))

color_world_average<- c("gray60", "gray60", "gray60", "gray60" ,
           "gray60","gray60" ,"gray80", "gray60","gray60" ,"gray60" ,"gray60" ,
           "gray60")
df_biggest_smallest %>%
  ggplot(aes(y = co2_per_kg, x = reorder(country, co2_per_kg), fill = animal_or_non)) +
  geom_col(width =  0.8) +
  coord_flip() +
  scale_y_continuous() +
  xlab(NULL) +
  ylab("CO2 emmisions (Kg)") +
  labs(
    title= my_title,
    subtitle = my_subtitle,
    caption = "Tidytuesday week #8 | Data: www.nu3.de | @kinenealan"
  )+
  theme(
    legend.position = "none",
    text = element_text(color = "gray50", size = 14),
    plot.background = element_rect(fill = "#1F2227"),
    panel.background = element_rect(fill = "#1F2227"),
    axis.text.y = element_text(color = color_world_average, size = 14, margin = margin(r = 8)),
    axis.text.x = element_text(color = "gray60", size = 14, margin = margin(t = 10, b = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(color = "gray50", size = 0.3),
    plot.title = element_markdown(hjust = 0, family = "JetBrains Mono"),
    plot.subtitle = element_markdown(hjust = 0, family = "JetBrains Mono", margin = margin( b = 20), size = 11),
    plot.caption = element_text(color = "gray60", size = 10)
  )+
  scale_fill_manual(values = c('#591527', '#4E737B'))+
  ggsave(here::here("plots", "food-consumption-emmissions.png"), dpi = 320, width = 14, height = 10, scale = 1)

