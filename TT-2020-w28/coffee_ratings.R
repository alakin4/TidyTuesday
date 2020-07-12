library(tidyverse)
library(ggforce)
library(ggtext)
library(glue)
library(ggpmthemes)
library(here)

# Data --------------------------------------------------------------------
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')


# Analysis between Arabica and Robusta rating -----------------------------

coffee_ratings%>%
  group_by(species)%>%
  summarise(avg_rating = mean(total_cup_points),
            proportion = n()/nrow(coffee_ratings))

#  Coffee: Arabica or Robusta?
#  Robusta coffee:
#  ~40% of coffee produced worldwide
#  1.7–4% caffeine.
#  80.9 average quality rating
#  
#   Arabica coffee:
#  ~60% of coffee produced worldwide
#  0.8–1.4% caffeine.
#  82.1 average quality rating

# Visualisation -----------------------------------------------------------

coffee_bean <- tribble(
  ~x, ~y,
  20,10.7,
  19.2, 10.2,
  20,10,
  20.2,9.8,
  20,9.5)

coffee_bean_2 <- tribble(
  ~x, ~y,
  35,11.1,
  32.5,10.6,
  35,10.2,
  35.4,9.7,
  35.1,9.5)

my_title <-'Coffee Beans: Robusta or Arabica?'
my_subtitle = glue::glue("There is a higher chance that your coffee is from Arabica beans because they have a higher<br>
                          worldwide production than Robusta beans. Arabica beans also have a higher average quality<br>
                          rating. However, they consist of lower caffeine than Robusta beans.")

Robusta_h2<-"<span style='color:#4c3d30;'>**Robusta Coffee**</span>"
Robusta_p<-"<span style='color:#4c3d30;'>Worldwide Production: **40%**<br>Caffeine: **1.7–4%**<br>
Av. quality rating: **80.9**</span>"

Arabica_h2<-"<span style='color:#4c3d30;'>**Arabica Coffee**</span>"
Arabica_p<-"<span style='color:#4c3d30;'>Worldwide Production: **60%**<br>Caffeine: **0.8–1.4%**<br>
Av. quality rating: **82.1**</span>"

ggplot() +
  geom_ellipse(aes(x0 = 20, y0 = 10, a = 3, b = 0.5, angle = 0),
               fill = '#4c3d30',
               color ='#4c3d30') +
  geom_bspline(data = coffee_bean,
               aes(x=x ,y=y),
               size=4, color='#eae6df', lineend = 'round')+
  geom_ellipse(aes(x0 = 35, y0 = 10.25, a = 3*1.5, b = 0.5*1.5, angle = 0),
               fill = '#4c3d30',
               color ='#4c3d30')+
  geom_bspline(data = coffee_bean_2,
               aes(x=x ,y=y),
               size=6, color='#eae6df', lineend = 'round')+
  annotate(
    geom = "curve", x = 33, y = 10.9, xend = 25.7, yend = 10.9, 
    curvature = 0,
    color = '#4c3d30', alpha = 0.8, linetype = 'dashed')+
  annotate(
    geom = "curve", x = 21, y = 9.54, xend = 28.8, yend = 9.54, 
    curvature = 0,
    color = '#4c3d30', alpha = 0.8, linetype = 'dashed')+
  annotate(geom = "richtext", 
           family = "Asul Bold",
           x = 22.9, 
           y =9.84, 
           label = Robusta_h2,
           color = "#3d3126",
           fill = NA, 
           label.color = NA,
           hjust = 0, 
           size = 9)+
  annotate(geom = "richtext", 
           x = 22.9, 
           y =9.68, 
           label = Robusta_p, 
           fill = NA, 
           label.color = NA,
           hjust = 0, 
           size = 7,
           lineheight=1.3)+
  annotate(geom = "richtext", 
           family = "Asul Bold",
           x = 25.7, 
           y =10.82, 
           label = Arabica_h2,
           color = "#3d3126",
           fill = NA, 
           label.color = NA,
           hjust = 0, 
           size = 9)+
  annotate(geom = "richtext", 
           x = 25.7, 
           y =10.65, 
           label = Arabica_p, 
           fill = NA, 
           label.color = NA,
           hjust = 0, 
           size = 7,
           lineheight=1.3)+
  xlab(NULL)+ylab(NULL)+
  labs(
    title= my_title,
    caption = "Tidytuesday week #28 | Data: Coffee Quality Database & USDA | @kinenealan",
    subtitle =  my_subtitle
  )+
  theme(
    text = element_text( family = "Roboto-Regular"),
    plot.title = element_markdown(hjust = 0.5, family = "Asul Bold", size = 36, color = "#3d3126"),
    plot.subtitle = element_markdown(hjust = 0.5 , size = 20, family = "Roboto-Regular",lineheight=1, color = "#3d3126"),
    plot.background = element_rect(fill = "#eae6df"),
    panel.background = element_rect(fill = "#eae6df"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(20,40, 20, 40),
    plot.caption = element_text(color = "gray50", size = 8,hjust = 0.5),
  )+
  ggsave(here::here("plots", "coffee_ratings.png"), dpi = 320, width = 16, height = 10, scale = 1)


