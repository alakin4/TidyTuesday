library(tidyverse)
library(ggplot2)
library(ggtext)
library(glue)
library(ggpmthemes)
library(lubridate)
library(here)
library(patchwork)

#---
#DATA
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')%>% 
  filter(!str_detect(material_type, "Total")) %>% 
  mutate(month = ymd(str_glue("{year}-{str_pad(month, width = 2, pad = '0')}-01")))
  

# What is in the bear you drink?
df_grain_nongrain <- brewing_materials %>% 
  group_by(material_type) %>% 
  summarise(amount_pounds = sum(month_current, na.rm = TRUE)) %>% 
  mutate(material_share = round(amount_pounds / sum(amount_pounds)*100,1)) %>% 
  ungroup()%>%
  arrange(desc(material_share))
df_grain_nongrain$material_type <- fct_reorder(factor(df_grain_nongrain$material_type), 
                                       df_grain_nongrain$material_share,
                                            .desc = FALSE)
#---------
# Explore the components of both Grain and Non-Grain
df_grain <- brewing_materials %>% 
  filter(material_type == 'Grain Products')%>%
  group_by(type) %>% 
  summarise(amount_pounds = sum(month_current, na.rm = TRUE)) %>% 
  mutate(material_share = round(amount_pounds / sum(amount_pounds)*100,1)) %>% 
  ungroup()%>%
  arrange(desc(material_share))
df_grain$type <- fct_reorder(factor(df_grain$type), 
                                      df_grain$material_share,
                                               .desc = FALSE)

df_nongrain <- brewing_materials %>% 
  filter(material_type == 'Non-Grain Products')%>%
  group_by(type) %>% 
  summarise(amount_pounds = sum(month_current, na.rm = TRUE)) %>% 
  mutate(material_share = round(amount_pounds / sum(amount_pounds)*100,1))%>% 
  ungroup()%>%
  arrange(desc(material_share))
df_nongrain$type <- fct_reorder(factor(df_nongrain$type), 
                                         df_nongrain$material_share,
                                               .desc = FALSE)
#--------
# Make the plot
theme_set(theme_light_modified(base_family = "Roboto-Regular"))
grain_nongrain_pal<-c('#3c3126', '#476d95')
df_grain_nongrain$one_bar<-'one_bar'
plot_grain_nongrain<-ggplot(df_grain_nongrain, aes(fill=material_type, 
                              y=material_share, 
                              x=one_bar)) + 
  geom_bar(position="stack", 
           stat="identity",
           width = 0.3,
           color = 'grey',
           size = 0.2)+
  scale_y_continuous(expand = c(0, 0))+
  xlab(NULL)+
  ylab(NULL)+
  scale_fill_manual(
    values = rev(grain_nongrain_pal))+
  coord_flip()+
  geom_richtext(aes(label = glue::glue("<span style='color:#ffffff;'>**{material_type}**<br>{material_share}%</span>")),
            position = position_stack(vjust = 0.5),
            fill = NA, label.color = NA,
            hjust = 0.5, 
            size = 4.3
            )+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none',
        text = element_text( family = "Roboto-Regular"))
# grain = reddish Grain Products 83.5%
# Non-grain= bluish Non-Grain Products 15.5%
plot_grain_nongrain
# --------
# Second plots:: Grain materials
non_grain_pal<-c('#476d95', '#6784a6',
                 '#859bb7', '#a3b3c9')
df_nongrain$one_bar<-'one_bar'
subtitle_1 <-"<span style='color:#476d95;'>Non-Grain Products</span>"
plot_nongrain<-ggplot(df_nongrain, aes(fill=type,
                     y=material_share,
                     x=one_bar)) + 
  geom_bar(position="stack", 
           stat="identity",
           width = 0.3,
           color = 'grey',
           size = 0.1)+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(
    values = rev(non_grain_pal))+
  xlab(NULL)+
  ylab(NULL)+
  labs(
    title= subtitle_1)+
  coord_flip()+
  annotate(geom = "richtext", 
           x = 1, 
           y =37.5,
           label = "<span style='color:#ffffff;'>**Sugar and syrups**<br> 75.4%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  annotate(geom = "richtext", 
           x = 1, 
           y =82,
           label = "<span style='color:#ffffff;'>**Hops<br>(dry)**<br>12.9%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  annotate(geom = "richtext", 
           x = 1, 
           y =94,
           label = "<span style='color:#ffffff;'>**Other**<br>11.3%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  annotate(
    geom = "curve", x = 0.85, y = 100, xend = 0.75, yend = 95, 
    curvature = -0.2, arrow = arrow(length = unit(0.5, "mm")),
    color = '#000000',
    size = 0.5)+
  annotate(geom = "richtext", 
           x = 0.65, 
           y =86,
           label = "<span style='color:#000000;'>**Hops<br>(used as extracts)**<br>0.4%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none',
        text = element_text(color = "#fcfcfc", size = 14),
        plot.title = element_markdown(hjust = 0.5,color = "#000000", family = "Asul Bold", size = 28)
  )

# Sugar and syrups 75.4%
# Hops (dry) 12.9%
# Other 11.3%
# Hops (used as extracts) 0.4%
plot_nongrain
#-------------------------------------------------
# Second plots:: Grain materials
grain_pal <-c('#b8926a', '#977858',
              '#775f46', '#584736',
              '#3c3126')
df_grain$one_bar<-'one_bar'
subtitle_2 <-"<span style='color:#3c3126;'>Grain Products</span>"
Note<-'All materials include their products,<br>e.g.,
       malt and malt products.'
plot_grain<-ggplot(df_grain, aes(fill=type,
                     y=material_share,
                     x=one_bar)) + 
  geom_bar(position="stack", 
           stat="identity",
           width = 0.3,
           color = 'grey',
           size = 0.1)+
  scale_fill_manual(
    values = grain_pal)+
  scale_y_continuous(expand = c(0, 0))+
  xlab(NULL)+
  ylab(NULL)+
  labs(
    title= subtitle_2,
    subtitle = Note)+
  coord_flip()+
  annotate(geom = "richtext", 
           x = 1, 
           y =37.5,
           label = "<span style='color:#ffffff;'>**Malt**<br>73.1%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  annotate(geom = "richtext", 
           x = 1, 
           y =79,
           label = "<span style='color:#ffffff;'>**Rice**<br>12.7%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  annotate(geom = "richtext", 
           x = 1, 
           y =92,
           label = "<span style='color:#ffffff;'>**Corn**<br>11.6%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  annotate(
    geom = "curve", x = 1.15, y = 98, xend = 1.3, yend = 95, 
    curvature = 0.2, arrow = arrow(length = unit(0.5, "mm")),
    color = '#000000')+
  annotate(geom = "richtext", 
           x = 1.3, 
           y =92,
           label = "<span style='color:#000000;'>**Barley**<br>2.1%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
  annotate(
             geom = "curve", x = 0.85, y = 100, xend = 0.75, yend = 98, 
             curvature = -0.2, arrow = arrow(length = unit(0.5, "mm")),
             color = '#000000',
             size = 0.5)+
  annotate(geom = "richtext", 
           x = 0.70, 
           y =92,
           label = "<span style='color:#000000;'>**Wheat**<br>0.5%</span>", 
           fill = NA, label.color = NA,
           hjust = 0.5, 
           size = 4.3)+
theme_minimal()+
theme(panel.grid = element_blank(),
      plot.margin = margin(1, 0, 10, 0),
      axis.text = element_blank(),
        legend.position = 'none',
      text = element_text(color = "#fcfcfc", size = 14),
      plot.title = element_markdown(hjust = 0.5,
                                    color = "#000000", 
                                    family = "Asul Bold", 
                                    size = 28),
      plot.subtitle = element_markdown(hjust = 0.5,
                                       color = "#000000",
                                       size = 12,
                                       family = "Roboto-Regular",
                                       lineheight=1)
      )
# Rice and rice products
# Corn and corn products
# Barley and barley products
# Wheat and wheat products
plot_grain
#-----
#Add the plots
my_title <-'Your beer most likely has malt, and<br>sugar and syrups'
my_subtitle = glue::glue("Grain products are more used in beer production than non-grain products. Malt and malt products are<br>the commonest grain products; while wheat and wheat
                         products make up the smallest percentage.<br>Sugar and syrups make up 75% of the non-grain products.")

(plot_grain_nongrain/(plot_grain|plot_nongrain) ) +
  plot_layout(widths = 1, heights  = 1)+
  plot_annotation(
    title = my_title,
    subtitle = my_subtitle,
    caption = "Tidytuesday week #14 | Data: www.ttb.gov/beer/statistics (2008-2017)  | @kinenealan",
    theme = theme(
      text = element_text( family = "Roboto-Regular"),
      plot.title = element_markdown(hjust = 0.5, family = "Asul Bold", size = 30),
      plot.subtitle = element_markdown(hjust = 0.5, 
                                       size = 13, 
                                       family = "Roboto-Regular",
                                       lineheight=1),
      plot.caption = element_text(color = "gray50", size = 10),
      plot.background = element_rect(fill = "#eae6df",
                                     color = "#eae6df"),
      panel.background = element_rect(fill = "#eae6df")
    )
  )+
  ggsave(here::here("plots", "beer_production.png"), dpi = 320, width = 14, height = 10, scale = 1)



