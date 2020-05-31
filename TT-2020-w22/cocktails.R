library(tidyverse)
library(janitor)
library(here)
library(paletteer)
library(patchwork)
library(reactable)
library(gt)
library(sparkline)
library(htmltools)
library(glue)



#https://vinepair.com/articles/big-shot-glass-answer-depends/
#https://www.calculateme.com/volume/fluid-ounces/to-tablespoons/
# https://omnipotent.net/jquery.sparkline/#pie
# https://en.wikibooks.org/wiki/Bartending/Glossary/Table_of_measures_and_conversions

#Data cleaning

selected_cocktails<-c("Dirty Martini",
                      "Espresso Martini",
                      "Planter's Punch",
                      "Mojito",
                      "Martini",
                      "Bloody Mary",
                      "Derby",
                      "Royal Gin Fizz",
                      "Irish Coffee",
                      "Bramble")
                      
df_selected_cocktails<-cocktails%>%
  select(drink,category,glass,ingredient,measure)%>%
  filter(drink%in%selected_cocktails)%>%
  mutate(ingredient=str_to_title(ingredient))%>%
  filter(str_detect(ingredient, 
                    "Sugar|Simple Syrup|Gin|Whiskey|Vodka"))%>% #|(V|v)odka
  mutate_at(c('measure'),~if_else(.=="70ml/2fl oz",'2 fl oz',.))%>%
  mutate(measure_size = str_replace(measure, "oz|cl|dash|fl oz|tsp", ""),
         units = gsub("^.* ","", measure))%>%
  separate(measure_size, c('measure_size_a','measure_size_b'), sep = " ")%>%
  mutate_at(c('measure_size_a','measure_size_b'),~ifelse(.=='1/2',
                                                         0.5,
                                                         ifelse(.=='2/3',
                                                                0.67,
                                                                as.numeric(.))))%>%
  mutate_at(c('measure_size_a','measure_size_b'),~ifelse(is.na(.), 0, as.numeric(.)))%>%
  mutate(measure_size=measure_size_a+measure_size_b,
         conversion_rate=case_when(
           units == "cl" ~ 0.34,
           units == "dash" ~ 0.03 ,
           units == "tsp" ~ 0.17,
           TRUE ~ 1),
         measure_size_oz= measure_size*conversion_rate,
         measure_size_a=NULL,
         measure_size_b=NULL)

# Get Percentages
df_selected_cocktails<-df_selected_cocktails%>%
  group_by(ingredient)%>%
  mutate(level_percent =round((measure_size_oz/max(measure_size_oz))*100,2))%>%
  ungroup()%>%
  #group similar ingredients together
  mutate(used_ingredient =case_when(
           str_detect(ingredient,"Gin")~ "Gin",
           str_detect(ingredient,"Sugar") ~ "Sugar",
           str_detect(ingredient,"Whiskey") ~ "Whiskey",
           TRUE ~ ingredient))

# change to wide
df_selected_cocktails<-df_selected_cocktails%>%
  select(drink,used_ingredient, level_percent)%>%
  pivot_wider(names_from = used_ingredient, 
              values_from = level_percent)%>%
  mutate_all(~ifelse(is.na(.),0,.))


background_color = '#172e2b' #greenish
primary_color = '#f28b82' #redish
secondary_color='#ffbeae' #pinkish
draw_pie <- function(.x) {
  plot_part = .x
  other_part= 100-plot_part
  sparkline(c(plot_part,other_part), 
            type = 'pie', 
            sliceColors = c('#f28b82','#172e2b'),
            offset = -90, 
            width= '2em')
}

# Learning alot from the documentation at:
# https://glin.github.io/reactable/articles/examples.html#column-formatting-1 and
# https://connorrothschild.github.io/recreating-a-table-by-the-economist-using-reactable
cocktailtable<-df_selected_cocktails%>%
  reactable(fullWidth = FALSE,
            showSortIcon = FALSE,
            compact = TRUE,
            pagination = FALSE,
            outlined=FALSE,
            defaultColDef = colDef(
                      ### define header styling
                      headerStyle = list(
                      textAlign = "left",
                      fontSize = "20px",
                      lineHeight = "14px",
                      textTransform = "capitalize",
                      color = primary_color,
                      fontWeight = "500",
                      borderBottom = "2px solid #ffbeae",
                      borderTop = "2px solid #ffbeae",
                      paddingBottom = "10px",
                      paddingTop = "10px",
                      verticalAlign = "center",
                      fontFamily = 'Asul-Regular'
                      ),
                      ### define default column styling
                      style = list(fontFamily = 'Asul-Regular',
                        fontSize = "20px",
                        verticalAlign = "center",
                        align = "center",
                        textAlign = "center"
                      )
              ),
            
            #MAKE PIES
            columns = list(
              drink = colDef(width = 140,
                              name='Cocktail',
                              style = list(fontSize = "16px",
                                          textAlign = "left",
                                          fontFamily = 'Asul-Regular')),
              Sugar = colDef(cell = draw_pie,
                             style = list(fontSize = "20px",
                                          textAlign = "center")),
              Vodka = colDef(cell = draw_pie),
              Gin = colDef(cell = draw_pie),
              Whiskey = colDef(cell = draw_pie,
                               style = list(fontSize = "20px",
                                            textAlign = "center"))),
            theme = reactableTheme(backgroundColor = background_color,
                                   borderColor=background_color,
                                   color=primary_color,
                                   rowStyle=list(borderBottom = "0.5px dotted #ffbeae"))
            )

cocktailtable
#ADD title and subtile


# A CSS dependency


html_print(div(class = "tableTitle",
               style ='width:540px;
               padding: 3px;
               background:#172e2b',
    div(
      class = "title", 
      style='text-align:center; font-family:Roboto',
      h2("Choose Your Cocktail Poison",style='color:#f28b82; 
            font-size:20px;
            text-transform:uppercase' ),
      div(style="display:flex;
          width:90%;
          color:#f28b82;
          column-gap: 0px;
          padding-left:10%",
          p(draw_pie(75), style="padding-left:0.5rem;"),
          p("indicates the amount (in oz) for the cocktail ingredients*", style="padding:0rem")
          )),
    cocktailtable, 
    div(class = "footer2",
        p(style="font-size:10px;
          color:#f28b82;
          text-align:center",
          "Tidytuesday week #22 | Data: www.kaggle.com  | @kinenealan"))))
#Piedra-Regular
# I manually saved the image from he viewer
