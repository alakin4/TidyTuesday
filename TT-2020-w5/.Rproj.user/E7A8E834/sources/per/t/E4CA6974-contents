library(tidyverse)
my_data <-relig_income[1:3,1:5]
colnames(my_data)<-c("respondent","age1","age2","var_1", "var_2")

#https://tidyr.tidyverse.org/reference/pivot_longer.html
pivot_longer(my_data, c("age1","age2","var_1", "var_2"), 
             names_to = c(".value", "wave"), 
             names_pattern = "(.+)(.+)"
)
