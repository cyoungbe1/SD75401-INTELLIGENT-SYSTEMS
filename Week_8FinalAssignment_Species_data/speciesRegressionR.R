getwd()
#check working directory

#import csv file
species_dataSet1 <- read.csv("species.csv", stringsAsFactors = TRUE)

str(species_dataSet1)


install.packages("ggplot2")
install.packages("tidyverse")


summary(species_dataSet1$p1.width)


hist(species_dataSet1$p1.width)

table(species_dataSet1$p1.width)

table(species_dataSet1$variety)

cor(species_dataSet1[c("s1.length","s1.width","p1.length", "p1.width")])

pairs(species_dataSet1[c("s1.length","s1.width","p1.length", "p1.width", "variety")])

install.packages("psych")
library(psych)

pairs.panels(species_dataSet1[c("p1.width", "p1.length", "s1.width","s1.length", "variety")])

species_data_model <- lm(p1.width ~ . , data = species_dataSet1)

species_data_model

summary(species_data_model)

agg_tbl <- species_dataSet1 %>% group_by(variety) %>% 
  summarise(mean_width=mean(p1.width),
            .groups = 'drop')


#end of question 1




