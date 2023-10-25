getwd()
#check working directory

#import csv file 
housetrain_data <- read.csv("house_train.csv", stringsAsFactors = TRUE)


housetrain_data <- read.csv("house_train.csv")

str(housetrain_data)


install.packages("ggplot2")
install.packages("tidyverse")

dataFrame <- read.csv("house_train.csv")

summary(housetrain_data)

summary(housetrain_data$price2007)

hist(housetrain_data$price2007)

table(housetrain_data$zip)

cor(housetrain_data[c("id","zip","poverty", "price2007")])

pairs(housetrain_data[c("id","zip","poverty","price2007")])

install.packages("psych")
library(psych)

pairs.panels(housetrain_data[c("id","zip","poverty", "price2007")])

housing_data_model <- lm(price2007 ~ . , data = housetrain_data)

housing_data_model_reduced <- lm(price2007 ~ id + zip + poverty , data = housetrain_data)
housing_data_model_reduced_state <- lm(price2007 ~ state + zip + poverty , data = housetrain_data)

housing_data_model_reduced

housing_data_model

housing_data_model_reduced_state

agg_tbl <- dataFrame %>% group_by(state) %>% 
  summarise(mean_price=mean(price2007),
            .groups = 'drop')

housing_data_model_2013 <- lm(price2013 ~ state + county, data = housetrain_data)

housing_data_model_2013

sort(coef(housing_data_model_2013), decreasing=TRUE)



