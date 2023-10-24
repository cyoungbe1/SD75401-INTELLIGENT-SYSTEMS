getwd()
#check working directory

#import csv file 
teenInterests <- read.csv("snsdata.csv")
str(teenInterests)

View(teenInterests)
nrow(teenInterests)

# look at missing data for female variable
table(teenInterests$gender)
table(teenInterests$gender, useNA = "ifany")  #2724 row has no gender information

summary(teenInterests$age)

#limit age range (13-20) for keeping relevant data
teen_marketData <- ifelse(teen_marketData$age >= 13 & teen_marketData < 20, teen_marketData$age, NA)

class(teen_marketData)


summary(teenInterests$age)

# reassign missing gender values to "unknown"
teenInterests$female <- ifelse(teenInterests$gender == "F" &
                                  !is.na(teenInterests$gender), 1, 0)
teenInterests$no_gender <- ifelse(is.na(teenInterests$gender), 1, 0)

View(teenInterests)

# check our recoding work
table(teenInterests$gender, useNA = "ifany")
table(teenInterests$female, useNA = "ifany")
table(teenInterests$no_gender, useNA = "ifany")

# finding the mean age by cohort
mean(teenInterests$age)                         # doesn't work
mean(teenInterests$age, na.rm = TRUE)          # works

# age by cohort
aggregate(data = teenInterests, age ~ gradyear, mean, na.rm = TRUE) #age as a function of Grad Year


# create a vector with the average age for each gradyear, repeated by person
average_Age <- ave(teenInterests$age, teenInterests$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))

View(average_Age)
teenInterests$age <- ifelse(is.na(teenInterests$age), average_Age, teenInterests$age)

summary(teenInterests$age)


## Step 3: Training a model on the data ----
teenInterests_INTERESTS <- teenInterests[5:40] #exclude the first four variables
View(teenInterests_INTERESTS)

#standardize the data so that they have a mean of 0 and st of 1
interests_standardized <- as.data.frame(lapply(teenInterests_INTERESTS, scale))
View(interests_standardized)

set.seed(3456) #"Because the k-means algorithm utilizes random starting points, 
#the set.seed() function is used to ensure that the results match
#the output in the examples that follow"

teenInterests_clusters <- kmeans(interests_standardized, 3)

## Step 4: Evaluating model performance ----
# look at the size of the clusters
teenInterests_clusters$size

# look at the cluster centers
teenInterests_clusters$centers[,1:15 ] #"lets look at the data of first 4 columns"

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
teenInterests$cluster <- teenInterests_clusters$cluster

# look at the first five records
teenInterests[1:5, c("cluster", "gender", "age", "friends")]





