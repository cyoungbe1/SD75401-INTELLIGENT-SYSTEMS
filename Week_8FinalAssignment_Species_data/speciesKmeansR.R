getwd()
#check working directory

#import csv file 
species_dataSet3 <- read.csv("species.csv")
str(species_dataSet3)

View(species_dataSet3)
nrow(species_dataSet3)

# create a vector with the average p1.width for each variety
average_width <- ave(species_dataSet3$p1.width, species_dataSet3$variety,
                   FUN = function(x) mean(x, na.rm = TRUE))

View(average_width)
species_dataSet3$p1.width <- ifelse(is.na(species_dataSet3$p1.width), average_width, species_dataSet3$p1.width)

summary(species_dataSet3$p1.width)


## Step 3: Training a model on the data ----
species_dataSet3_all <- species_dataSet3[1:4] #exclude the first four variables
View(species_dataSet3_all)

#standardize the data so that they have a mean of 0 and st of 1
species_standardized <- as.data.frame(lapply(species_dataSet3_all, scale))
View(species_standardized)

set.seed(3456)  
#the set.seed() function is used to ensure that the results match
#the output in the examples that follow"

species_clusters <- kmeans(species_standardized, 4)

## Step 4: Evaluating model performance ----
# look at the size of the clusters
species_clusters$size

# look at the cluster centers
species_clusters$centers[,1:4 ] #"lets look at the data of first 4 columns"

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
species_clusters$cluster <- species_clusters$cluster

# look at the first five columns
species_dataSet3[1:4, c("s1.length", "s1.width", "p1.length", "p1.width")]