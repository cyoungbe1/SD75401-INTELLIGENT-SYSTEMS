getwd()
#check working directory

#import csv file
species_dataSet2 <- read.csv("species.csv", stringsAsFactors = TRUE)


normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

# create training and test data
species_dataSet2_train <- species_dataSet2[1:100, ]
species_dataSet2_test <- species_dataSet2[101:150, ]

# create labels for training and test data

species_dataSet2_train_labels <- species_dataSet2[1:100, 1]
species_dataSet2_test_labels <- species_dataSet2[101:150, 1]

## Step 3: Training a model on the data ----

# load the "class" library
--install.packages("class")
library(class)

species_dataSet2_test_pred <- knn(train = species_dataSet2_train, test = species_dataSet2_test,
                      cl = species_dataSet2_train_labels, k = 12)

