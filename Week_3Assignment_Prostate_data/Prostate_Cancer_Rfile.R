getwd()
#check working directory

#import csv file 
prostateCancer_data <- read.csv("Prostate_Cancer.csv",TRUE,",", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(prostateCancer_data)

# remove id var
prostateCancer_data <- prostateCancer_data[-1]

#tabulate diagnosis M/B
table(prostateCancer_data$diagnosis)

#recode as factor
prostateCancer_data$diagnosis <- factor(prostateCancer_data$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

#create Normalize fn
normalize_FN <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#TEST - Normalize_FN 
normalize_FN(c(1, 2, 3, 4, 5))
normalize_FN(c(10, 20, 30, 40, 50))

# normalize Prostate data
prostateCancer_data_N <- as.data.frame(lapply(prostateCancer_data[1:10], normalize_FN))

# confirm that normalization worked
summary(prostateCancer_data_N$area_mean)

# create training data 
prostateCancer_data_trainSet <- prostateCancer_data_N[1:65, ]

# test data
prostateCancer_data_testSet <- prostateCancer_data_N[66:100, ]

# create labels for training data 
prostate_train_labels <- prostateCancer_data[1:65, 1]

# create labels for test data
prostate_test_labels <- prostateCancer_data[66:100, 1]

# load "gmodels" library
library(gmodels)


# use the scale() function to z-score standardize a data frame
prostate_data_z <- as.data.frame(scale(prostateCancer_data[-1]))

# confirm that the transformation was applied correctly
summary(prostate_data_z$area_mean)

# create training data 
prostateCancer_data_trainSet <- prostateCancer_data_N[1:65, ]

# test data
prostateCancer_data_testSet <- prostateCancer_data_N[66:100, ]


# re-classify test cases
prostate_test_pred <- knn(train = prostateCancer_data_trainSet, test = prostateCancer_data_testSet,
                      cl = prostate_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = prostate_test_labels, y = prostate_test_pred,
           prop.chisq = FALSE)

##create confusion matrix
tab <- table(prostate_test_pred,prostate_test_labels)


##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# try several different values of k
prostateCancer_data_trainSet <- wbcd_n[1:65, ]
prostateCancer_data_testSet <- wbcd_n[66:100, ]


prostate_test_pred <- knn(train = prostateCancer_data_trainSet, test = prostateCancer_data_testSet, cl = prostate_train_labels, k=1)
CrossTable(x = prostate_test_labels, y = prostate_test_pred, prop.chisq=FALSE)


prostate_test_pred <- knn(train = prostateCancer_data_trainSet, test = prostateCancer_data_testSet, cl = prostate_train_labels, k=5)
CrossTable(x = prostate_test_labels, y = prostate_test_pred, prop.chisq=FALSE)

prostate_test_pred <- knn(train = prostateCancer_data_trainSet, test = prostateCancer_data_testSet, cl = prostate_train_labels, k=11)
CrossTable(x = prostate_test_labels, y = prostate_test_pred, prop.chisq=FALSE)

prostate_test_pred <- knn(train = prostateCancer_data_trainSet, test = prostateCancer_data_testSet, cl = prostate_train_labels, k=15)
CrossTable(x = prostate_test_labels, y = prostate_test_pred, prop.chisq=FALSE)

prostate_test_pred <- knn(train = prostateCancer_data_trainSet, test = prostateCancer_data_testSet, cl = prostate_train_labels, k=21)
CrossTable(x = prostate_test_labels, y = prostate_test_pred, prop.chisq=FALSE)

prostate_test_pred <- knn(train = prostateCancer_data_trainSet, test = prostateCancer_data_testSet, cl = prostate_train_labels, k=27)
CrossTable(x = prostate_test_labels, y = prostate_test_pred, prop.chisq=FALSE)