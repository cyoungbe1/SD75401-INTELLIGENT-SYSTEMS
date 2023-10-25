getwd()
#check working directory

#import csv file 
passenger_data <- read.csv("titanic.csv",TRUE,",")

#convert dataset to dataframe
class(passenger_data)

#print head
head(passenger_data)

#create subset survivors female
survivors_female <- subset(passenger_data, Sex == "female" & Survived == 1)

#create subset survivors male
survivors_male <- subset(passenger_data, Sex == "male" & Survived == 1)

#create subset survivors all
survivors_all <- subset(passenger_data, Survived == 1)

#create dataframe for all survivors
class(survivors_all)

#create dataframe for all female survivors
class(survivors_female)

#create dataframe for all male survivors
class(survivors_male)

#identify oldest survivor  - Barkworth, Mr. Algernon Henry Wilson
survivors_all$Name[which.max(survivors_all$Age)]

#identify youngest female survivor & Pclass - Cumings, Mrs. John Bradley (Florence Briggs Thayer) Pclass 1
survivors_female$Name[which.min(survivors_female$Age & survivors_female$Pclass)]

#identify youngest male survivor & Pclass - Beesley, Mr. Lawrence, Pclass 2
survivors_male$Name[which.min(survivors_male$Age & survivors_male$Pclass)]

#sort passengers descending alphabetically
random_passengers <- sample(passenger_data$Name, 10)
sort(random_passengers)

#convert pass name var to char var
passenger_data$Name <- as.character(passenger_data$Name)

#age observations missing
sum(is.na(passenger_data$Age))

#create "Status" var and designate "dead" or "alive"
passenger_data$Status <- factor(ifelse(passenger_data$Survived == 0, "dead", "alive"))

#count passengers per class
table(passenger_data$Pclass)

#find all passengers with last name "Moran"
passenger_moran <- passenger_data[grep("Moran", passenger_data$Name),]

#create subset dataframe  - Moran
passenger_moran <- subset(passenger_data, grepl("Moran", passenger_data$Name))

#check status - "survived"
passenger_moran$Status[which(passenger_moran$Survived == 1)] <- "survived"

#check status - "did not survive"
passenger_moran$Status[which(passenger_moran$Survived == 0)] <- "did not survive"

#did all survive?
all(passenger_moran$Survived == 1)
#FALSE

#subset passenger_data into dataframe with N/A for Age
pass_missingAge <- subset(passenger_data, is.na(Age))

#count observations according to Pclass
table(pass_missingAge$Pclass)

#get proportions / divide row total by number of observations by Pclass
table(pass_missingAge$Pclass)/nrow(pass_missingAge)



