install.packages("tidyverse")
install.packages("dplyr")
install.packages("psych")
install.packages ("corrplot")
install.packages("janitor")
install.packages("umx")
install.packages("devtools")


getwd()
library(readxl)
library(dplyr)
library(umx)
library (psych)
library (corrplot)
library (janitor)
         
#Upload the dataset
train <- read_excel("C:/Users/pansy.dwe/Desktop/AIRLINE/train.xlsx")
attach(train)
names(train)

#data transformation from 
train_transform1 <- train %>%mutate_at(c("Gender", "Customer Type", "Type of Travel", "satisfaction", "Class"), funs(recode(.,"Business" = 1, "Eco"= 2, "Eco Plus" = 3,"neutral or dissatisfied"= 1, "satisfied" = 2, "Female" = 1, "Male"= 2, "Loyal Customer" =1, "disloyal Customer" = 2, "Personal Travel" = 1, "Business travel" = 2)))

#Remove null value
sum(is.na(train_transform1))
train_without_null = na.omit(train_transform1)
train_without_null


# Outliers detection
boxplot(train_without_null$Age, ylab = "Age", col = I("turquoise"),  main = "Population distribution")
summary(Age)

boxplot(train_without_null$`Flight Distance`, ylab = "Flight Distance (km)", col = I("orange"),  main = "Flight distance distribution")
boxplot(train_without_null$`Arrival Delay in Minutes`, ylab = "Arrival Delay in minutes", col = I("orange"),  main = "Arrival delay in minutes")
boxplot(train_without_null$`Departure Delay in Minutes`, ylab = "Departure Delay in minutes", col = I("orange"),  main = "Departure delay in minutes")
#Note: Outliers detected in Flight distance, Arrival delay in minutes, Departure delay in minutes but no outliers detected in Age. 

#correlation coefficient matrix

library(corrplot)
corrplot(cor(train_without_null), method = "circle")

mcor<-round(cor(train_without_null),2)
mcor

#Pearson's correlation

cor.test(train_without_null$satisfaction, train_without_null$`Gate location`)
cor.test(train_without_null$satisfaction, train_without_null$`Online boarding`)
cor.test(train_without_null$satisfaction, train_without_null$`Inflight wifi service`)
cor.test(train_without_null$satisfaction, train_without_null$`Ease of Online booking`)
cor.test(train_without_null$satisfaction, train_without_null$`Baggage handling`)
cor.test(train_without_null$satisfaction, train_without_null$`Inflight service`)
cor.test(train_without_null$satisfaction, train_without_null$`Departure/Arrival time convenient`)

cor.test(train_without_null$satisfaction, train_without_null$Age)

cor.test(train_without_null$`Arrival Delay in Minutes`, train_without_null$`Departure/Arrival time convenient`)
cor.test(train_without_null$`Departure Delay in Minutes`, train_without_null$`Departure/Arrival time convenient`)

#Reliability test (cronbarch Alpha value)
install.packages("umx")
train_matrix <- data.matrix(train_without_null)

library(umx)
reliability(cov(train_without_null))

# > reliability(cov(train_without_null))
# Results: Alpha reliability =  0.0064 and Standardized alpha =  0.6615.
# Cronbach's appha value should be at least 0.07 - 0.08 to get a good quality/ reliable dataset. Therefore, we will remove columsn which are not relavant for the analysis (serial number, id) and data detected for outliers and very low/ no correlation with satisfaction (Arrival delay in minutes and departure delay in minutes) and group some variable which are important for our anlaysis (Age and flight distsance)

#Groupping

train_without_null$Agegroup <- cut (train_without_null$Age,
                                breaks = c (-Inf
                                            ,20,30,40,50,60,70
                                            , Inf),
                                Labels = c ("under 20"
                                            , "20 to 29", "30 to 39", "40 to 49", 
                                            "50 to 59", "60 to 69", "70 and above"),
                                right = FALSE)

train_without_null$flight_distance_group <- cut (train_without_null$`Flight Distance`,
                                                 breaks = c (-Inf
                                                             ,499,2000,
                                                             Inf),
                                                 
                                                 Labels = c ("less than 500 km"
                                                             ,"between 500 & 2000", "above 2000"),
                                                 right = FALSE)




#Remove columns
train_without_null_remove_columns <- train_without_null[, ! (names (train_without_null)%in% c( "...1", "id","Age", "Flight Distance", "Arrival Delay in Minutes", "Departure Delay in Minutes"))]
head(train_without_null_remove_columns)

#Reliability test after groupping and removing un-necessary data(cronbarch Alpha value)
install.packages("umx")
train_matrix2 <- data.matrix(train_without_null_remove_columns)

library(umx)
reliability(cov(train_matrix2))

#correlation matrix with final dataset for 14 categories
train_cat_corr<- train_without_null_remove_columns[, ! (names (train_without_null_remove_columns)%in% c("Gender", "Customer Type","Type of Travel", "Class","Agegroup", "flight_distance_group"))]
head(train_cat_corr)

library(corrplot)
corrplot(cor(train_cat_corr), method = "number")

mcor<-round(cor(train_cat_corr),2)
mcor

write.table(train_without_null, file = "ExportfromR.csv", sep = ",")
write.table(mcor, file = "correlation.csv", sep = ",")


cor.test(train_without_null$satisfaction, train_without_null$Age)

