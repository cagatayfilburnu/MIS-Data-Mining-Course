# DATA MINING AND BUSINESS INTELLIGENCE PROJECT
# Kamil Çaðatay Filburnu

### MODEL I
# Model I that is we did it in classroom section.

# Firstly, we should import our libraries that we need.

library(modeldata)
library(e1071)
library(caret)

# Data Analysis
# Then, we are looking the data.

?attrition

names(attrition)

table(attrition$Attrition)
View(attrition)



str(attrition)
summary(attrition)

# Test, Train Part

Index1 <- createDataPartition(attrition$Attrition, p = 0.8, list = FALSE)
train1 <- attrition[Index1,]
summary(train1)

test1 <- attrition[-Index1,]
summary(test1)


# Naive Bayes Model1

set.seed(90210211)
model1 <- naiveBayes(Attrition~., data = train1)
model1

predict1 <- predict(model1, newdata = test1)
ctable1 <- table(test1$Attrition, predict1)
ctable1

confusionMatrix(ctable1)

# As a result of Model I that we did it in lesson, we have 0.7747 accuracy. Also, Sensitivity is 0.9286

# -----------------------------------------------------------------------------------------------------------------------------------
  
### MODEL II
# Task 1: Eliminate all numeric variables then build model and test it. get confusion matrix results

# First, we need to import our libraries

library(caret)
library(modeldata)
library(e1071)


# Data Analysis
# I created a new variable that is "eliminatedData" because I do not want to break original attrition data in global environment. 

eliminatedData <- attrition

names(eliminatedData)
summary(eliminatedData)
str(eliminatedData)


# Then, according to our task 1, we need to eliminate our numeric variables

eliminatedData$Age <- NULL
eliminatedData$DailyRate <- NULL
eliminatedData$DistanceFromHome <- NULL
eliminatedData$HourlyRate <- NULL
eliminatedData$JobLevel <- NULL
eliminatedData$MonthlyIncome <- NULL
eliminatedData$MonthlyRate <- NULL
eliminatedData$NumCompaniesWorked <- NULL
eliminatedData$PercentSalaryHike <- NULL
eliminatedData$StockOptionLevel <- NULL
eliminatedData$TotalWorkingYears <- NULL
eliminatedData$TrainingTimesLastYear <- NULL
eliminatedData$YearsAtCompany <- NULL
eliminatedData$YearsInCurrentRole <- NULL
eliminatedData$YearsSinceLastPromotion <- NULL
eliminatedData$YearsWithCurrManager <- NULL

str(eliminatedData)

# After that, I created test and train variables before the model

summary(eliminatedData)

Index2 <- createDataPartition(eliminatedData$Attrition, p = 0.8, list = FALSE)
train2 <- eliminatedData[Index2,]
summary(train2)

test2 <- eliminatedData[-Index2,]
summary(test2)

# Now, I created the model for eliminatedData

set.seed(3213)
model2 <- naiveBayes(Attrition~., data = train2)
model2

predict2 <- predict(model2, newdata = test2)

ctable2 <- table(test2$Attrition, predict2)
ctable2

confusionMatrix(ctable2)


# Consequently, according to our Confusion Matrix for Task 1, we have 0.8567 accuracy. 
# So, Model 2 is better than model that we did in lesson (0.7747 accuracy)

-----------------------------------------------------------------------------------------------------------------------------------
  
### MODEL III
# Task 2: Convert numeric variables to categorical ones. build model and test it. get confusion matrix results.

# First, we need to import our libraries

library(caret)
library(modeldata)
library(e1071)


# Data Analysis
# I created a new variable that is "convertedData" because I do not want to break original attrition data in global environment.

convertedData <- attrition

names(convertedData)
summary(convertedData)
str(convertedData)


# Then, according to our task 2, we need to convert our numeric variables to categorical ones. We determine separate levels for each feature and create factors.

convertedData$Age <- cut(convertedData$Age, breaks = c(0, 35, 50, Inf), labels = c("Younger", "Middle-Age", "Elderly"))

convertedData$DailyRate <- cut(convertedData$DailyRate, breaks = c(0, 750, Inf), labels = c("Low", "High"))

convertedData$DistanceFromHome <- cut(convertedData$DistanceFromHome, breaks = c(0, 9, Inf), labels = c("Close", "Far"))

convertedData$HourlyRate <- cut(convertedData$HourlyRate, breaks = c(0, 65, Inf), labels = c("Low", "High"))

convertedData$JobLevel <- cut(convertedData$JobLevel, breaks = c(0, 2, 4, Inf), labels = c("First Level", "Second Level", "Third Level"))

convertedData$MonthlyIncome <- cut(convertedData$MonthlyIncome, breaks = c(0, 3000, 6500, 8300, Inf), labels = c("Low", "Medium", "High", "Very High"))

convertedData$MonthlyRate <- cut(convertedData$MonthlyRate, breaks = c(0, 14500, Inf), labels = c("Low", "High"))

convertedData$NumCompaniesWorked <- cut(convertedData$NumCompaniesWorked, breaks = c(0, 3, Inf), labels = c("Regular Time", "Overtime"))

convertedData$PercentSalaryHike <- cut(convertedData$PercentSalaryHike, breaks = c(0, 15, Inf), labels = c("Low Raise", "High Raise"))

convertedData$StockOptionLevel <- cut(convertedData$StockOptionLevel, breaks = c(0, 0.8, Inf), labels = c("Level 1", "Level 2"))

convertedData$TotalWorkingYears <- cut(convertedData$TotalWorkingYears, breaks = c(0, 6, 11, 22, Inf), labels = c("Apprentice", "Regular", "Expert", "Senior"))

convertedData$TrainingTimesLastYear <- cut(convertedData$TrainingTimesLastYear, breaks = c(0, 2, Inf), labels = c("Low", "High"))

convertedData$YearsAtCompany <- cut(convertedData$YearsAtCompany, breaks = c(0, 7, 12, Inf), labels = c("Junior", "Middle", "Senior"))

convertedData$YearsInCurrentRole <- cut(convertedData$YearsInCurrentRole, breaks = c(0, 4, 10, Inf), labels = c("Jr. Role", "Middle Role", "Senior Role"))

convertedData$YearsSinceLastPromotion <- cut(convertedData$YearsSinceLastPromotion, breaks = c(0, 2, 7, Inf), labels = c("New Promotion", "Late Promotion", "Manager Level"))

convertedData$YearsWithCurrManager <- cut(convertedData$YearsWithCurrManager, breaks = c(0, 5, Inf), labels = c("Low", "High"))

summary(convertedData)


# However, we determine NA values in some factors. We need to fix it so, I replaced them with middle values in factors that have 3 or more levels. If there are 2 levels, I selected level that is higher than numeric mean.

convertedData$NumCompaniesWorked <- factor(convertedData$NumCompaniesWorked, 
                                           exclude = NULL, 
                                           levels = c(levels(convertedData$NumCompaniesWorked), NA), 
                                           labels = c(levels(convertedData$NumCompaniesWorked), "Overtime"))

convertedData$TotalWorkingYears <- factor(convertedData$TotalWorkingYears, 
                                          exclude = NULL, 
                                          levels = c(levels(convertedData$TotalWorkingYears), NA), 
                                          labels = c(levels(convertedData$TotalWorkingYears), "Regular"))

convertedData$StockOptionLevel <- factor(convertedData$StockOptionLevel, 
                                         exclude = NULL, 
                                         levels = c(levels(convertedData$StockOptionLevel), NA), 
                                         labels = c(levels(convertedData$StockOptionLevel), "Level 1"))

convertedData$TrainingTimesLastYear <- factor(convertedData$TrainingTimesLastYear, 
                                              exclude = NULL, 
                                              levels = c(levels(convertedData$TrainingTimesLastYear), NA), 
                                              labels = c(levels(convertedData$TrainingTimesLastYear), "High"))

convertedData$YearsAtCompany <- factor(convertedData$YearsAtCompany, 
                                       exclude = NULL, 
                                       levels = c(levels(convertedData$YearsAtCompany), NA), 
                                       labels = c(levels(convertedData$YearsAtCompany), "Middle"))

convertedData$YearsInCurrentRole <- factor(convertedData$YearsInCurrentRole, 
                                           exclude = NULL, 
                                           levels = c(levels(convertedData$YearsInCurrentRole), NA), 
                                           labels = c(levels(convertedData$YearsInCurrentRole), "Middle Role"))

convertedData$YearsSinceLastPromotion <- factor(convertedData$YearsSinceLastPromotion, 
                                                exclude = NULL, 
                                                levels = c(levels(convertedData$YearsSinceLastPromotion), NA), 
                                                labels = c(levels(convertedData$YearsSinceLastPromotion), "Late Promotion"))

convertedData$YearsWithCurrManager <- factor(convertedData$YearsWithCurrManager, 
                                             exclude = NULL, 
                                             levels = c(levels(convertedData$YearsWithCurrManager), NA), 
                                             labels = c(levels(convertedData$YearsWithCurrManager), "High"))

summary(convertedData)
str(convertedData)


# Lastly for Task 2, I created test and train variables before the model and created the Naive Bayes Model3.

Index3 <- createDataPartition(convertedData$Attrition, p = 0.8, list = FALSE)
train3 <- convertedData[Index3,]
summary(train3)

test3 <- convertedData[-Index3,]
summary(test3)




set.seed(321231343)
model3 <- naiveBayes(Attrition~., data = train3)
model3

predict3 <- predict(model3, newdata = test3)

ctable3 <- table(test3$Attrition, predict3)
ctable3

confusionMatrix(ctable3)


# -----------------------------------------------------------------------------------------------------------------------------------
  
### TASK 3 and CONCLUSION
  
# Model 1         : 0.7747 Accuracy,  0.9286 Sensitivity
# Model 2 & TASK 1: 0.8567 Accuracy,  0.8723 Sensitivity
# Model 3 & TASK 2: 0.8532 Accuracy,  0.9143 Sensitivity

# According to our model solutions, Model 2 and Model 3 have close accuracy. Also, Model 3 has better sensitivity score with 0.9143 than Model 2. As a result, in my opinion, Model 3 (TASK 2) is the best way for our project.
