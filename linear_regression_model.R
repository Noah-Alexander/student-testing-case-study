# Group 6 - Ali Zain Charolia, Sahran Prasla, Noah Alexander, Nathan Acosta, Joshua Duazo, Toan Nguyen 
# Linear Regression Models -  What groups of students should we market the test preparation course to? 

# MATH SCORES: 

# Load the data from local machine (Assuming data set already exists on the local machine) into a exams_data variable
exams_data <- read.csv("C:/Users/Ali Zain/Desktop/MATH4322/Final_Project/exams.csv")

# use summary() to fetch the summary of different variables in the data set
summary(exams_data)

# use as.factor() to convert predictor columns from numeric to factor 
exams_data$gender = as.factor(exams_data$gender)
exams_data$race.ethnicity = as.factor(exams$race.ethnicity)
exams_data$parental.level.of.education = as.factor(exams_data$parental.level.of.education)
exams_data$lunch = as.factor(exams_data$lunch)
exams_data$test.preparation.course = as.factor(exams_data$test.preparation.course)

# use summary() again to fetch the summary of different variables in the data set after factoring
summary(exams_data)

# import the necessary package from the library to implement Linear Regression
library(ISLR)

# create linear model and load it into a exams_data.lm variable 
exams_data.lm <- lm(exams_data$math.score ~ exams_data$gender + exams_data$race.ethnicity + exams_data$parental.level.of.education + exams_data$lunch + exams_data$test.preparation.course, data = exams_data)

# use summary() on exams_data.lm to fetch the summary of different variables in the data set after creating the linear model
summary(exams_data.lm)

# use step() to show we shouldn't get rid of anything
step(exams_data.lm)

# now, we will find the best model

# import the necessary leaps package from the library to implement regsubsets to perform best subset selection
library(leaps)

# use regsubsets() to perform best subset selection by identifying the best model that contains a given number of predictors, where the best is quantified using RSS.
exams_data.fit <- regsubsets(exams_data$math.score ~ exams_data$gender + exams_data$race.ethnicity + exams_data$parental.level.of.education + exams_data$lunch + exams_data$test.preparation.course, data = exams_data)

# use summary() on exams_data.fit and load it in a exams_data.res variable to fetch the summary of different variables in the data set after performing best subset selection on the data set
exams_data.res <- summary(exams_data.fit)
exams_data.res 

length(exams_data)

# use cbind() to merge data frames together 
exams_data.statistics <- cbind(exams_data.res$rsq, 
                               exams_data.res$adjr2, 
                               exams_data.res$cp, 
                               exams_data.res$bic)

# Load up the column names in the data
colnames(exams_data.statistics) <- c("rsq","Adjr2","Cp", "Bic")

# Display the statistics data
exams_data.statistics

# Leave one out cross validation
mse.loocv = matrix(nrow = nrow(exams_data), ncol = 1)

# for loop to create multiple models
for (i in 1:nrow(exams_data)) {
  sample = i
  
  # Framing data for easier reusability
  exams_data = data.frame(exams_data, exams_data$math.score)
  # use lm() to create multiple models
  exam_data.lm <- lm(math.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data = exams_data[-sample,])
  
  # get the MSE for each model
  mse.loocv[i,1] = (exams_data$math.score[sample] - predict(exam_data.lm, exams_data[sample,]))^2
}

# use head() to get the first [5] MSE values from mse.loocv
head(mse.loocv)

# use colMeans() to find means of all mse.loocv values
colMeans(mse.loocv)

# use par() to set parameters for multiple plots (2,2)
par(mfrow=c(2,2))

# use plot() to plot different graphs for the linear model [exams_data.lm]
plot(exams_data.lm)

# finally, use summary to find which predictors are significant in predicting the response variable, math.scores
summary(exams_data.lm)



# READING SCORES:
 
# use summary() again to fetch the summary of different variables in the data set after factoring
summary(exams_data)

# import the necessary package from the library to implement Linear Regression
library(ISLR)

# create linear model and load it into a exams_data.lm variable 
exams_data.lm <- lm(exams_data$reading.score ~ exams_data$gender + exams_data$race.ethnicity + exams_data$parental.level.of.education + exams_data$lunch + exams_data$test.preparation.course, data = exams_data)

# use summary() on exams_data.lm to fetch the summary of different variables in the data set after creating the linear model
summary(exams_data.lm)

# use step() to show we shouldn't get rid of anything
step(exams_data.lm)

# now, we will find the best model

# import the necessary leaps package from the library to implement regsubsets to perform best subset selection
library(leaps)

# use regsubsets() to perform best subset selection by identifying the best model that contains a given number of predictors, where the best is quantified using RSS.
exams_data.fit <- regsubsets(exams_data$reading.score ~ exams_data$gender + exams_data$race.ethnicity + exams_data$parental.level.of.education + exams_data$lunch + exams_data$test.preparation.course, data = exams_data)

# use summary() on exams_data.fit and load it in a exams_data.res variable to fetch the summary of different variables in the data set after performing best subset selection on the data set
exams_data.res <- summary(exams_data.fit)
exams_data.res 

length(exams_data)

# use cbind() to merge data frames together 
exams_data.statistics <- cbind(exams_data.res$rsq, 
                               exams_data.res$adjr2, 
                               exams_data.res$cp, 
                               exams_data.res$bic)

# Load up the column names in the data
colnames(exams_data.statistics) <- c("rsq","Adjr2","Cp", "Bic")

# Display the statistics data
exams_data.statistics

# Leave one out cross validation
mse.loocv = matrix(nrow = nrow(exams_data), ncol = 1)

# for loop to create multiple models
for (i in 1:nrow(exams_data)) {
  sample = i
  
  # Framing data for easier reusability
  exams_data = data.frame(exams_data, exams_data$reading.score)
  # use lm() to create multiple models
  exam_data.lm <- lm(reading.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data = exams_data[-sample,])
  
  # get the MSE for each model
  mse.loocv[i,1] = (exams_data$reading.score[sample] - predict(exam_data.lm, exams_data[sample,]))^2
}

# use head() to get the first [5] MSE values from mse.loocv
head(mse.loocv)

# use colMeans() to find means of all mse.loocv values
colMeans(mse.loocv)

# use par() to set parameters for multiple plots (2,2)
par(mfrow=c(2,2))

# use plot() to plot different graphs for the linear model [exams_data.lm]
plot(exams_data.lm)

# finally, use summary to find which predictors are significant in predicting the response variable, reading.scores
summary(exams_data.lm)


# WRIIING SCORES:


# use summary() again to fetch the summary of different variables in the data set after factoring
summary(exams_data)

# import the necessary package from the library to implement Linear Regression
library(ISLR)

# create linear model and load it into a exams_data.lm variable 
exams_data.lm <- lm(exams_data$writing.score ~ exams_data$gender + exams_data$race.ethnicity + exams_data$parental.level.of.education + exams_data$lunch + exams_data$test.preparation.course, data = exams_data)

# use summary() on exams_data.lm to fetch the summary of different variables in the data set after creating the linear model
summary(exams_data.lm)

# use step() to show we shouldn't get rid of anything
step(exams_data.lm)

# now, we will find the best model

# import the necessary leaps package from the library to implement regsubsets to perform best subset selection
library(leaps)

# use regsubsets() to perform best subset selection by identifying the best model that contains a given number of predictors, where the best is quantified using RSS.
exams_data.fit <- regsubsets(exams_data$writing.score ~ exams_data$gender + exams_data$race.ethnicity + exams_data$parental.level.of.education + exams_data$lunch + exams_data$test.preparation.course, data = exams_data)

# use summary() on exams_data.fit and load it in a exams_data.res variable to fetch the summary of different variables in the data set after performing best subset selection on the data set
exams_data.res <- summary(exams_data.fit)
exams_data.res 

length(exams_data)

# use cbind() to merge data frames together 
exams_data.statistics <- cbind(exams_data.res$rsq, 
                               exams_data.res$adjr2, 
                               exams_data.res$cp, 
                               exams_data.res$bic)

# Load up the column names in the data
colnames(exams_data.statistics) <- c("rsq","Adjr2","Cp", "Bic")

# Display the statistics data
exams_data.statistics

# Leave one out cross validation
mse.loocv = matrix(nrow = nrow(exams_data), ncol = 1)

# for loop to create multiple models
for (i in 1:nrow(exams_data)) {
  sample = i
  
  # Framing data for easier reusability
  exams_data = data.frame(exams_data, exams_data$writing.score)
  # use lm() to create multiple models
  exam_data.lm <- lm(writing.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data = exams_data[-sample,])
  
  # get the MSE for each model
  mse.loocv[i,1] = (exams_data$writing.score[sample] - predict(exam_data.lm, exams_data[sample,]))^2
}

# use head() to get the first [5] MSE values from mse.loocv
head(mse.loocv)

# use colMeans() to find means of all mse.loocv values
colMeans(mse.loocv)

# use par() to set parameters for multiple plots (2,2)
par(mfrow=c(2,2))

# use plot() to plot different graphs for the linear model [exams_data.lm]
plot(exams_data.lm)

# finally, use summary to find which predictors are significant in predicting the response variable, writing.scores
summary(exams_data.lm)