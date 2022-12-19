exams <- read.csv("C:/Users/19034/Desktop/MATH4322/group_project/exams.csv")
summary(exams)
avg.score = (exams$math.score + exams$reading.score + exams$writing.score) / 3
exams = data.frame(exams, avg.score)

exams$gender = as.factor(exams$gender)
exams$race.ethnicity = as.factor(exams$race.ethnicity)
exams$parental.level.of.education = as.factor(exams$parental.level.of.education)
exams$lunch = as.factor(exams$lunch)
exams$test.preparation.course = as.factor(exams$test.preparation.course)
summary(exams)

library(ISLR)
exams.lm <- lm(avg.score ~ gender+ race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data=exams)
summary(exams.lm)
step(exams.lm) #shows that we shouldn't get rid of anything

#Is the step function sufficient in showing the model has only important
#varaiables contributing? Or do we need regsubsets too?

#finding the best model
library(leaps) #needed for regsubsets
exam.fit = regsubsets(avg.score ~ gender+ race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data=exams, nvmax = 15)
exam.res = summary(exam.fit)
exam.res
exam.stat = cbind(exam.res$rsq,
                 exam.res$adjr2,
                 exam.res$cp,
                 exam.res$bic)
colnames(exam.stat) = c("rsq","Adjr2","Cp","BIC")
exam.stat
#8 is the best, and it wants to get rid of specific dummy variables, but that would be irresponsible
#we can see that using all the predictors is the best model.


#Leave-One-Out Cross Validation
mse.loocv = matrix(nrow = nrow(exams), ncol = 1)
for (i in 1:nrow(exams)) {
  sample = i
  #Creating the models
  exam.lm2 = lm(avg.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data = exams[-sample,])
  #Getting the MSE for each model
  mse.loocv[i,1] = (exams$avg.score[sample] - predict(exam.lm2,exams[sample,]))^2
}
head(mse.loocv)
colMeans(mse.loocv)
#MSE = 151.1771

#should we do LOOCV or is a validation set enough?
#If we do LOOCV, what is our model? Do we average the models for all iterations?

par(mfrow=c(2,2))
plot(exams.lm)
#linear, independent, normal residual distribution, equal variance, no outliers

#this model is solid, scientifically B)


#What predictors are important for predicting high scores?
summary(exams.lm)

#Things that increase scores:
#  being from ethnic group D or E
#  parents having a bachelors or masters degree
#  not being on reduced lunch
#  taking the course (!)
  
#Things that decrease scores:
#  being male
#  being from ethnic group B or C
#  parents having only some college, high school, or some high school education

#Default values in the score:
#  female, ethnic group A, parents with associates degree, reduced lunch, took course