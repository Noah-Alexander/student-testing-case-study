exams <- read.csv("C:/Users/19034/Desktop/MATH4322/group_project/exams.csv")
exams$gender = as.factor(exams$gender)
exams$race.ethnicity = as.factor(exams$race.ethnicity)
exams$parental.level.of.education = as.factor(exams$parental.level.of.education)
exams$lunch = as.factor(exams$lunch)
exams$test.preparation.course = as.factor(exams$test.preparation.course)
summary(exams)

-------
set.seed(234)
sample = sample.int(n = nrow(exams),
                    size = floor(.75*nrow(exams)),
                    replace = FALSE)
train = exams[sample,]
test = exams[-sample,]
exams.pred = predict.glm(exams.glm,newdata = test,type = "response")
yhat = ifelse(exams.pred < 0.5, 'completed', 'none')
(conf.test = table(test$test.preparation.course,yhat))

45/200
------

#k-folds cross validation
library("boot")
exams.glm = glm(test.preparation.course~ .,
                 data = exams,
                 family = "binomial")
cv.error = cv.glm(exams, exams.glm, K=10)$delta[1]
#test error rate = 16.53%


summary(exams.glm)
step(exams.glm)
#shows that we should take out the standard or free/reduced lunch variable

exams.glm = glm(test.preparation.course~ .-lunch,
                data = exams,
                family = "binomial")
cv.error = cv.glm(exams, exams.glm, K=10)$delta[1]
#test error rate 16.26%

#finding the best model
library(leaps) #needed for regsubsets
exam.fit = regsubsets(test.preparation.course ~ ., data=exams, nvmax = 15)
exam.res = summary(exam.fit)
exam.res
exam.stat = cbind(exam.res$rsq,
                  exam.res$adjr2,
                  exam.res$cp,
                  exam.res$bic)
colnames(exam.stat) = c("rsq","Adjr2","Cp","BIC")
exam.stat
#shows test scores and gender are the most accurate predictors to who took the course

exams.glm = glm(test.preparation.course ~ gender+math.score+reading.score+writing.score, exams, family = "binomial")
cv.error = cv.glm(exams, exams.glm, K=10)$delta[1]
#test error rate is 17.10% with only these predictors

summary(exams.glm)

#men and people with good writing scores(!!) are less likely to take the course
#the writing scores interpretation is really interesting, definitely not something we
#expected to find