exams_df = read.csv(file = 'exams.csv')
head(exams_df)
categoricals_indexes = c(1,2,3,4,5)

exams_df[, categoricals_indexes] = data.frame(lapply(exams_df[, categoricals_indexes], factor))
str(exams_df)

library(tree)
set.seed(234)
test_accuracy = c()

par(mar = c(1, 1, 1, 1))
par(mfrow = c(5, 2))

for (i in c(1:10)) {
  
  sample = sample.int(n=nrow(exams_df),
                      size = floor(0.8*nrow(exams_df)),
                      replace = FALSE)
  
  train = exams_df[sample,]
  test = exams_df[-sample,]
  
  tree.prep = tree(prep ~ gender + ethnic + edu + lunch + m_score + r_score + w_score, data = train)
  
  cv.prep = cv.tree(tree.prep)
  plot(cv.prep$size, cv.prep$dev, type = 'b', xlab = 'Size', ylab = 'Dev', main = 'CV')
  min_cv = min(cv.prep$dev)
  dev_index = match(min_cv, cv.prep$dev)
  size = cv.prep$size[dev_index]
  prune.prep = prune.tree(tree.prep, best = size)
  
  tree.pred = predict(prune.prep, test, type = "class")
  confusion_matrix = as.data.frame(table(tree.pred,test$prep))
  total = sum(confusion_matrix$Freq)
  correct = sum(confusion_matrix[c(1,4),]$Freq)
  accuracy = correct/total
  
  test_accuracy = append(test_accuracy, accuracy)
  
  plot(prune.prep)
  text(prune.prep)
}

average_accuracy = mean(test_accuracy)
par(mfrow = c(1,1))
par(mar = c(4, 4, 4, 4))
barplot(test_accuracy, main = paste('Test Accuracy of Tree Models (Avg Accuracy = ', average_accuracy, ')', sep = " "), ylim = c(0:1))