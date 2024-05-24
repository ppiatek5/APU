> setwd("C:/Users/mrbre/Desktop/Studia/APU/Lab4/")

> smartfony <- read.csv('smartfony_data.csv')
> smartfony_reduced <- smartfony[,
+                                c('pamiec_RAM', 'pamiec_wbudowana', 'aparat_foto', 'cena', 'ocena')]

> install.packages("mlr")
> library(mlr)
> smartfony_task <- makeClassifTask(data = smartfony_reduced, target = "ocena")
> install.packages("rFerns")
> install.packages("randomForest")
> lrns <- makeLearners(c("lda","rpart", "C50","rFerns",
+                        "randomForest"), type = "classif")
> comp <- benchmark(learners = lrns,
+                         tasks = smartfony_task,
+                         resampling = cv5)
> learner <- c("lda", "rpart", "C50", "rFerns", "randomForest")
> accuracy <- c(0.267, 0.2, 0.2, 0.867, 0.267)
> data <- data.frame(learner, accuracy)
> barplot(data$accuracy, names.arg = data$learner, ylim = c(0, 1), ylab = "Accuracy")
> smartfony_task <- makeClassifTask(data = smartfony_reduced, target = "ocena")
> smartfony_train <- smartfony_reduced[1:12, ]
> smartfony_test <- smartfony_reduced[13:15, ]
> smartfony_learner <- makeLearner("classif.rFerns")
> smartfony_model <- train(smartfony_learner, smartfony_task)
> smartfony_predictions <- predict(smartfony_model, newdata = smartfony_test)
> performance <- performance(smartfony_predictions, measures = list(acc))
> print(performance)