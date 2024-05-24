> setwd("C:/Users/mrbre/Desktop/Studia/APU/Lab4/")

> install.packages("mlr")
> install.packages("rpart.plot")
> install.packages("C50")

> library(mlr)
> library(rpart.plot)
> library(C50)
> library(datasets)

> data(iris)
> iris_task <- makeClassifTask(data = iris, target = "Species")

> indices <- sample(1:nrow(iris), size = nrow(iris), replace = FALSE)
> train_ratio <- 0.7
> train_size <- floor(train_ratio * nrow(iris))
> train_data <- iris[indices[1:train_size], ]
> test_data <- iris[indices[(train_size + 1):nrow(iris)], ]

> levels(factor(listLearners()$type))
> learner <- makeLearner("classif.rpart")
> model <- train(learner, iris_task)
> predictions <- predict(model, newdata = test_data)
> performance <- performance(predictions, measures = list(acc))
> print(performance)
> summary(model)

> ruleModel <- C5.0(Species ~ ., data=train_data, rules=TRUE)
> ruleModel
> summary(ruleModel)