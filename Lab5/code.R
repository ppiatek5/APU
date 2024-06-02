setwd('F:/Sem8/APU/Lab5')

install.packages("tensorflow")
library(tensorflow)
install_tensorflow()

install.packages("keras")
library(keras)
install_keras()

cifar <- dataset_cifar10()

X_train <- cifar$train$x
X_test <- cifar$test$x
y_train <- cifar$train$y
y_test <- cifar$test$y

X_train <- X_train / 255
X_test <- X_test / 255

y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)

model <- keras_model_sequential()

model <- model %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(32,32,3)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten(input_shape = c(32,32,3)) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = "accuracy"
)

summary(model)

history <- model %>%
  fit(X_train, y_train, epochs = 10, batch_size = 32,
      validation_data = list(X_test,y_test), shuffle=TRUE)

model %>%
  evaluate(X_test, y_test)

save_model_tf(model, "saved_model/model")