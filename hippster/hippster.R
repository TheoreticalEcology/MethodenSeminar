load(file = "C:/Users/LocalAdmin/Dropbox/Hipster_app/DATA.RData")

library(tidyverse)
library(keras)

image = data[91,,,]
train_X[1,,,,drop=FALSE] %>% 
  image_to_array() %>%
  `/`(., 255) %>%
  as.raster() %>%
  plot()

raw = unlist(lapply(stringr::str_split(labels, "/"), function(i) i[length(i)]))

Y = matrix(as.numeric(substr(raw, 0, 1)), ncol = 1L)

set.seed(42)
indices = sample.int(130, 30)


train_X = data[-indices,,,]
train_Y = Y[-indices,,drop =FALSE]

test_X = data[indices,,,]
test_Y = Y[indices,,drop=FALSE]


model = keras_model_sequential()
model %>% 
  layer_conv_2d(input_shape = c(225, 225,3),filters = 16, kernel_size = c(5,5), activation = NULL, use_bias = F) %>% 
  layer_batch_normalization() %>% 
  layer_activation_relu() %>% 
  layer_max_pooling_2d() %>% 
  layer_conv_2d(filters = 16, kernel_size = c(2,2), activation = NULL, use_bias = F) %>% 
  layer_batch_normalization() %>% 
  layer_activation_relu() %>% 
  layer_max_pooling_2d() %>% 
  layer_flatten() %>% 
  layer_dense(units = 10L) %>% 
  layer_batch_normalization() %>% 
  layer_activation_relu() %>% 
  layer_dense(1, activation = "sigmoid")
summary(model)

model %>% 
  compile(
    optimizer = keras::optimizer_adam(lr = 0.001),
    loss = keras::loss_binary_crossentropy,
    metrics = "accuracy"
  )

model %>% 
  fit(x = train_X, y = train_Y, 
      shuffle = TRUE, batch_size = 25L, 
      validation_data = list(test_X, test_Y),
      epochs = 5L)




#### Transfer learning
densenet = application_densenet121(include_top = FALSE, input_shape  = c(225L, 225L, 3L))


model = keras::keras_model(inputs = densenet$input, outputs = 
                             layer_dense(layer_flatten(densenet$output), units = 1L, activation = "sigmoid")
)
keras::freeze_weights(model, from = 1, to = length(model$layers)-1L)

train_X[1,,,] %>% 
  image_to_array() %>%
  `/`(., 255) %>%
  as.raster() %>%
  plot()

model %>%  predict(train_X[1,,,,drop=FALSE])

model %>% 
  compile(loss = loss_binary_crossentropy, optimizer = optimizer_adam(0.001))

model %>% 
  fit(
    x = train_X, 
    y = train_Y,
    epochs = 5L,
    batch_size = 20L,
    shuffle = T,
    validation_data = list(test_X, test_Y),
  )

pred = 
  model %>% 
    predict(test_X)

mean(ifelse(pred > 0.5, 1, 0 ) == test_Y[,1])
