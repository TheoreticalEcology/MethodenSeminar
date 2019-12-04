load(file = "C:/Users/LocalAdmin/Dropbox/Hipster_app/DATA.RData")

library(tidyverse)
library(keras)

image = data[91,,,]
image %>% 
  image_to_array() %>%
  `/`(., 255) %>%
  as.raster() %>%
  plot()

raw = unlist(lapply(stringr::str_split(labels, "/"), function(i) i[length(i)]))

Y = matrix(as.numeric(substr(raw, 0, 1)), ncol = 1L)


model = keras_model_sequential()
model %>% 
  layer_conv_2d(input_shape = c(225, 225,3),filters = 16, kernel_size = c(5,5), activation = NULL, use_bias = F) %>% 
  layer_batch_normalization() %>% 
  layer_activation_relu() %>% 
  layer_dropout(0.2) %>% 
  layer_max_pooling_2d() %>% 
  layer_conv_2d(filters = 16, kernel_size = c(2,2), activation = NULL, use_bias = F) %>% 
  layer_batch_normalization() %>% 
  layer_activation_relu() %>% 
  layer_dropout(0.2) %>% 
  layer_max_pooling_2d() %>% 
  layer_flatten() %>% 
  layer_dense(1, activation = "sigmoid")
summary(model)

model %>% 
  compile(
    optimizer = keras::optimizer_adam(),
    loss = keras::loss_binary_crossentropy,
    metrics = "accuracy"
  )

model %>% 
  fit(x = data, y = Y, shuffle = TRUE, batch_size = 10L, validation_split = 0.2)
