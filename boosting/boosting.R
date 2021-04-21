## Boosting:
library(rpart)

x = runif(1000,-5,5)

y = x*sin(x)*2 + rnorm(1000,0, cos(x)+1.8)

data = data.frame(x = x, y = y)

plot(y~x)


## Gradient Boosting:
control = rpart.control(minsplit = 25L, maxdepth = 10, cp = 10e-3)


get_model = function(x,y, cp){
  control = rpart.control(minsplit = 25L, maxdepth = 10, cp = cp)
  model = rpart(y~x, data.frame(x = x,y = y), control = control)
  pred = predict(model, data.frame(x = x,y = y))
  return(list(model = model, pred = pred))
}

depth = 1L
pred = NULL
model_list = list()

get_boosting_model = function(depth, cp = 10e-3){
  m_list = list()
  
  for(i in 1:depth){
    if(i == 1) {
      m = get_model(x,y, cp)
      pred = m$pred
    } else {
      y_res = y-pred
      m = get_model(x, y_res, cp)
      pred = pred + m$pred
    }
    m_list[[i]] = m$model
  }
  model_list <<- m_list
  
  return(pred)
}

pred = get_boosting_model(10L, cp = 1e-5)[order(data$x)]
length(model_list)
plot(model_list[[1]])


plot(y~x)
lines(x = data$x[order(data$x)],get_boosting_model(1L, cp = 1e-2)[order(data$x)], col = 'red', lwd = 4)

lines(x = data$x[order(data$x)],get_boosting_model(10L, cp = 1e-2)[order(data$x)], col = 'blue', lwd = 4)

lines(x = data$x[order(data$x)],get_boosting_model(100L, cp = 1e-2)[order(data$x)], col = 'green', lwd = 4)

rf = ranger::ranger(y~x, data = data,min.node.size = 200)
lines(x = data$x[order(data$x)],predict(rf, data)$predictions[order(data$x)], col = 'orange', lwd = 4)
rf = ranger::ranger(y~x, data = data,min.node.size = 1)
lines(x = data$x[order(data$x)],predict(rf, data)$predictions[order(data$x)], col = 'blue', lwd = 4)

library(mlr)

getParamSet(makeLearner("regr.xgboost"))



