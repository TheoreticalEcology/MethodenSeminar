library(maptools)
library(maps)
library(plyr)
library(dplyr)
library(mlr)
library(lme4)
# keras::install_keras()
# sapply(list.files("Functions/",full.names = T), source)

data = read.csv("Earthworm/1804_2_sWormModelData.csv")[,-1]
str(data)
names(data)

sites = 
  data %>% 
  filter(Study_Name!="birkhofer2013")



tmp = sites %>% select(logAbundance,logBiomass, SpeciesRichness, ESA, elevation, ph_new, 
                           CLYPPT, SLTPPT, CECSOL, ORCDRC,bio10_7,SnowMonths_cat,
                           Aridity, PETyr)

imputed = missRanger::missRanger(tmp[,-c(1:3)])
tmp = cbind(tmp[,1:3], imputed)

tmp2 = tmp[complete.cases(tmp[,1:3]),]


sub = mlr::createDummyFeatures(normalizeFeatures(obj = tmp2[,4:ncol(tmp2)]))

X = as.matrix(sub)

Y = tmp2[,1:3]


n = 3L
cvMSE = vector("list", n)
library(keras)
for(i in 1:n){
  subset = sample.int(nrow(X),size = as.integer(nrow(X)/n))
  
  trainX = X[-subset,]
  trainY = Y[-subset,]
  
  testX = X[subset,]
  testY = Y[subset,]
  
  dnn = keras_model_sequential()
  dnn %>% 
    layer_dense(units = 20L, activation = "relu") %>% 
    layer_dense( input_shape = ncol(trainX), units = 3L, activation = NULL)
  
  dnn %>% 
    compile(loss = keras::loss_mean_squared_error, optimizer = keras::optimizer_adamax(lr = 0.1))
  
  hist = 
    dnn %>% 
      fit(x = as.matrix(trainX), y = as.matrix(trainY), validation_split = 0.0, epochs = 20L)
  
  preds = 
    dnn %>% 
    predict(x = as.matrix(testX))
  loss =cbind(exp(preds[,1:2]) - 1, preds[,3]) - cbind(exp(testY[,1:2]) - 1, testY[,3])
  mse = apply(loss, 2, function(l) mean(l^2))
  cvMSE[[i]] = mse
  
}

results = abind::abind(cvMSE, along = 0)
apply(results, 2, mean)


### Calculate importance
library(DALEX)

dnn = keras_model_sequential()
dnn %>% 
  layer_dense(units = 20L, activation = "relu") %>% 
  layer_dense( input_shape = ncol(trainX), units = 1L, activation = NULL)

dnn %>% 
  compile(loss = keras::loss_mean_squared_error, optimizer = keras::optimizer_adamax(lr = 0.1))

hist = 
  dnn %>% 
  fit(x = as.matrix(X), y = as.matrix(Y[,1,drop = FALSE]), validation_split = 0.2, epochs = 5L)

# Abundance
explainer = DALEX::explain(dnn, data = X, y = Y[,1], predict_function = function(model, newdata) predict(model, as.matrix(newdata))[,1])

vi = DALEX::variable_importance(explainer)


# Biomass
explainer = DALEX::explain(dnn, data = X, y = Y[,2], predict_function = function(model, newdata) predict(model, as.matrix(newdata))[,2])

vi = DALEX::variable_importance(explainer)


# Richness
explainer = DALEX::explain(dnn, data = X, y = Y[,3], predict_function = function(model, newdata) predict(model, as.matrix(newdata))[,3])

vi = DALEX::variable_importance(explainer)


t1 = rbinom(10L, 1L, 0.3)
t2 = rbinom(10L, 1L, 0.5)
t3 = rbinom(10L, 1L, 0.4)
t4 = rep(0, 10L)

sum(dbinom(t4,prob = 0.3,size = 1L,log = TRUE))
sum(dbinom(t4,prob = 0.5,size = 1L,log = TRUE))
