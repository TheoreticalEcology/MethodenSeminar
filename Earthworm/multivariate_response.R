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

imputed = missForest::missForest(xmis = tmp[,-c(1:3)])

sub = mlr::createDummyFeatures(normalizeFeatures(obj = tmp[,3:ncol(tmp)]))
X = as.matrix(sub)
Y = tmp[,1:3]




cvMSE = vector("list", 10)


for(i in 1:10){
  subset = sample.int(nrow(X),size = as.integer(nrow(X)/10))
  
  trainX = X[-subset,]
  trainY = Y[-subset,]
  
  testX = X[subset,]
  testY = Y[subset,]
  
  dnn = keras_model_sequential()
  dnn %>% 
    layer_dense(units = 3L, activation = NULL)
  
  dnn %>% 
    compile(loss = keras::loss_mean_squared_error, optimizer = keras::optimizer_adamax(lr = 0.1))
  
  hist = 
    dnn %>% 
    fit(x = as.matrix(trainX), y = as.matrix(trainY), validation_split = 0.0, epochs = 70L)
  preds = 
    dnn %>% 
    predict(x = as.matrix(testX))
  loss =cbind(exp(preds[,1:2]) - 1, preds[,3]) - cbind(exp(testY[,1:2]) - 1, testY[,3])
  mse = apply(loss, 2, function(l) mean(l^2))
  cvMSE[[i]] = mse
  
}