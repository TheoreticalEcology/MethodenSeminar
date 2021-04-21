###### Hyper Parameter Optimization

# install.packages("mlr")
# install.packages("xgboost")

library(mlr)

x = matrix(runif(10000,-2,2), nrow = 1000,10)

w = runif(10, 0, 3)


y = x %*% w + 3 + rnorm(1000, 0, cos(x[,1])^2) + (1:1000 * sin(x[,2]))*0.1 

x = apply(x, 2, FUN = function(k) k + rnorm(1000,0,0.3))

plot(y ~ x[,2])



library(mlr)

learner = makeLearner("regr.xgboost") 
getParamSet(learner)

task = makeRegrTask(data = data.frame(x = x, y = y), target = "y")

m1 = train(learner, task)
pred = predict(m1, task)

rmse_m1 = mlr::performance(pred, rmse)

## Tuning:

parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeDiscreteParam("booster", values = c("gbtree", "dart")),
                                        ParamHelpers::makeDiscreteParam("sample_type", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
                                        ParamHelpers::makeDiscreteParam("normalize_type", values = c("tree", "forest"), requires = quote(booster == "dart")),
                                        ParamHelpers::makeNumericParam("eta", lower = 0.01, upper = 0.5),
                                        ParamHelpers::makeNumericParam("gamma", lower = -9, upper = 5, trafo = function(x) 2^x),
                                        ParamHelpers::makeIntegerParam("max_depth", lower = 1, upper = 10),
                                        ParamHelpers::makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x),
                                        ParamHelpers::makeNumericParam("alpha", lower = -10, upper = 5, trafo = function(x) 2^x),
                                        ParamHelpers::makeNumericParam("min_child_weight", 0, 10),
                                        ParamHelpers::makeIntegerParam("nrounds", lower = 1, upper = 100),
                                        ParamHelpers::makeNumericParam("rate_drop", lower = 0, upper = 0.2, requires = quote(booster == "dart")),
                                        ParamHelpers::makeNumericParam("skip_drop", lower = 0, upper = 0.3, requires = quote(booster == "dart")))

tuneControl = makeTuneControlRandom(maxit = 5L)

tuneResult = tuneParams(learner, task, resampling = mlr::makeResampleDesc("Holdout"),
                        measures = rmse,par.set = parameter,control = tuneControl)

# parEff = generateHyperParsEffectData(tuneResult, partial.dep = TRUE)
# plotHyperParsEffect(parEff, x = "nrounds", y = "eta")

learner2 = setHyperPars(learner, par.vals = tuneResult$x)

m2 = train(learner2, task)
pred = predict(m2, task)
rmse_m2 = mlr::performance(pred, rmse)

#### 


learnerTune = makeTuneWrapper(learner, resampling = cv3, measures = rmse, 
                              par.set = parameter,control = tuneControl)

parallelMap::parallelStartSocket(3)

task = normalizeFeatures(task)

result = resample(learnerTune,task, 
                  measures = rmse, models = TRUE, 
                  keep.pred = TRUE, resampling = cv10,extract = mlr::getTuneResult)

result$measures.test

library(tidyverse)
pred_1 =
  result$pred$data %>% 
  filter(set == "test") %>% 
  filter(iter == 5)

plot(pred_1$response, pred_1$truth, xlim = c(-70, 100), ylim = c(-70, 100))








