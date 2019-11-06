library(maptools)
library(maps)
library(plyr)
library(dplyr)
library(mlr)
library(lme4)
#sapply(list.files("Functions/",full.names = T), source)

data = read.csv("/Users/alex/Documents/Uni neu WS17/Theoretische Ökologie/Seminar/CodingClub/Phillips, et al. 2019, Earthworms/1804_2594_Dataset/1804_2_sWormModelData.csv")[,-1]
str(data)
names(data)

sites = 
  data %>% 
  filter(Study_Name!="birkhofer2013")

sites$scalePH <-scale(sites$ph_new)

########## TO DO ########## 
# - Fitting unlogged models 
# - multivariate response models:
#   - keras
#   - gllvm
#   - multivariate BRT
# - checking variable importances (and compare with paper results)


########## Richness ##########


richness <- sites[complete.cases(sites$SpeciesRichness),] #6089
richness <- droplevels(richness[richness$ESA != "Unknown",]) # 
richness <- droplevels(richness[-which(richness$SpeciesRichness != round(richness$SpeciesRichness)),]) # 5642

# richness <- richness[complete.cases(richness$scalePH),]

# richness <- droplevels(richness[!(is.na(richness$PHIHOX)),])
richness <- droplevels(richness[!(is.na(richness$bio10_15)),]) ## 
richness <- droplevels(richness[!(is.na(richness$OCFinal)),]) ## 
richness <- droplevels(richness[!(is.na(richness$phFinal)),]) ## 
richness <- droplevels(richness[!(is.na(richness$scaleAridity)),]) ## 
richness <- droplevels(richness[!(is.na(richness$SnowMonths_cat)),]) ## 5799
table(richness$ESA)
richness_notinclude <- c("Needleleaf deciduous forest", "Tree open",
                         "Sparse vegetation",  "Cropland/Other vegetation mosaic", "Urban",
                         "Bare area (consolidated", "Paddy field", "Wetland/Herbaceous", "Water bodies")


richness <- droplevels(richness[!(richness$ESA %in% richness_notinclude),]) ##    5737
summary(richness$phFinal)
richness$scalePH <- as.vector(scale(richness$phFinal))
richness$scaleCLYPPT <- scale(richness$ClayFinal)
richness$scaleSLTPPT <- scale(richness$SiltFinal)
richness$scaleCECSOL <- scale(richness$CECSOL)
richness$scaleORCDRC <- scale(richness$OCFinal)

richness$bio10_1_scaled <- scale(richness$bio10_1)
richness$bio10_4_scaled <- scale(richness$bio10_4)
richness$bio10_7_scaled <- scale(richness$bio10_7)
richness$bio10_12_scaled <- scale(richness$bio10_12)
richness$bio10_15_scaled <- scale(richness$bio10_15)

richness$scaleAridity <- scale(richness$Aridity)
richness$ScalePET <- scale(richness$PETyr)
richness$ScalePETSD <- scale(richness$PET_SD)
richness$scaleElevation <- scale(richness$elevation)
#ind <- df_variables(richness)
#dat <- richness[,c(ind)]
#cor <- findVariables(dat, VIFThreshold = 3)

r1 <- glmer(SpeciesRichness ~  ESA + scaleElevation + (scalePH  + 
                                                         scaleCLYPPT + scaleSLTPPT + scaleCECSOL + scaleORCDRC)^2 +
              (bio10_7_scaled + bio10_15_scaled + SnowMonths_cat + scaleAridity + 
                 ScalePET)^2 + 
              scaleCLYPPT:bio10_15_scaled + scaleSLTPPT:bio10_15_scaled +
              scaleCLYPPT:ScalePET + scaleSLTPPT:ScalePET +
              scaleCLYPPT:scaleAridity + scaleSLTPPT:scaleAridity +
              (1|file/Study_Name), data = richness, family = poisson,
            control = glmerControl(optCtrl = list(maxfun = 2e5), optimizer ="bobyqa"))


#saveRDS(richness, file = "data/richness.RDS")


library(mlr)
library(missForest)
tmp = sites %>% select(SpeciesRichness, ESA, elevation, ph_new, 
                       CLYPPT, SLTPPT, CECSOL, ORCDRC,bio10_7,
                       bio10_15, SnowMonths_cat, Aridity, PETyr,logBiomass, logAbundance)
tmp = tmp[complete.cases(tmp$SpeciesRichness),]

imputed = missForest::missForest(tmp[,-1])
tmp[,2:15] = imputed$ximp

tmp = cbind(tmp$SpeciesRichness, imputed$ximp)
colnames(tmp)[1] = "SpeciesRichness"

task = makeRegrTask(id = "richness",data = tmp, target = "SpeciesRichness")
task = normalizeFeatures(task)

model = train(learner, task)

learner = makeLearner("regr.ranger", importance = "impurity")
getParamSet(learner)
pars = makeParamSet(
  makeIntegerParam("mtry", 1, sum(task$task.desc$n.feat)-1L),
  makeIntegerParam("min.node.size", 5, 200))
learnerTune = makeTuneWrapper(learner, cv3, measures = mse, par.set = pars, control = makeTuneControlRandom(maxit = 30L))
parallelMap::parallelStartSocket(cpus = 5L, level = "mlr.resample")
result = resample(learnerTune,task, cv3, list(mse, rmse), models = TRUE, extract = getTuneResult, keep.pred = TRUE)

learner2 = setHyperPars(learner, par.vals = result$extract[[3]]$x)
result = resample(learner2,task, cv10, list(mse, rmse), models = TRUE,  keep.pred = TRUE)


model = train(learner, task)




##########  Biomass  ########## 
biomass <- sites[complete.cases(sites$logBiomass),] # 3689
biomass <- droplevels(biomass[biomass$ESA != "Unknown",]) # 

# biomass <- droplevels(biomass[!(is.na(biomass$PHIHOX)),])
biomass <- droplevels(biomass[!(is.na(biomass$bio10_15)),]) ## 
biomass <- droplevels(biomass[!(is.na(biomass$OCFinal)),]) ## 
biomass <- droplevels(biomass[!(is.na(biomass$phFinal)),]) ## 
biomass <- droplevels(biomass[!(is.na(biomass$SnowMonths_cat)),]) ##  
biomass <- droplevels(biomass[!(is.na(biomass$Aridity)),]) ##  3359


table(biomass$ESA)
biomass_notinclude <- c("Tree open", "Sparse vegetation", "Cropland/Other vegetation mosaic",
                        "Urban", "Paddy field")

biomass <- droplevels(biomass[!(biomass$ESA %in% biomass_notinclude),]) ##   3326

summary(biomass$phFinal)
biomass$scalePH <- as.vector(scale(biomass$phFinal))
biomass$scaleCLYPPT <- scale(biomass$ClayFinal)
biomass$scaleSLTPPT <- scale(biomass$SiltFinal)
biomass$scaleCECSOL <- scale(biomass$CECSOL)
biomass$scaleORCDRC <- scale(biomass$OCFinal)

biomass$bio10_1_scaled <- scale(biomass$bio10_1)
biomass$bio10_4_scaled <- scale(biomass$bio10_4)
biomass$bio10_7_scaled <- scale(biomass$bio10_7)
biomass$bio10_12_scaled <- scale(biomass$bio10_12)
biomass$bio10_15_scaled <- scale(biomass$bio10_15)


biomass$scaleAridity <- scale(biomass$Aridity)
biomass$ScalePET <- scale(biomass$PETyr)
biomass$ScalePETSD <- scale(biomass$PET_SD)
biomass$ScaleElevation <- scale(biomass$elevation)

ind <- df_variables(biomass)
dat <- biomass[,c(ind)]
cor <- findVariables(dat, VIFThreshold = 3)

# bio10_7, bio10_12  bio10_15, CECSOL, 
# elevation, PETyr,phFinal,ClayFinal,SiltFinal,OCFinal

## All fine

b1 <- lmer(logBiomass ~  ESA + ScaleElevation + (scalePH  + scaleCLYPPT + scaleSLTPPT + scaleORCDRC + scaleCECSOL)^2 +
             (bio10_7_scaled + bio10_12_scaled  + bio10_15_scaled + ScalePET + SnowMonths_cat)^2 + 
             scaleCLYPPT:bio10_12_scaled + scaleSLTPPT:bio10_12_scaled +
             scaleCLYPPT:bio10_15_scaled + scaleSLTPPT:bio10_15_scaled +
             (1|file/Study_Name), data = biomass,
           control = lmerControl(optCtrl = list(maxfun = 2e5), optimizer ="bobyqa"))

saveRDS(biomass, file = "data/biomass.RDS")



########## Abundance ########## 
hist(sites$logAbundance)
abundance <- sites[complete.cases(sites$logAbundance),] # 7111
abundance <- droplevels(abundance[abundance$ESA != "Unknown",]) #

# abundance <- droplevels(abundance[!(is.na(abundance$PHIHOX)),])
abundance <- droplevels(abundance[!(is.na(abundance$bio10_15)),]) ##   
abundance <- droplevels(abundance[!(is.na(abundance$OCFinal)),]) ##  
abundance <- droplevels(abundance[!(is.na(abundance$phFinal)),]) ##  
abundance <- droplevels(abundance[!(is.na(abundance$SnowMonths_cat)),]) ##  
abundance <- droplevels(abundance[!(is.na(abundance$Aridity)),]) ##  6456


table(abundance$ESA)
abundance_notinclude <- c("Needleleaf deciduous forest", "Tree open", "Sparse vegetation", "Urban", 
                          "Bare area (consolidated", "Bare area (unconsolidated",  "Paddy field", "Wetland/Herbaceous",
                          "Water bodies")

abundance <- droplevels(abundance[!(abundance$ESA %in% abundance_notinclude),]) #  


abundance$scalePH <- as.vector(scale(abundance$phFinal))
abundance$scaleCLYPPT <- scale(abundance$ClayFinal)
abundance$scaleSLTPPT <- scale(abundance$SiltFinal)
abundance$scaleCECSOL <- scale(abundance$CECSOL)
abundance$scaleORCDRC <- scale(abundance$OCFinal)

abundance$bio10_1_scaled <- scale(abundance$bio10_1)
abundance$bio10_4_scaled <- scale(abundance$bio10_4)
abundance$bio10_7_scaled <- scale(abundance$bio10_7)
abundance$bio10_12_scaled <- scale(abundance$bio10_12)
abundance$bio10_15_scaled <- scale(abundance$bio10_15)

abundance$scaleAridity <- scale(abundance$Aridity)
abundance$ScalePET <- scale(abundance$PETyr)
abundance$ScalePETSD <- scale(abundance$PET_SD)
abundance$ScaleElevation  <- scale(abundance$elevation)

#ind <- df_variables(abundance)
#dat <- abundance[,c(ind)]
#cor <- findVariables(dat, VIFThreshold = 3)

# bio10_7, bio10_15,CECSOL,elevation,Aridity,PETyr,  phFinal,ClayFinal,SiltFinal, OCFinal   

a1 <- lmer(logAbundance ~  ESA + ScaleElevation + (scalePH  + scaleCLYPPT + scaleSLTPPT + scaleCECSOL + scaleORCDRC)^2 +
             (bio10_7_scaled + bio10_15_scaled + SnowMonths_cat + scaleAridity + 
                ScalePET)^2 +
             scaleCLYPPT:bio10_15_scaled + scaleSLTPPT:bio10_15_scaled +
             scaleCLYPPT:ScalePET + scaleSLTPPT:ScalePET + 
             scaleCLYPPT:scaleAridity + scaleSLTPPT:scaleAridity + 
             (1|file/Study_Name), data = abundance,
           control = lmerControl(optCtrl = list(maxfun = 2e5), optimizer ="bobyqa"))

#saveRDS(abundance, file = "Earthworms/abundance.RDS")

#abundance = abundance[complete.cases(abundance$logBiomass) & complete.cases(abundance$SpeciesRichness), ]


# mlr: 
tmp = abundance %>% select(logAbundance, ESA, elevation, scalePH, 
                           CLYPPT, SLTPPT, CECSOL, ORCDRC,bio10_7,
                           bio10_15, SnowMonths_cat, Aridity, PETyr, SpeciesRichness, logBiomass)
tmp$logAbundance = exp(tmp$logAbundance) -1 

imputed <- missForest(tmp[,2:15])
tmp[,2:15] = imputed$ximp

# 1. create task
task = makeRegrTask(id = "abundance",data = tmp, target = "logAbundance")

# 2. normalize Features
task = normalizeFeatures(task) #unser datensatz

# 3. ranger -> random forest
learner = makeLearner("regr.ranger", importance = "impurity") # model: random forrest
getParamSet(learner)

# 4. mit 10-cv fitten -> 10fache Crossvalidierung
result = resample(learner = learner, task = task, resampling = cv3, measures = mlr::mse, models = TRUE)
importance = mlr::getFeatureImportance(result$models[[1]])

pars = makeParamSet(makeIntegerParam("mtry", 1, sum(task$task.desc$n.feat)-1L),
                    makeIntegerParam("min.node.size", 5, 200))
learnerTune = makeTuneWrapper(learner, cv3, measures = mse, par.set = pars, control = makeTuneControlRandom(maxit = 10L))
parallelMap::parallelStartSocket(cpus = 5L, level = "mlr.resample")
result = resample(learnerTune,task, cv3, list(mse, rmse))



