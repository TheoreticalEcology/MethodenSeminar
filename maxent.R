library(rmaxent)
library(raster)
library(dismo)
library(MIAmaxent)
library(rasterVis)
library(viridis)

occ_file <- system.file('ex/bradypus.csv', package='dismo')
occ <- read.table(occ_file, header=TRUE, sep=',')[,-1]

pred_files <- list.files(system.file('ex', package='dismo'), '\\.grd$', full.names=TRUE )
predictors <- stack(pred_files)

me <- maxent(predictors, occ, factors='biome', args=c('hinge=false', 'threshold=false'))
me

prediction <- project(me, predictors)

levelplot(prediction$prediction_logistic, margin=FALSE, col.regions=viridis, at=seq(0, 1, len=100)) +
  layer(sp.points(SpatialPoints(occ), pch=20, col=1))


EV1 <- raster(list.files(system.file("extdata", "EV_continuous", 
                                     package="MIAmaxent"), full.names=TRUE)[1])
PO <- read.csv(system.file("extdata", "occurrence_PO.csv", package="MIAmaxent"))
plot(EV1, legend=FALSE)
points(PO$POINT_X, PO$POINT_Y, pch = 20, cex = 0.5, col = 'blue')



grasslandPO <- readData(
  occurrence=system.file("extdata", "occurrence_PO.csv", package="MIAmaxent"), 
  contEV=system.file("extdata", "EV_continuous", package="MIAmaxent"),
  catEV=system.file("extdata", "EV_categorical", package="MIAmaxent"),
  maxbkg=20000)

library(maxnet)


data(bradypus)
p <- bradypus$presence
data <- bradypus[,-1]
mod <- maxnet(p, data)
plot(mod, type="cloglog")
mod <- maxnet(p, data, maxnet.formula(p, data, classes="lq"))
plot(mod, "tmp6190_ann")

mod
predict(mod, data, type = "logistic")

# simulation