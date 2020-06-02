
# Library -----------------------------------------------------------------

# devtools::install_github("oharar/PointedSDMs")

library(PointedSDMs)

library(sp)
library(spatstat)
library(RColorBrewer)
# library(mapview)

data("SolTin_covariates")
str(SolTin_covariates)
Projection <- CRS("+proj=longlat +ellps=WGS84")

# Coord density to polygons
region.mask=as.owin(cbind(SolTin_covariates[,c("X","Y")], In=rep(TRUE,nrow(SolTin_covariates))),
                    step=c(0.25, 0.3))
region.mask$m[is.na(region.mask$m)] <- FALSE
Region.poly <- simplify.owin(as.polygonal(region.mask), dmin=0.5)
PolyPoints <- cbind(Region.poly$bdry[[1]]$x[c(1,length(Region.poly$bdry[[1]]$x):1)],
                    Region.poly$bdry[[1]]$y[c(1,length(Region.poly$bdry[[1]]$y):1)])
Pgon <- Polygons(list(region=Polygon(coords=PolyPoints)), ID="region")
region.polygon=SpatialPolygons(list(Pgon), proj4string = Projection)

Use <- c("Forest","NPP", "Altitude") # , "ForestQ","NPPQ", "AltitudeQ")
Covariates <- SpatialPointsDataFrame(SolTin_covariates[,c("X","Y")],
                                     data=SolTin_covariates[,Use], proj4string = Projection)
Covariates@data <-data.frame(apply(Covariates@data,2, scale))  # scale the covariates
# spplot(Covariates, layout=c(3,1), col.regions=grey(seq(0,1,length=20)), key.space="right")
spplot(Covariates, layout=c(3,1), col.regions=brewer.pal(6, "Blues")[-1], key.space="right", pretty=TRUE)


data("SolTin_ebird")
ebird <- SpatialPoints(SolTin_ebird[,c("X","Y")], proj4string = Projection)
data("SolTin_gbif")
gbif <- SpatialPoints(SolTin_gbif[,c("X","Y")], proj4string = Projection)
data("SolTin_parks")
Parks <- SpatialPointsDataFrame(SolTin_parks[,c("X","Y")],
                                data = SolTin_parks[,c("area","Present")],
                                proj4string = Projection)

# Expert polygon
data("SolTin_range")
Pgon.range <- Polygons(list(region=Polygon(coords=SolTin_range)), ID="range")
range.polygon=SpatialPolygons(list(Pgon.range), proj4string = Projection)
MapCols <- c(brewer.pal(4, "Paired"), grey(c(0.4,0.1)))
# c("blue3", "darkgreen", "pink", "red", "grey70")
names(MapCols) <- c("eBird", "GBIF", "Park, absent", "Park, present", "Region", "Expert Range")
par(mar=rep(0,4))
plot(region.polygon, col=MapCols["Region"])
plot(range.polygon, col=MapCols["Expert Range"], border=NA, add=TRUE)
points(ebird, cex=0.5, pch=19, col=MapCols["eBird"])
points(gbif, cex=0.5, pch=19, col=MapCols["GBIF"])
points(Parks, cex=0.7, pch=19, col=MapCols[c("Park, absent", "Park, present")][1+Parks@data$Present])
legend(region.polygon@bbox["x","min"]+0.01*diff(region.polygon@bbox["x",]),
       region.polygon@bbox["y","min"]+0.95*diff(region.polygon@bbox["y",]),
       legend = names(MapCols), fill = MapCols, cex=0.8)



## Tesselation
Meshpars <- list(cutoff=0.8, max.edge=c(1, 3), offset=c(1,1))
## Convert sp polygon to tesselated net
Mesh <- MakeSpatialRegion(data=NULL, bdry=region.polygon, meshpars=Meshpars,
                          proj = Projection)
## ? Find integration points to optimize error vs. comp time
stk.ip <- MakeIntegrationStack(mesh=Mesh$mesh, data=Covariates, area=Mesh$w,
                               tag='ip', InclCoords=TRUE)

## An INLA stack, with the distance to the range (expert) polygon added
stk.ip.dists <- AddDistToRangeToStack(in.stk=stk.ip, coords=c("X", "Y"),
                                      polynoms = range.polygon, scale=FALSE)
plot(Mesh$mesh)


## Create data for projections
Nxy.scale <- 0.5 # use this to change the resolution of the predictions
Boundary <- Mesh$mesh$loc[Mesh$mesh$segm$int$idx[,2],] # get the boundary of the region
Nxy <- round(c(diff(range(Boundary[,1])), diff(range(Boundary[,2])))/Nxy.scale)
stk.pred <- MakeProjectionGrid(nxy=Nxy, mesh=Mesh$mesh, data=Covariates,
                               tag='pred', boundary=Boundary)
stk.pred$stk <- AddDistToRangeToStack(in.stk=stk.pred$stk, coords=c("X", "Y"),
                                      polynoms = range.polygon, scale=FALSE)


##
stk.eBird <- MakePointsStack(presences=ebird, data=Covariates, mesh=Mesh$mesh,
                             polynoms = range.polygon, tag='ebird',
                             InclCoords=TRUE)
stk.gbif <- MakePointsStack(presences=gbif, data=Covariates, mesh=Mesh$mesh,
                            polynoms = range.polygon, tag='gbif',
                            InclCoords=TRUE)
stk.parks <- MakeBinomStack(observs=Parks, data=Covariates, mesh=Mesh$mesh,
                            presname='Present', polynoms = range.polygon,
                            tag='parks', InclCoords=TRUE)

##
SolTinModel <- FitModel(stk.eBird, # P data
                        stk.gbif, # P data
                        stk.ip.dists, # distance to the expert range polygon
                        stk.parks, # P/A data
                        stk.pred$stk, # predictor stack
                        mesh = Mesh$mesh, # tesselation mesh
                        CovNames=NULL,
                        predictions = TRUE)
summary(SolTinModel$model)$fixed


##
library(mapview)
Pred <- SpatialPixelsDataFrame(points=stk.pred$predcoords, data=SolTinModel$predictions, proj4string=Projection)
Pred@data$precision <- Pred@data$stddev^-2
ncolours <- 200
greencols.fn <- colorRampPalette(brewer.pal(9, 'Greens'))
greencols <- greencols.fn(ncolours)
bluecols.fn <- colorRampPalette(brewer.pal(9, 'Blues'))
bluecols <- bluecols.fn(ncolours)
map.mean <- mapview(Pred, zcol = c("mean"), legend = TRUE# alpha=0.2,
                    ) # col.regions=greencols
map.stddev <- mapview(Pred, zcol = c("stddev"), legend = TRUE, alpha=0.3
                      ) #col.regions=bluecols
sync(map.mean, map.stddev)



SolTinModelNoSpat <- FitModel(stk.eBird, stk.gbif, stk.ip.dists, stk.parks, stk.pred$stk,
                              CovNames=NULL, mesh = Mesh$mesh, predictions = TRUE, spat.ind = NULL)
summary(SolTinModelNoSpat$model)$fixed

##
PredNoSpat <- SpatialPixelsDataFrame(points=stk.pred$predcoords,
                                     data=SolTinModelNoSpat$predictions, proj4string=Projection)
# plot(PredNoSpat, attr="mean", col=grey(seq(0,1,length=100)))
map.nospat.mean <- mapview(PredNoSpat, zcol = c("mean"), legend = TRUE # alpha=0.2,
                           )
map.nospat.stddev <- mapview(PredNoSpat, zcol = c("stddev"), legend = TRUE, alpha=0.3
                             )
sync(map.nospat.mean, map.nospat.stddev)

##
Quad.form <- resp ~ 0 + (Forest + Altitude)^2 + I(Forest^2) + I(Altitude^2) + int.ebird + DistToPoly1 +
  int.gbif + Intercept + X + Y + int.parks + f(i, model = mesh)
SolTinModel.quad <- FitModel(stk.eBird, stk.gbif, stk.ip.dists, stk.parks, stk.pred$stk,
                             formula=Quad.form, spat.ind = NULL,
                             CovNames=NULL, mesh = Mesh$mesh, predictions = TRUE)
summary(SolTinModel.quad$model)$fixed

