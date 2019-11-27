##########################################################################################
# Spatial rasters  ---------------------------------------
##########################################################################################

# Library -----------------------------------------------------------------
library(rgdal) # Depending on operating system, gdal has to be installed on the system.
library(sp)
library(raster)
library(here)

# … -----------------------------------------------------------------
setwd(here()) # setwd to git root.
if (!dir.exists('Earthworm/rasters')) system('mkdir Earthworm/rasters') # Windows users will have to manually make the directory.

# Download rasters -----------------------------------------------------------------

# source('Earthworm/multivariate_response.R')
# varname <- names(tmp)
# responsename <- c("logAbundance", "logBiomass", "SpeciesRichness")
# predname <- varname[!varname %in% responsename]
## dput(predname):
predname <- c("ESA","elevation", "ph_new", "CLYPPT", "SLTPPT", "CECSOL",
              "ORCDRC", "bio10_7", "SnowMonths_cat", "Aridity", "PETyr")

url <- c(
  NA,
  'https://asterweb.jpl.nasa.gov/images/GDEM-10km-BW.tif',
  'https://files.isric.org/soilgrids/data/aggregated/10km/PHIKCL_M_sl1_10km_ll.tif',
  NA,
  'https://files.isric.org/soilgrids/data/aggregated/10km/SLTPPT_M_sl1_10km_ll.tif',
  'https://files.isric.org/soilgrids/data/aggregated/10km/CECSOL_M_sl1_10km_ll.tif',
  'https://files.isric.org/soilgrids/data/aggregated/10km/ORCDRC_M_sl1_10km_ll.tif',
  NA, # Worldclim: Mean Temperature of Warmest Quarter; raster::getData('worldclim', var='bio10', res=10, lon=5, lat=45)
  NA,
  NA,
  NA)


Rasters <- data.frame(name = predname, url, path = NA, projection = NA, stringsAsFactors = F)




.gg = sapply(1:length(Rasters$url[!is.na(url)]), 
             function(u) download.file(Rasters$url[!is.na(url)][u], destfile = Rasters$path[!is.na(url)][u], method = "wget"))

Rasters$path[!is.na(url)] <- paste('Earthworm/rasters', paste0(predname, '.tif'), sep = '/')[!is.na(url)]
Rasters$projection[!is.na(Rasters$path)] <- sapply(Rasters$path[!is.na(Rasters$path)], function(r) projection(raster(r)))


# Stack rasters -----------------------------------------------------------------
## ?raster::stack

## Stacking geo raster files with different resolutions, crs, and extents.
## Value: RasterStack
stackRasters <- function(path,
                         projection,
                         name = NA,
                         targetextent = NULL,
                         targetres = NULL,
                         targetcrs = NULL){


  rasters <- lapply(path, raster::raster)
  setProj <- function(r, p){
    if (is.na(raster::projection(r))) {
      raster::projection(r) <- p
    }
    return(r)
  }
  rasters <- mapply(setProj, rasters, projection, SIMPLIFY = F)
  names(rasters) <- name

  ## Project all rasters to one CRS resolution.
  if (is.null(targetres)) {
    res_x <- sapply(rasters, raster::xres) # get the x resolutions
    i_bestres <- res_x == min(res_x) # get all rasters which share the best resolution
    bestraster  <- rasters[i_bestres][[1]]
    cat(bestraster@file@name, "is the target raster for reprojecting ... \n")

    rasters[!i_bestres] <- lapply(rasters[!i_bestres], function(r){
      cat("  - ", r@file@name, "\n")
      raster::projectRaster(from = r, to = bestraster)}) # Also projects to the target extent!

  } else {
    rasters <- mapply(rasters, raster::projectRaster, res = targetres, crs = targetcrs)
  }

  if(is.null(targetextent)) targetextent <- raster::extent(bestraster)

  ## Cropping to targetextent after reprojection.
  rasters <- lapply(rasters, function(r) raster::crop(r, targetextent))
  fromdisk <- sapply(rasters, raster::fromDisk) # are the data from disk instead of RAM?
  # rasters[fromdisk] <- lapply(rasters[fromdisk], function(r) raster::readAll(r)) # force the data in rasters into the object in memory
  stack <- raster::stack(rasters)
  return(stack)
}

R <- na.omit(Rasters)
predstack <- stackRasters(R$path, R$projection, R$name)
predstack <- raster::scale(predstack, scale = T)
plot(predstack)
# plot(predstack)


df = predstack@data@values
df = df[complete.cases(df),]
head(df)


# --------------------
n <- 100
x = matrix(runif(6*n, -1, 1), ncol = 6)

library(keras)
library(dplyr)

data = read.csv("Earthworm/1804_2_sWormModelData.csv")[,-1]
str(data)
names(data)
sites = 
  data %>% 
  filter(Study_Name!="birkhofer2013")
tmp = sites %>% select(logAbundance,logBiomass, SpeciesRichness, elevation, ph_new, 
                       SLTPPT, CECSOL, ORCDRC)
imputed = missRanger::missRanger(tmp[,-c(1:3)])
tmp = cbind(tmp[,1:3], imputed)
tmp2 = tmp[complete.cases(tmp[,1:3]),]
sub = mlr::createDummyFeatures(mlr::normalizeFeatures(obj = tmp2[,4:ncol(tmp2)]))
X = as.matrix(sub)
Y = tmp2[,1:3]

dnn = keras_model_sequential()
dnn %>% 
  layer_dense( input_shape = ncol(X),units = 25L, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 25L, activation = "relu") %>% 
    layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 1L, activation = NULL)

dnn %>% 
  compile(loss = keras::loss_mean_squared_error, optimizer = keras::optimizer_adamax(lr = 0.01))

hist = 
  dnn %>% 
  fit(x = as.matrix(X), y = as.matrix(Y[,3,drop = F]), validation_split = 0.2, epochs = 85L,shuffle = TRUE)

pred_function = function(model, data) {
  pred = predict(model, as.matrix(data))[,1]
  return(cbind(p = pred, se = rep(0.01, length(pred))))
}

ppp = predict(predstack, dnn, fun = pred_function, index = 1)
plot(ppp, col = topo.colors(10))
