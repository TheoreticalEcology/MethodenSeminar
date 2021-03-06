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
if (!dir.exists('Earthworm/rasters')) system('mkdir Earthworm/rasters')

# Download rasters -----------------------------------------------------------------

# source('Earthworm/multivariate_response.R')
# varname <- names(tmp)
# responsename <- c("logAbundance", "logBiomass", "SpeciesRichness")
# predname <- varname[!varname %in% responsename]
## dput:
predname <- c("ESA","elevation", "ph_new", "CLYPPT", "SLTPPT", "CECSOL",
              "ORCDRC", "bio10_7", "SnowMonths_cat", "Aridity", "PETyr")

url <- c(
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA,
  NA)

projection <- NA

Rasters <- data.frame(name = predname, url, path = NA, projection)

Rasters$path[!is.na(url)] <- paste('Earthworm/rasters', paste0(predname, '.tif'), sep = '/')[!is.na(url)]

download.file(Rasters$url, target = Rasters$path)


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
  names(rasters) <- names

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
ps <- scale(predstack, scale = T)

# Predict -----------------------------------------------------------------
## ?raster::predict
