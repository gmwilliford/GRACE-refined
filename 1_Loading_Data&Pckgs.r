library(ncdf4)
library(raster)
library(randomForest)
library(caret)
library(rgdal)
library(sf)
library(RANN)
library(rasterVis)
library(akima)
library(dplyr)
library(grid)
library(data.table)

#MONTHLY GRACE#


setwd("D:/Research/MASTER DATA/GRACE NC/2012")

#list to hold clipped GRACE files
grace_clipped_list <- list()
grace_files <- list.files(path = ".", pattern = "GRD.*\\.nc$", full.names = TRUE)

#Loop through GRACE files and process each
for (i in 1:length(grace_files)) {
  #open
  grace_nc <- nc_open(grace_files[i])
  grace_var <- "lwe_thickness"
  grace_data <- ncvar_get(grace_nc, grace_var)
  grace_mtrx <- matrix(grace_data, ncol = nrow(grace_nc$dim$lon$vals))
  grace_rst <- raster(grace_files[i])
  
  #shift extent of GRACE raster by 180 degrees (necessary for proper alignment)
  grace_rst <- rotate(grace_rst, angle = 180, transform = TRUE)
  extent(grace_rst)
  
  #MS Embayment Shapefile
  shapefile <- st_read("D:/Research/MS_Embayment_SHP/Mississippi_embayment.shp")
  shapefile_proj <- st_transform(shapefile, st_crs(grace_rst))
  
  #cropping GRACE by shapefile
  grace_clip <- crop(grace_rst, shapefile_proj)
  
  #project into UTM
  grace_utm <- projectRaster(grace_clip, crs = "+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs")
  
  #close file
  nc_close(grace_nc)
  
  #append to list
  grace_clipped_list <- append(grace_clipped_list, grace_clip)
}


#MONTHLY GLDAS 
folder_path <- "D:/Research/MASTER DATA/GLDAS/1x1/2012"
file_list <- list.files(path= folder_path, pattern = "*.nc4")

#define the variables to extract
var_names <- c("Evap_tavg", "Qs_acc", "SWE_inst", "CanopInt_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst", "Rainf_f_tavg")

#initialize  empty list to hold the stacked rasters for all gldas 1x1 variables
gldas1_stacked <- list()

#loop over each yearly .nc4 file
for (file_name in list.files(folder_path, pattern = "\\.nc4$", full.names = TRUE)) {
  # Initialize an empty list to hold the rasters for the current file
  var_rasters <- list()
  
  #loop over variable names and extract each one as a raster
  for (var_name in var_names) {
    # Get the raster for the current variable
    cur_raster <- raster(file_name, varname = var_name)
    # Add the raster to the list of rasters for the current file
    var_rasters[[var_name]] <- cur_raster
  }
  
  #loop over the list of rasters and crop each one
  cur_gldas1_clipped <- list()
  for (var_name in var_names) {
    cur_raster <- var_rasters[[var_name]]
    cur_raster_clipped <- crop(cur_raster, extent(shapefile_proj))
    cur_gldas1_clipped[[var_name]] <- cur_raster_clipped
  }
  
  #stack the clipped rasters for the current file
  cur_gldas1_stacked <- stack(cur_gldas1_clipped)
  
  #append the stacked rasters for the current file to the list of stacked rasters for all files
  gldas1_stacked <- append(gldas1_stacked, cur_gldas1_stacked)
}


#MONTHLY GLDAS 0.25x0.25

folder_path025 <- "D:/Research/MASTER DATA/GLDAS/0.25x0.25/2012"
file025_list <- list.files(path= folder_path, pattern = "*.nc4")
var_names <- c("Evap_tavg", "Qs_acc", "SWE_inst", "CanopInt_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst", "Rainf_f_tavg")

gldas025_stacked <- list()

#loop over each yearly .nc4 file
for (file_name in list.files(folder_path025, pattern = "\\.nc4$", full.names = TRUE)) {
  # Initialize an empty list to hold the rasters for the current file
  var_rasters <- list()
  
  #loop over variable names and extract each one as a raster
  for (var_name in var_names) {
    # Get the raster for the current variable
    cur_raster <- raster(file_name, varname = var_name)
    # Add the raster to the list of rasters for the current file
    var_rasters[[var_name]] <- cur_raster
  }
  #loop over the list of rasters and crop each one
  cur_gldas025_clipped <- list()
  for (var_name in var_names) {
    cur_raster <- var_rasters[[var_name]]
    cur_raster_clipped <- crop(cur_raster, extent(shapefile_proj))
    cur_gldas025_clipped[[var_name]] <- cur_raster_clipped
  }
  
  cur_gldas025_stacked <- stack(cur_gldas025_clipped)
  
  #append the stacked rasters for the current file to the list of stacked rasters for all files
  gldas025_stacked <- append(gldas025_stacked, cur_gldas025_stacked)
}

