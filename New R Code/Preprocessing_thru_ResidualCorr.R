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
library(RNetCDF)
library(googledrive)

setwd("D:/Research/GRACE_2.0")

grace_files <- list.files(path = ".", pattern = "GRD.*\\.nc$", full.names = TRUE)
output_dir <- "D:/Research/MASTER_DATA_2.0/"

#Extracting Date Information from GRACE Files
for(file in grace_files) {
    grace_nc <- nc_open(file)
    time_coverage_start <- ncatt_get(grace_nc, 0, "time_coverage_start")$value
    time_coverage_end <- ncatt_get(grace_nc, 0, "time_coverage_end")$value
    start_date <- gsub("-", "_", substr(time_coverage_start, 1, 10))
    end_date <- gsub("-", "_", substr(time_coverage_end, 5, 10))
    new_file_name <- paste0("GRACE_", start_date, "_", end_date, ".nc")
    new_file_path <- file.path(output_dir, new_file_name)
    nc_close(grace_nc)
    file.copy(file, new_file_path, overwrite = TRUE)
}

#Clipped GRACE
setwd("D:/Research/MASTER_DATA_2.0/GRACE_ALL/2023/")
grace_files <- list.files(path = ".", pattern = "GRACE.*\\.nc$", full.names = TRUE)

grace_clipped_list <- list()

#Clipping files to U.S. extent
for(i in 1:length(grace_files)) {
  grace_nc <- nc_open(grace_files[i])
  grace_var <- "lwe_thickness"
  grace_data <- ncvar_get(grace_nc, grace_var)
  grace_mtrx <- matrix(grace_data, ncol = nrow(grace_nc$dim$lon$vals))
  grace_rst <- raster(grace_files[i], var = grace_var)
  
  #shift extent of GRACE raster by 180 degrees (necessary for proper alignment)
  grace_rst <- rotate(grace_rst)
  extent(grace_rst)
  shapefile <- st_read("D:/Research/MASTER_DATA_2.0/Shapefile/US_shape_WGS84.shp")
  #cropping GRACE by shapefile
  grace_clip <- crop(grace_rst, shapefile)
  grace_masked <- mask(grace_clip, shapefile)
  nc_close(grace_nc)
  grace_clipped_list <- append(grace_clipped_list, grace_masked)
}


#MONTHLY GLDAS 1 degree by 1 degree resolution
folder_path <- "D:/Research/MASTER_DATA_2.0/GLDAS01/2023"

file_list <- list.files(path= folder_path, pattern = "*.nc4", full.names = TRUE)
#GLDAS variables to extract
var_names <- c("Evap_tavg", "Qs_acc", "SWE_inst", "CanopInt_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst", "Rainf_f_tavg")

gldas1_stacked <- list()
# Loop over each yearly .nc4 file
for (file_name in list.files(folder_path, pattern = "\\.nc4$", full.names = TRUE)) {
  var_rasters <- list()
  
  for (var_name in var_names) {
    cur_raster <- raster(file_name, varname = var_name)
    var_rasters[[var_name]] <- cur_raster
  }
  
  # Loop over the list of rasters and crop each one
  cur_gldas1_clipped <- list()
  for (var_name in var_names) {
    cur_raster <- var_rasters[[var_name]]
    cur_raster_clipped <- crop(cur_raster, extent(shapefile))
    cur_raster_masked <- mask(cur_raster_clipped, shapefile)
    cur_gldas1_clipped[[var_name]] <- cur_raster_masked
  }
  
  cur_gldas1_stacked <- stack(cur_gldas1_clipped)
  gldas1_stacked <- append(gldas1_stacked, cur_gldas1_stacked)
}

# MONTHLY GLDAS at 0.25x0.25 resolution
folder_path025 <- "D:/Research/MASTER_DATA_2.0/GLDAS025/2023"
file025_list <- list.files(path= folder_path025, pattern = "*.nc4", full.names = TRUE)
var_names <- c("Evap_tavg", "Qs_acc", "SWE_inst", "CanopInt_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst", "Rainf_f_tavg")

gldas025_stacked <- list()

for (file_name in list.files(folder_path025, pattern = "\\.nc4$", full.names = TRUE)) {
  var_rasters <- list()
    for (var_name in var_names) {
    cur_raster <- raster(file_name, varname = var_name)
    var_rasters[[var_name]] <- cur_raster
  }

  cur_gldas025_clipped <- list()
  for (var_name in var_names) {
    cur_raster <- var_rasters[[var_name]]
    cur_raster_clipped <- crop(cur_raster, extent(shapefile))
    cur_raster_masked <- mask(cur_raster_clipped, shapefile)
    cur_gldas025_clipped[[var_name]] <- cur_raster_masked
  }
  
  cur_gldas025_stacked <- stack(cur_gldas025_clipped)
    gldas025_stacked <- append(gldas025_stacked, cur_gldas025_stacked)
}


# RANDOM FOREST MODEL #
model_data_list <- list()
for (i in 1:length(grace_clipped_list)) {
  predictors_month <- as.data.frame(gldas1_stacked[[i]], xy = TRUE)
  response_month <- as.data.frame(grace_clipped_list[[i]], xy = TRUE)
  model_data_list[[i]] <- cbind(predictors_month, lwe_thickness = response_month[, 3])  # Unique x, y
}

model_data_xy <- do.call(rbind, model_data_list)
model_data_xy <- na.omit(model_data_xy)
model_data <- model_data_xy %>% select(-c(x, y))

set.seed(123)
n <- nrow(model_data)
train_rows <- sample(1:n, size = floor(0.75 * n), replace = FALSE)
test_rows <- setdiff(1:n, train_rows)

train_data <- model_data[train_rows, ]
test_data <- model_data[test_rows, ]

tuned_mtry <- tuneRF(x = train_data[, 1:10], y = train_data$lwe_thickness, stepFactor = 1.5, improve = 1e-5, ntree = 700)
model <- randomForest(lwe_thickness ~ ., data = train_data, ntree = 700, 
                      mtry = tuned_mtry[which.min(tuned_mtry[, 2]), 1], nodesize = 6, importance = TRUE)
predictions <- predict(model, test_data[, 1:10])
mse <- mean((test_data$lwe_thickness - predictions)^2)


# Bounding box for MS Embayment
extent_resample <- extent(-94.5, -87.25, 29.75, 37.25)

# 0.25 resolution data prediction and correction
gldas025_predictors <- as.data.frame(gldas025_stacked[[1]], xy = TRUE) %>% 
  select(x, y, all_of(var_names))
predicted_lwe_thickness_025 <- predict(model, gldas025_predictors[, var_names])
gldas025_predictors$predicted_lwe <- predicted_lwe_thickness_025

# Calculate residuals at 1 degree resolution for the first month within the bounding box
predictors_1deg <- as.data.frame(gldas1_stacked[[1]], xy = TRUE) %>% 
  filter(x >= -94.5 & x <= -87.25 & y >= 29.75 & y <= 37.25)
response_1deg <- as.data.frame(grace_clipped_list[[1]], xy = TRUE) %>% 
  filter(x >= -94.5 & x <= -87.25 & y >= 29.75 & y <= 37.25)
model_data_1deg <- cbind(predictors_1deg, lwe_thickness = response_1deg[, 3])
model_data_1deg$predictions <- predict(model, model_data_1deg[, var_names])
model_data_1deg$residuals <- model_data_1deg$lwe_thickness - model_data_1deg$predictions

# Interpolate residuals to 0.25 resolution
target_raster <- raster(gldas025_stacked[[1]][[1]])
residual_raster <- rasterFromXYZ(model_data_1deg[, c("x", "y", "residuals")], 
                                 crs = CRS("+proj=longlat +datum=WGS84"))
residuals_025 <- as.data.frame(resample(residual_raster, target_raster, method = "bilinear"), xy = TRUE)
names(residuals_025) <- c("x", "y", "residuals")

# Apply residual correction
gldas025_corrected <- gldas025_predictors %>%
  left_join(residuals_025, by = c("x", "y")) %>%
  mutate(corrected_lwe = predicted_lwe + residuals) %>%
  na.omit()

# Create and plot corrected raster
corrected_raster <- rasterFromXYZ(gldas025_corrected[, c("x", "y", "corrected_lwe")], 
                                  crs = CRS("+proj=longlat +datum=WGS84"))
corrected_raster_clipped <- crop(corrected_raster, extent_resample)
plot(corrected_raster_clipped, main = "Corrected LWE 0.25 Resolution", 
     xlab = "Longitude", ylab = "Latitude", col = terrain.colors(100))
