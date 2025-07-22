library(ncdf4)
library(raster)
library(randomForest)
library(sf)
library(dplyr)
library(ggplot2)

# Constants
EARTH_RADIUS_KM <- 6371
DAYS_PER_MONTH <- 30.44
SECONDS_PER_MONTH <- DAYS_PER_MONTH * 86400


# Loading shapefile and extents 
shapefile <- st_read("D:/Research/MASTER_DATA_2.0/Shapefile/US_shape_WGS84.shp")
ms_embayment <- extent(-94.5, -87.25, 29.75, 37.25)
conus_extent <- extent(-125, -66, 24, 50)

# Process GRACE data & convert units
process_grace <- function(files) {
  lapply(files, function(file) {
    grace_rst <- rotate(raster(file, varname = "lwe_thickness"))  
    grace_clip <- mask(crop(grace_rst, shapefile), shapefile)
    grace_clip <- crop(grace_clip, conus_extent)
    
    # Convert to km³
    lat <- init(grace_clip, "y")
    pixel_area_km2 <- (EARTH_RADIUS_KM * pi/180)^2 * cos(abs(lat) * pi/180)
    grace_clip * pixel_area_km2 
  })
}

grace_files <- list.files("D:/Research/MASTER_DATA_2.0/GRACE_ALL/2023/", 
                          pattern = "GRACE.*\\.nc$", full.names = TRUE)
grace_clipped_list_km3 <- process_grace(grace_files)

# Define GLDAS variables 
storage_vars <- c("SWE_inst", "CanopInt_inst", "RootMoist_inst", 
                  "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", 
                  "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst")
flux_vars <- c("Evap_tavg", "Qs_acc", "Rainf_f_tavg")
var_names <- c(flux_vars, storage_vars)

# Process GLDAS data
process_gldas <- function(folder_path, res_degree) {
  lapply(list.files(folder_path, "\\.nc4$", full.names = TRUE), function(file_name) {
    stack <- stack(lapply(var_names, function(v) {
      r <- raster(file_name, varname = v)
      r_cropped <- crop(r, conus_extent)
      if(exists("shapefile")) mask(r_cropped, shapefile) else r_cropped
    }))
    names(stack) <- var_names
    
    lat <- init(stack[[1]], "y")
    pixel_area_km2 <- (EARTH_RADIUS_KM * pi/180 * res_degree)^2 * cos(abs(lat) * pi/180)
    
    # Storage variables
    for(var in storage_vars) {
      stack[[paste0(var, "_km3")]] <- stack[[var]] * pixel_area_km2 * 1e-3
    }
    
    # Flux variables =
    for(var in flux_vars) {
      stack[[paste0(var, "_km3_month")]] <- stack[[var]] * SECONDS_PER_MONTH * pixel_area_km2 * 1e-3
    }
    
    stack[[c(paste0(storage_vars, "_km3"), paste0(flux_vars, "_km3_month"))]]
  })
}

# Process GLDAS datasets 
gldas1_stacked_km3 <- process_gldas("D:/Research/MASTER_DATA_2.0/GLDAS01/2023", 1.0)
gldas025_2023_km3 <- process_gldas("D:/Research/MASTER_DATA_2.0/GLDAS025/2023", 0.25)
gldas025_full_km3 <- process_gldas("D:/Research/MASTER_DATA_2.0/GLDAS025/", 0.25)

# Calculate long-term storage means for anomaly calculation
storage_means_km3 <- list()
for(var in paste0(storage_vars, "_km3")) {
  var_stack <- stack(lapply(gldas025_full_km3, function(x) {
    if(var %in% names(x)) crop(x[[var]], ms_embayment)
  }))
  storage_means_km3[[var]] <- calc(var_stack, mean, na.rm = TRUE)
}

# Prepare model data
prepare_model_data <- function(grace_list, gldas_list) {
  monthly_data <- list()
  for(i in seq_along(grace_list)) {
    grace_coords <- as.data.frame(grace_list[[i]], xy=TRUE)[,1:2]
    gldas_coords <- as.data.frame(gldas_list[[i]], xy=TRUE)[,1:2]
    common_coords <- merge(grace_coords, gldas_coords, by=c("x","y"))
    if(nrow(common_coords) == 0) {
      warning(paste("No common coordinates found for month", i))
      next
    }
    grace_values <- extract(grace_list[[i]], common_coords[,1:2])
    gldas_values <- as.data.frame(extract(gldas_list[[i]], common_coords[,1:2]))
    monthly_data[[i]] <- cbind(gldas_values, lwe_thickness_km3=grace_values)
  }
  do.call(rbind, monthly_data) %>% na.omit()
}

model_data <- prepare_model_data(grace_clipped_list_km3, gldas1_stacked_km3)

# Random Forest Model
set.seed(123)
train_idx <- sample(nrow(model_data), 0.80 * nrow(model_data))
model <- randomForest(lwe_thickness_km3 ~ ., data = model_data[train_idx, ], 
                      ntree = 700, importance = TRUE)

# Residual correction 
residual_correction <- function(month_index) {
  tryCatch({
    highres_data <- gldas025_2023_km3[[month_index]]
    lowres_data <- gldas1_stacked_km3[[month_index]]
    grace_data <- grace_clipped_list_km3[[month_index]]
    
    highres_df <- as.data.frame(highres_data, xy = TRUE, na.rm = TRUE)
    highres_df$predicted <- predict(model, highres_df[, grep("_km3", names(highres_df))])
    
    lowres_df <- as.data.frame(lowres_data, xy = TRUE, na.rm = TRUE)
    lowres_df$predicted <- predict(model, lowres_df[, grep("_km3", names(lowres_df))])
    
    grace_df <- as.data.frame(grace_data, xy = TRUE, na.rm = TRUE)
    names(grace_df)[3] <- "observed"
    
    residuals_df <- lowres_df %>%
      inner_join(grace_df, by = c("x", "y")) %>%
      mutate(residual = observed - predicted)
    
    residual_raster <- rasterFromXYZ(residuals_df[, c("x", "y", "residual")], crs = crs(highres_data))
    resampled_residual <- resample(residual_raster, highres_data[[1]], method = "bilinear")
    
    residual_values <- as.data.frame(resampled_residual, xy = TRUE)
    names(residual_values)[3] <- "residual"
    
    corrected_df <- highres_df %>%
      left_join(residual_values, by = c("x", "y")) %>%
      mutate(corrected = predicted + ifelse(is.na(residual), 0, residual)) %>%
      filter(!is.na(corrected))
    
    rasterFromXYZ(corrected_df[, c("x", "y", "corrected")], crs = crs(highres_data)) %>% 
      crop(ms_embayment)
  }, error = function(e) {
    message(sprintf("Error processing month %d: %s", month_index, e$message))
    return(NULL)
  })
}

corrected_rasters <- lapply(seq_along(gldas025_2023_km3), residual_correction)
corrected_rasters <- Filter(Negate(is.null), corrected_rasters)

# Calculate storage var anomalies 
calculate_storage_anomalies <- function(monthly_data, means) {
  lapply(monthly_data, function(month_data) {
    anomaly_stack <- stack()
    for(var in names(means)) {
      if(var %in% names(month_data)) {
        cropped_layer <- crop(month_data[[var]], ms_embayment)
        anomaly_stack <- addLayer(anomaly_stack, cropped_layer - means[[var]])
      }
    }
    if(nlayers(anomaly_stack) > 0) sum(anomaly_stack) else NULL
  })
}

storage_anomalies_2023 <- calculate_storage_anomalies(gldas025_2023_km3, storage_means_km3)

# Calculate GWS anomalies 
calculate_gws_anomalies <- function(corrected_tws, storage_anomalies) {
  lapply(seq_along(corrected_tws), function(i) {
    if(!is.null(storage_anomalies[[i]])) {
      storage_anomaly <- crop(storage_anomalies[[i]], corrected_tws[[i]])
      corrected_tws[[i]] - storage_anomaly
    }
  })
}

gws_anomalies <- calculate_gws_anomalies(corrected_rasters, storage_anomalies_2023)

install.packages("spData")
library(spData)
state_shape <- st_as_sf(us_states)
state_boundaries <- st_crop(state_shape, ms_embayment)



# Visualization 
plot_results <- function(month_index = 1) {
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  plot(corrected_rasters[[month_index]], 
       main = paste("Corrected TWS (km³) - Month", month_index),  
       col = rev(hcl.colors(100, "BluYl")))
  plot(state_boundaries, add = TRUE, col = NA, border = "black", lwd = 1)
  plot(storage_anomalies_2023[[month_index]], 
       main = "Storage Variable Anomaly (km³)",  
       col = rev(hcl.colors(100, "Purples")))
  plot(state_boundaries, add = TRUE, col = NA, border = "black", lwd = 1)
  plot(gws_anomalies[[month_index]], 
       main = "GWS Anomaly (km³)",  
       col = rev(hcl.colors(100, "Blue-Red")))
  plot(state_boundaries, add = TRUE, col = NA, border = "black", lwd = 1)
  var_imp <- importance(model)
  dotchart(sort(var_imp[, "%IncMSE"]), main = "Variable Importance (%IncMSE)")
}

plot_results(5)


# Summary statistics 
generate_stats <- function(month_index) {
  if(month_index <= length(corrected_rasters)) {
    data.frame(
      Metric = c("TWS", "Storage_Anomaly", "GWS_Anomaly"),
      Min = c(cellStats(corrected_rasters[[month_index]], min),
              cellStats(storage_anomalies_2023[[month_index]], min),
              cellStats(gws_anomalies[[month_index]], min)),
      Mean = c(cellStats(corrected_rasters[[month_index]], mean),
               cellStats(storage_anomalies_2023[[month_index]], mean),
               cellStats(gws_anomalies[[month_index]], mean)),
      Max = c(cellStats(corrected_rasters[[month_index]], max),
              cellStats(storage_anomalies_2023[[month_index]], max),
              cellStats(gws_anomalies[[month_index]], max))
    )
  }
}

lapply(1:3, generate_stats)

