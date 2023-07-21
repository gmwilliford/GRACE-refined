##DOWNSCALING##

downscaled_list <- list()
predictions_list <- vector("list", length(gldas025_stacked))

#loop over each file in the gldas 0.25x0.25
for (i in 1:length(gldas025_stacked)) {
  #get raster stack of gldas025 predictors
  predictors <- gldas025_stacked[[i]]
  
  #convert the stack to  data frame
  predictors_df <- as.data.frame(predictors, xy=TRUE)
  predictors_df <- predictors_df[, -c(1, 2)]
  
  #set CRS
  proj4string(predictors) <- CRS("+proj=longlat +datum=WGS84")
  
  #prediction using the  model
  predictions <- predict(model, newdata = predictors_df)
  predictions_list[[i]] <- predictions
  
  #create an empty raster
  downscale <- raster(extent_resample, res = 0.25, crs = CRS("+proj=longlat +datum=WGS84"))
  values(downscale) <- predictions
  
  #add the predicted raster to the downscaled list
  downscaled_list[[i]] <- downscale
}

##RESIDUAL CORRECTION##

corrected <- list()

#loop through each raster in the res_rasters list
for (i in seq_along(residuals_rasters)) {
  # Convert residuals and downscaled rasters to data frames
  resid_df <- as.data.frame(residuals_rasters[[i]], xy = TRUE)
  downdf <- as.data.frame(downscaled_list[[i]], xy = TRUE)

  
  # Combine the data frames based on x, y coordinates
  combined_df <- merge(downdf, resid_df, by = c("x", "y"))
  
  # Replace NA values with 0 in the residuals dataframe
  combined_df[is.na(combined_df)] <- 0
  
  # Calculate corrected values by adding residuals to downscaled values
  combined_df$corrected_value <- combined_df$layer.x + combined_df$layer.y
  
  # Remove unnecessary columns and keep only the corrected value
  correcteddf <- combined_df[, c("x", "y", "corrected_value")]
  
  # Convert correcteddf back to a raster
  corrected_raster <- rasterFromXYZ(correcteddf, res = c(0.25,0.25), CRS("+proj=longlat +datum=WGS84"))
  
  
  #add the corrected raster to the corrected list
  corrected[[i]] <- corrected_raster
}


##Calculating GWS##

#Define the variable names to sum
sum_var_names <-c("CanopInt_inst", "SWE_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst")

#LIST OF DOWNSCALED RASTERS
final_list <- list()

# Loop over each file in the gldas025_stacked list
for (i in 1:length(gldas025_stacked)) {
  sum_raster <- calc(predictors, sum, na.rm=FALSE, na.rm.data=FALSE, na.rm.interp=FALSE, na.rm.filter=FALSE, forcefun=FALSE, filename="")
  sum_raster_clipped <- crop(sum_raster, extent_resample)
  
  #get the corresponding downscaled raster
  downscaled_raster <- corrected[[i]]
  
  #GWS calculation
  final_raster <- downscaled_raster*100 - sum_raster_clipped/10
  
  #Accounting for Specific Yield 
  final_raster <- final_raster * 0.25
  
  final_list[[i]] <- final_raster
}

# Define the new CRS (NAD83, EPSG:4269)
new_crs <- CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")

# Loop through each raster in the final_list and reproject it to the new CRS
for (i in seq_along(final_list)) {
  final_list[[i]] <- projectRaster(final_list[[i]], crs = new_crs)
  final_df <- as.data.frame(final_list[[i]], xy = TRUE, na.rm = TRUE)
  final_raster2 <- rasterFromXYZ(final_df)
  final_list[[i]] <- final_raster2
}
