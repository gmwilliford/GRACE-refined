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
  downscale <- raster(extent_proj, res = 0.25, crs = CRS("+proj=longlat +datum=WGS84"))
  values(downscale) <- predictions
  
  #add the predicted raster to the downscaled list
  downscaled_list[[i]] <- downscale
}

##RESIDUAL CORRECTION##

corrected <- list()

#loop through each raster in the res_rasters list
for (i in seq_along(residuals_rasters)) {
  #get the current raster from res_rasters and downscaled_list
  res_raster <- residuals_rasters[[i]]
  downscaled_raster <- downscaled_list[[i]]
  
  #add the residual raster to the downscaled raster for residual correction
  corrected_raster <- res_raster + downscaled_raster
  
  #add the corrected raster to the corrected list
  corrected[[i]] <- corrected_raster
}

correcteddf <- as.data.frame(corrected[[1]])
resid_df <- as.data.frame(residuals_rasters[[1]], xy =TRUE)
downdf <- as.data.frame(downscaled_raster[[1]])

test <- cbind(resid_df, downdf, correcteddf)

##Calculating GWS##

#Define the variable names to sum
sum_var_names <-c("CanopInt_inst", "SWE_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst")

#LIST OF DOWNSCALED RASTERS
final_list <- list()

# Loop over each file in the gldas025_stacked list
for (i in 1:length(gldas025_stacked)) {
  predictors <- gldas025_stacked[[i]]
  sum_raster <- calc(predictors, sum, na.rm=FALSE, na.rm.data=FALSE, na.rm.interp=FALSE, na.rm.filter=FALSE, forcefun=FALSE, filename="")
  sum_raster_clipped <- crop(sum_raster, extent_proj)
  
  #get the corresponding downscaled raster
  downscaled_raster <- downscaled_list[[i]]
  
  #GWS calculation
  final_raster <- downscaled_raster*1000 - sum_raster_clipped/1000
  
  #Accounting for Specific Yield 
  final_raster <- final_raster * 0.25
  
  final_list[[i]] <- final_raster
}
