## RESIDUAL Calculation ##


model_data_list <- list()

#loop over each file in the grace_clipped_list and the corresponding file in gldas_stacked_list
for (i in 1:length(grace_clipped_list)) {
  #get clipped grace raster
  grace_clip <- grace_clipped_list[[i]]
  
  #get corresponding stack of gldas predictors
  predictors <- gldas1_stacked[[i]]
  
  xy_df <- as.data.frame(grace_clip, xy=TRUE)
  
  #combine the predictor variables into a data frame
  predictors_df <- as.data.frame(predictors, xy=TRUE)
  
  #same for response variable
  response_df <- as.data.frame(grace_clip, xy=TRUE)
  
  #subset predictors data frame to exclude x and y columns
  predictors_df <- predictors_df[, -c(1, 2)]
  
  #subset response
  response_df <- response_df[, -c(1, 2)]
  
  model_data <- cbind(predictors_df, response_df)
  
  #add the model data to the master list of model data
  model_data_list[[i]] <- model_data
}

#define the extent of the new residual raster using minimum and maximum values of longitude and latitude
xmin <- -94.5
xmax <- -87.25
ymin <- 29.75
ymax <- 37.25
extent_proj <- extent(xmin, xmax, ymin, ymax)

#create empty raster to store residuals
rast <- raster(extent_proj, res = 0.25, crs = CRS("+proj=longlat +datum=WGS84"))



residuals_rasters <- list()

#loop over each month in the monthly_data_list
for (i in seq_along(model_data_list)) {
  #get the monthly data for the current month
  monthly_data <- model_data_list[[i]]
  
  #predict using the model
  predicted <- predict(model, newdata = monthly_data)
  
  #extract the response variable from the monthly data
  response_data <- monthly_data[, ncol(monthly_data)]
  
  #calculate residuals
  residuals <- response_data - predicted
  
  #create a residual raster with the same extent, resolution, and projection as grace_clip
  residual_raster <- raster(grace_clip)
  
  #ensure the values of the residual raster are set to 0
  values(residual_raster) <- 0
  
  #assign the residuals to the corresponding cells in the residual raster
  residual_raster[] <- residuals
  
  #resample to a 0.25x0.25 grid using bilinear interpolation
  residual_raster_resampled <- resample(residual_raster, rast, method = "bilinear", filename = "")
  
  #add the resampled residuals raster to the residuals_rasters list
  residuals_rasters[[i]] <- residual_raster_resampled
}
