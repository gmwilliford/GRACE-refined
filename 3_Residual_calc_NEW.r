## RESIDUAL Calculation ##
xmin <- -94.5
xmax <- -87.25
ymin <- 29.75
ymax <- 37.25
extent_resample <- extent(xmin, xmax, ymin, ymax)


residuals <- test_data$response_df - predictions

#create empty raster to store residuals
rast <- raster(extent_resample, res = 0.25, crs = CRS("+proj=longlat +datum=WGS84"))
response_df <- do.call(rbind, lapply(grace_clipped_list, function(x) as.data.frame(x, xy = TRUE)))
response_df$residuals <- 0
# Assign the calculated residuals to the corresponding rows
response_df$residuals[test_rows] <- residuals

# Calculate the number of rows in each group (49 rows in your case)
rows_per_group <- 49

# Calculate the total number of groups
total_groups <- nrow(response_df) %/% rows_per_group

# Split the dataframe into a list of dataframes, each containing 49 consecutive rows
split_data <- split(response_df, rep(1:total_groups, each = rows_per_group, length.out = nrow(response_df)))

for (i in seq_along(split_data)) {
  assign(paste0("grace_nc_", i), split_data[[i]])
}
monthly_data_list <- split_data


residuals_dataframes <- list()
response_dataframes <- list()

# Loop over each monthly dataframe
for (i in seq_along(monthly_data_list)) {
  # Get the current monthly dataframe
  monthly_data <- monthly_data_list[[i]]
  
  # Extract x and y columns
  xy_data <- monthly_data[, c("x", "y")]
  
  # Extract residuals column and create a new dataframe
  residuals_df <- data.frame(x = xy_data$x, y = xy_data$y, residuals = monthly_data$residuals)
  
  # Extract "Liquid_Water_Equivalent_Thickness" column and create a new dataframe
  response_df <- data.frame(x = xy_data$x, y = xy_data$y, Liquid_Water_Equivalent_Thickness = monthly_data$Liquid_Water_Equivalent_Thickness)
  
  # Add the dataframes to the respective lists
  residuals_dataframes[[i]] <- residuals_df
  response_dataframes[[i]] <- response_df
}

#create empty raster to store residuals
rast <- raster(extent_resample, res = 0.25, crs = CRS("+proj=longlat +datum=WGS84"))

residuals_rasters <- list()

#loop over each month in the monthly_data_list
for (i in seq_along(residuals_dataframes)) {
  
  # Get the current monthly residuals dataframe
  residuals_df <- residuals_dataframes[[i]]
  
  # Create a raster with the same extent, resolution, and projection as grace_clip
  residual_raster <- raster(grace_clip)
  
  # Set the raster values to the residuals using the x and y coordinates
  residual_raster[] <- residuals_df$residuals
  
  #resample to a 0.25x0.25 grid using bilinear interpolation
  residual_raster_resampled <- resample(residual_raster, rast, method = "bilinear", filename = "")
  
  #add the resampled residuals raster to the residuals_rasters list
  residuals_rasters[[i]] <- residual_raster_resampled
}
