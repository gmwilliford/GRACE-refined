##GWS MAPPING###

plot(final_list[[1]], xlab = "lon", ylab = "lat")
plot(st_geometry(shapefile_proj), border = "black", lwd = 2, col = "transparent", add = TRUE)

#stack of final list (every month in year)
final_stack <- stack(final_list)

color_scale <- colorRampPalette(c("brown", "white", "red"))
#reverse color scale
color_scale <- rev(color_scale(1000))
custom_intervals <- seq(-50, 50, by = 5)

#plot the raster stack with the reversed color scale and custom intervals
rasterVis::levelplot(final_stack, col.regions = color_scale, main = "Change in GWS anomaly",
                     at = custom_intervals, colorkey = TRUE)

###EXPORT AS GEOTIFF###
# Specify the output directory where the GeoTIFF files will be saved
output_dir <- "D:/Research/GRACE/GeoTiff_GWS"

# Export each raster in the final_list as a GeoTIFF file
for (i in seq_along(final_list)) {
  # Set the file name for the GeoTIFF file
  file_name <- paste0("month_", i, ".tif")
  
  # Export the raster as a GeoTIFF file
  writeRaster(final_list[[i]], filename = file.path(output_dir, file_name), format = "GTiff")
}

##MONTHLY AVERAGES###
gws_list <- list()

##Monthly GWS for plot##
for (i in 1:length(final_list)) {
  
  # Calculate the mean of all raster cells for each raster layer (year)
  gws_avg <- cellStats(final_list[[i]], stat = "mean", na.rm = TRUE, bylayer = TRUE)
  
  # Convert RasterLayer to data.frame
  gws_avg_df <- as.data.frame(gws_avg)
  
  # Add the averaged GRACE raster to the list of averaged rasters
  gws_list[[i]] <- gws_avg_df
}
