
#GWS RASTER TO DATAFRAME CONVERSION#

# Create an empty list to store the centroid dataframes 
centroid_list <- list()

# Loop over each raster in the final_list
for (i in 1:length(final_list)) {
  # Get the current raster
  raster_data <- final_list[[i]]
  
  # Calculate the centroids of the raster cells
  centroids <- raster::cellFromXY(raster_data, raster::xyFromCell(raster_data, 1:ncell(raster_data)))
  
  # Get the coordinates and values of the centroids
  centroid_coords <- raster::coordinates(raster_data)[centroids, ]
  centroid_values <- raster_data[centroids]
  
  # Create a dataframe with x, y, and value columns
  centroid_df <- data.frame(x = centroid_coords[, 1], y = centroid_coords[, 2], value = centroid_values)
  
  # Add the centroid dataframe to the list
  centroid_list[[i]] <- centroid_df
}


#COMBINING YEARLY DATAFRAMES# 

# Combine all dataframes into a single dataframe
combined_df <- do.call(rbind, centroid_list)


# WRITING DF TO CSV
# Specify the file path and name for the CSV file
csv_file <- "D:/Research/MASTER DATA/GWS CSV/2012_GWSdata.csv"

# # Write the combined dataframe to the CSV file
 write.csv(combined_df, file = csv_file, row.names = FALSE)


