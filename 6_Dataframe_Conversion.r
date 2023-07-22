
#GWS RASTER TO DATAFRAME CONVERSION#


centroid_list <- list()

for (i in 1:length(final_list)) {

  raster_data <- final_list[[i]]
  centroids <- raster::cellFromXY(raster_data, raster::xyFromCell(raster_data, 1:ncell(raster_data)))
  
  # Get the coordinates and values of the centroids
  centroid_coords <- raster::coordinates(raster_data)[centroids, ]
  centroid_values <- raster_data[centroids]
  centroid_df <- data.frame(x = centroid_coords[, 1], y = centroid_coords[, 2], value = centroid_values)
  centroid_list[[i]] <- centroid_df
}


#COMBINING YEARLY DATAFRAMES# 
combined_df <- do.call(rbind, centroid_list)


# WRITING DF TO CSV

csv_file <- "D:/Research/MASTER DATA/GWS CSV/All_GWSdata.csv"
 write.csv(combined_df, file = csv_file, row.names = FALSE)
 
 
 
 
 
 ###MEMPHIS CENTROID####

 centroid_list <- list()
 
 # Coordinates of the point of interest
 target_lon <- -89.875
 target_lat <- 35.125
 

 for (i in 1:length(final_list)) {
   raster_data <- final_list[[i]]
   target_cell <- cellFromXY(raster_data, c(target_lon, target_lat))
   target_coords <- coordinates(raster_data)[target_cell, ]
   target_value <- raster_data[target_cell]
   target_centroid_data <- data.frame(value = target_value)
   centroid_list[[i]] <- target_centroid_data
 }
 

 combined_df <- do.call(rbind, centroid_list)
 

 csv_file <- "D:/Research/MASTER DATA/GWS CSV/Memphis_GWSdataAGAIN.csv"
 write.csv(combined_df, file = csv_file, row.names = FALSE)
