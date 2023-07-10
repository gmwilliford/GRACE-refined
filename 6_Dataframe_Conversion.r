
#GWS DATAFRAME CONVERSION#
# Initialize an empty list to store the dataframes
df_list <- list()

# Loop over each raster in the final_list
for (i in 1:length(final_list)) {
  # Convert raster to dataframe
  df <- as.data.frame(final_list[[i]], xy = TRUE)
  
  # Add the dataframe to the list of dataframes
  df_list[[i]] <- df
}

# Combine all dataframes into a single dataframe
combined_df <- do.call(rbind, df_list)

# # Specify the file path and name for the CSV file
csv_file <- "D:/Research/MASTER DATA/GWS CSV/2012_GWSdata.csv"

# # Write the combined dataframe to the CSV file
 write.csv(combined_df, file = csv_file, row.names = FALSE)


# Specify the latitude and longitude values for the grid cells of interest
lat_lon_values <- data.frame(Latitude = c(33.625, 32.375, 35.125),
                             Longitude = c(-92.875, -92.375, -89.625))

# Initialize an empty list to store the extracted dataframes
extracted_df_list <- list()

# Loop over each dataframe in the df_list
for (i in 1:length(df_list)) {
  # Extract the data for the grid cells of interest
  extracted_df <- df_list[[i]][df_list[[i]]$x %in% lat_lon_values$Longitude & df_list[[i]]$y %in% lat_lon_values$Latitude, ]
  
  # Add the extracted dataframe to the list of extracted dataframes
  extracted_df_list[[i]] <- extracted_df
}

# Combine all extracted dataframes into a single dataframe
extracted_combined_df <- do.call(rbind, extracted_df_list)

# Specify the file path and name for the extracted CSV file
extracted_csv_file <- "D:/Research/MASTER DATA/GWS CSV/2012_GWSlatlon.csv"

# Write the extracted combined dataframe to the CSV file
write.csv(extracted_combined_df, file = extracted_csv_file, row.names = FALSE)

