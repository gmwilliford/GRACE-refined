#Calculating Anomalies for Surficial Variables

surface_var_names <- c("CanopInt_inst", "SWE_inst", "RootMoist_inst", "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst")

surface_var_list <- list()

for (file_name in list.files(folder_path025, pattern = "\\.nc4$", full.names = TRUE)) {
  surface_var_rasters <- list()
  
  for (var_name in surface_var_names) {
    cur_raster <- raster(file_name, varname = var_name)
    cur_raster_clipped <- crop(cur_raster, extent_resample)
    surface_var_rasters[[var_name]] <- cur_raster_clipped
  }
  
  surface_var_list <- c(surface_var_list, list(surface_var_rasters))
}

#lists for each surface variable
CanopInt_list <- list()
SWE_list <- list()
Rootmoist_list <- list()
SoilMoi0_10cm_list <- list()
SoilMoi10_40cm_list <- list()
SoilMoi40_100cm_list <- list()
SoilMoi100_200cm_list <- list()


for (i in 1:68) {
  CanopInt_list[[i]] <- surface_var_list[[i]]$CanopInt_inst
  SWE_list[[i]] <- surface_var_list[[i]]$SWE_inst
  Rootmoist_list[[i]] <- surface_var_list[[i]]$RootMoist_inst
  SoilMoi0_10cm_list[[i]] <- surface_var_list[[i]]$SoilMoi0_10cm_inst
  SoilMoi10_40cm_list[[i]] <- surface_var_list[[i]]$SoilMoi10_40cm_inst
  SoilMoi40_100cm_list[[i]] <- surface_var_list[[i]]$SoilMoi40_100cm_inst
  SoilMoi100_200cm_list[[i]] <- surface_var_list[[i]]$SoilMoi100_200cm_inst
}

#Function to calculate grid cell mean for a list of rasters
calculate_grid_cell_mean <- function(raster_list) {
  raster_stack <- stack(raster_list)
  mean_raster <- calc(raster_stack, mean)
  return(mean_raster)
}

#grid mean for each variable
CanopInt_mean <- calculate_grid_cell_mean(CanopInt_list)
SWE_mean <- calculate_grid_cell_mean(SWE_list)
Rootmoist_mean <- calculate_grid_cell_mean(Rootmoist_list)
SoilMoi0_10cm_mean <- calculate_grid_cell_mean(SoilMoi0_10cm_list)
SoilMoi10_40cm_mean <- calculate_grid_cell_mean(SoilMoi10_40cm_list)
SoilMoi40_100cm_mean <- calculate_grid_cell_mean(SoilMoi40_100cm_list)
SoilMoi100_200cm_mean <- calculate_grid_cell_mean(SoilMoi100_200cm_list)

#Function to calculate the difference between a raster and the grid cell mean
calculate_difference <- function(raster_list, mean_raster) {
  diff_list <- list()
  for (i in seq_along(raster_list)) {
    diff_raster <- raster_list[[i]] - mean_raster
    diff_list[[i]] <- diff_raster
  }
  return(diff_list)
}

#Calculating the grid difference from grid mean
CanopInt_diff <- calculate_difference(CanopInt_list, CanopInt_mean)
SWE_diff <- calculate_difference(SWE_list, SWE_mean)
Rootmoist_diff <- calculate_difference(Rootmoist_list, Rootmoist_mean)
SoilMoi0_10cm_diff <- calculate_difference(SoilMoi0_10cm_list, SoilMoi0_10cm_mean)
SoilMoi10_40cm_diff <- calculate_difference(SoilMoi10_40cm_list, SoilMoi10_40cm_mean)
SoilMoi40_100cm_diff <- calculate_difference(SoilMoi40_100cm_list, SoilMoi40_100cm_mean)
SoilMoi100_200cm_diff <- calculate_difference(SoilMoi100_200cm_list, SoilMoi100_200cm_mean)

#sum the corresponding rasters in lists
sum_corresponding_rasters <- function(list1, list2, list3, list4, list5, list6, list7) {
  sum_list <- list()
  for (i in 1:length(list1)) {
    sum_raster <- list1[[i]] + list2[[i]] + list3[[i]] + list4[[i]] + list5[[i]] + list6[[i]] + list7[[i]]
    sum_list[[i]] <- sum_raster
  }
  return(sum_list)
}


combined_diff_list <- sum_corresponding_rasters(CanopInt_diff, SWE_diff, Rootmoist_diff, SoilMoi0_10cm_diff, SoilMoi10_40cm_diff, SoilMoi40_100cm_diff, SoilMoi100_200cm_diff)

