# STEP 1: GRACE PROCESSING & MODEL TRAINING
# To be run first.  This chunk of code will process the raw GRACE and GLDAS files, assembling them
# into training data for the random forest model.       

library(ncdf4)
library(raster)
library(randomForest)
library(sf)
library(dplyr)
library(ggplot2)
library(caret)
library(stars) 
library(viridis) 
library(patchwork) 
library(ggrepel)   

# Loading US shape, declaring contiguous US extent (no Alaska, Hawaii, P.R.)
shapefile <- st_read("D:/Research/MASTER_DATA_2.0/Shapefile/US_shape_WGS84.shp")
conus_extent <- extent(-125, -66, 24, 50)

# Process GRACE files & convert units from meters to centimeters
process_grace <- function(files) {
  lapply(files, function(f) {
    rst_m   <- rotate(raster(f, varname="lwe_thickness"))  # m
    rst_cm  <- rst_m * 100                                  # cm
    rst_clip <- mask(crop(rst_cm, shapefile), shapefile)
    crop(rst_clip, conus_extent)
  })
}

grace_base <- "D:/Research/MASTER_DATA_2.0/GRACE_ALL"
years <- 2002:2023
grace_files <- unlist(lapply(years, function(y) {
  list.files(
    file.path(grace_base, as.character(y)),
    pattern    = "\\.nc$",
    full.names = TRUE
  )
}))

# Date extraction 
extract_grace_date <- function(nc_file) {
  message("Processing file: ", nc_file)
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc))
  
  global_attrs <- ncatt_get(nc, 0)
  date_attrs <- c("time_coverage_start", "time_coverage_end", "date_created", "date_issued")
  for (attr in date_attrs) {
    if (attr %in% names(global_attrs)) {
      date_str <- global_attrs[[attr]]
      date <- tryCatch(as.Date(date_str, format = "%Y-%m-%d"), error = function(e) NULL)
      if (is.null(date)) date <- tryCatch(as.Date(date_str, format = "%Y%m%d"), error = function(e) NULL)
      if (is.null(date)) date <- tryCatch(as.Date(date_str, format = "%Y%m"), error = function(e) NULL)
      if (!is.null(date)) return(format(date, "%Y-%m"))
    }
  }
  
  if ("time_bounds" %in% names(nc$var)) {
    time_bounds <- ncvar_get(nc, "time_bounds")
    time_units <- ncatt_get(nc, "time_bounds", "units")$value
    if (grepl("days since", time_units)) {
      ref_date <- as.Date(sub("days since ", "", time_units))
      date <- ref_date + time_bounds[1]
      return(format(date, "%Y-%m"))
    }
  }
  
  basename <- basename(nc_file)
  if (grepl("\\d{4}_\\d{2}", basename)) {
    date_str <- regmatches(basename, regexpr("\\d{4}_\\d{2}", basename))[[1]]
    date <- tryCatch(as.Date(paste0(date_str, "_01"), format = "%Y_%m_%d"), error = function(e) NULL)
    if (!is.null(date)) return(format(date, "%Y-%m"))
  }
  
  warning("Could not extract date from ", nc_file)
  return(NA)
}

year_months <- sapply(grace_files, extract_grace_date)
grace_clipped_list <- process_grace(grace_files)


# GLDAS PROCESSING
# These are conversion variables
DAYS_PER_MONTH <- 30.44
SECONDS_PER_MONTH <- DAYS_PER_MONTH * 86400

# Separating storage from flux variables
storage_vars <- c("SWE_inst", "CanopInt_inst", "RootMoist_inst", 
                  "SoilMoi0_10cm_inst", "SoilMoi10_40cm_inst", 
                  "SoilMoi40_100cm_inst", "SoilMoi100_200cm_inst", "Qs_acc")
flux_vars <- c("Evap_tavg", "Rainf_f_tavg")
var_names <- c(flux_vars, storage_vars)

process_gldas_files <- function(file_list, res_degree) {
  lapply(file_list, function(file_name) {
    stk <- stack(lapply(var_names, function(v) {
      r <- raster(file_name, varname = v)
      r_cropped <- crop(r, conus_extent)
      if (exists("shapefile")) mask(r_cropped, shapefile) else r_cropped
    }))
    names(stk) <- var_names
    
    # getting storage in cm, given 1 kg/m2 ≡ 1 mm water
    for (var in storage_vars) {
      stk[[paste0(var, "_cm")]] <- stk[[var]] * 0.1
    }
    
    # flux vars in cm/month 
    for (var in flux_vars) {
      stk[[paste0(var, "_cm_month")]] <-
        stk[[var]] *
        SECONDS_PER_MONTH *  # mm per month
        0.1                  # cm per month
    }
    
    new_storage <- paste0(storage_vars, "_cm")
    new_flux    <- paste0(flux_vars, "_cm_month")
    
    stk[[ c(new_storage, new_flux) ]]
  })
}

# Directories for processed GLDAS
gldas1_base   <- "D:/Research/MASTER_DATA_2.0/GLDAS01"
gldas025_base <- "D:/Research/MASTER_DATA_2.0/GLDAS025"

gldas1_files <- unlist(lapply(years, function(y) {
  list.files(
    file.path(gldas1_base, as.character(y)),
    pattern    = "\\.nc4$",
    full.names = TRUE
  )
}))
gldas025_files <- unlist(lapply(years, function(y) {
  list.files(
    file.path(gldas025_base, as.character(y)),
    pattern    = "\\.nc4$",
    full.names = TRUE
  )
}))


gldas1_stacked <- process_gldas_files(gldas1_files, 1.0)
gldas025_stacked <- process_gldas_files(gldas025_files, 0.25)
output_raster_dir <- "D:/Research/MASTER_DATA_2.0/GLDAS_OUTPUTS"
output_dir_025 <- file.path(output_raster_dir, "025deg")
dir.create(output_dir_025, recursive = TRUE, showWarnings = FALSE)

lapply(seq_along(gldas025_stacked), function(i) {
  filepath <- file.path(output_dir_025, sprintf("gldas025_%03d.grd", i))
  writeRaster(gldas025_stacked[[i]],
              filename = filepath,
              format = "raster",
              overwrite = TRUE)
})

output_dir_1 <- file.path(output_raster_dir, "1deg")
dir.create(output_dir_1, recursive = TRUE, showWarnings = FALSE)

lapply(seq_along(gldas1_stacked), function(i) {
  filepath <- file.path(output_dir_1, sprintf("gldas1deg_%03d.grd", i))
  writeRaster(gldas1_stacked[[i]],
              filename = filepath,
              format = "raster",
              overwrite = TRUE)
})


# If files have already been processed and saved
gldas025_stacked <- lapply(
  list.files("D:/Research/MASTER_DATA_2.0/GLDAS_OUTPUTS/025deg", pattern = "\\.grd$", full.names = TRUE),
  brick
)


gldas1_stacked <- lapply(
  list.files("D:/Research/MASTER_DATA_2.0/GLDAS_OUTPUTS/1deg", pattern = "\\.grd$", full.names = TRUE),
  brick
)


# COMPUTE ANOMALIES
output_dir <- "D:/Research/MASTER_DATA_2.0/"

#Calculating storage means over full CONUS
compute_storage_means <- function(gldas_list) {
  cm_vars       <- paste0(storage_vars, "_cm")
  storage_means <- setNames(vector("list", length(cm_vars)), cm_vars)
  
  for (var in cm_vars) {
    rasters_list <- lapply(gldas_list, function(x) {
      if (var %in% names(x)) {
        r <- crop(x[[var]], conus_extent)
        readAll(r)
      } else NULL
    })
    rasters_list <- Filter(Negate(is.null), rasters_list)
    storage_means[[var]] <- calc(stack(rasters_list), mean, na.rm = TRUE) %>% readAll()
  }
  
  storage_means
}

#Flux means over full CONUS
compute_flux_means <- function(gldas_list) {
  cm_vars    <- paste0(flux_vars, "_cm_month")
  flux_means <- setNames(vector("list", length(cm_vars)), cm_vars)
  
  for (var in cm_vars) {
    rasters_list <- lapply(gldas_list, function(x) {
      if (var %in% names(x)) {
        r <- crop(x[[var]], conus_extent)
        readAll(r)
      } else NULL
    })
    rasters_list <- Filter(Negate(is.null), rasters_list)
    flux_means[[var]] <- calc(stack(rasters_list), mean, na.rm = TRUE) %>% readAll()
  }
  
  flux_means
}

# Compute means for 0.25 degree data 
storage_means_025 <- compute_storage_means(gldas025_stacked)
flux_means_025    <- compute_flux_means(gldas025_stacked)

# Compute means for 1.0 degree data 
storage_means_1 <- compute_storage_means(gldas1_stacked)
flux_means_1    <- compute_flux_means(gldas1_stacked)

# Building full GLDAS anomaly stacks
compute_anomalies <- function(gldas_list, means_list) {
  vars <- names(means_list)
  lapply(seq_along(gldas_list), function(i) {
    month_stack <- gldas_list[[i]]
    anom_layers <- lapply(vars, function(var) {
      month_stack[[var]] - means_list[[var]]
    })
    stack(anom_layers, names = vars)
  })
}

storage_anoms <- compute_anomalies(gldas025_stacked, storage_means_025)
storage_anoms_model <- compute_anomalies(gldas1_stacked, storage_means_1)
flux_anoms <- compute_anomalies(gldas025_stacked, flux_means_025)
flux_anoms_model <- compute_anomalies(gldas1_stacked, flux_means_1)

gldas_anoms_stacked <- Map(function(s, f) {
  stack(s, f)
}, storage_anoms_model, flux_anoms_model)

# Assigning each stack its GLDAS variable names
gldas_anoms_stacked <- lapply(gldas_anoms_stacked, function(stk) {
  nms <- c(paste0(storage_vars, "_cm"),
           paste0(flux_vars,    "_cm_month"))
  names(stk) <- nms
  stk
})

gldas025_anoms_stacked <- Map(function(s, f) {
  stack(s, f)
}, storage_anoms, flux_anoms)

gldas025_anoms_stacked <- lapply(gldas025_anoms_stacked, function(stk) {
  nms <- c(paste0(storage_vars, "_cm"),
           paste0(flux_vars,    "_cm_month"))
  names(stk) <- nms
  stk
})

# MODEL PREPARATION

# Prepare model data with year and month
prepare_model_data <- function(grace_list, gldas_list, year_months) {
  stopifnot(length(grace_list) == length(gldas_list),
            length(grace_list) == length(year_months))
  
  monthly_data <- vector("list", length(grace_list))
  for (i in seq_along(grace_list)) {
    # grab matching coords
    grace_coords  <- as.data.frame(grace_list[[i]], xy=TRUE)[,1:2]
    gldas_coords  <- as.data.frame(gldas_list[[i]], xy=TRUE)[,1:2]
    common_coords <- merge(grace_coords, gldas_coords, by = c("x","y"))
    if (nrow(common_coords)==0) next
    
    # extract tws values
    grace_values <- raster::extract(grace_list[[i]], common_coords[,1:2])
    gldas_values <- as.data.frame(raster::extract(gldas_list[[i]], common_coords[,1:2]))
    df <- cbind(gldas_values, lwe_thickness_cm = grace_values)
    df$month <- as.integer(substr(year_months[i], 6, 7))
    df$year <- as.integer(substr(year_months[i], 1, 4))
    
    # cyclical encoding for seasonality
    df$month_sin <- sin(2 * pi * df$month / 12)
    df$month_cos <- cos(2 * pi * df$month / 12)
    
    # dropping raw month column
    df$month <- NULL
    
    monthly_data[[i]] <- df
  }
  
  dplyr::bind_rows(monthly_data) %>% na.omit()
}

model_data <- prepare_model_data(
  grace_clipped_list,
  gldas_anoms_stacked,
  year_months
)


# RANDOM FOREST BASE MODEL
set.seed(123)
train_idx <- sample(nrow(model_data), 0.80 * nrow(model_data))
model <- randomForest(lwe_thickness_cm ~ ., data = model_data[train_idx, ], 
                      ntree = 700, importance = TRUE)

save(model, file = "random_forest_base_model.RData")
load("random_forest_base_model.RData") 

# Validation double-check
test  <- model_data[-train_idx, ]
pred <- predict(model, newdata = test)
rmse <- sqrt(mean((pred - test$lwe_thickness_cm)^2))
r2   <- cor(pred, test$lwe_thickness_cm)^2

cat(sprintf("Hold‐out RMSE = %.3f cm,  R² = %.3f\n", rmse, r2))