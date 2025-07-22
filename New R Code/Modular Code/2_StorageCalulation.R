# STEP 2: RESIDUAL CORRECTION & STORAGE CALCULATION
# This second chunk defines several functions which  use the GRACE, GLDAS, and modeled TWS data to 
# define, interpolate, and implement residuals for correcting high-resolution data TWS predictions.
# Relevant storage components are then subtracted from the high resolution TWS stack to calculate GWS

# Function to rebuild the coarse predictor stack (anomalies + time covariates)
make_coarse_stack <- function(lr, ym) {
  yr <- as.integer(substr(ym, 1, 4))
  mo <- as.integer(substr(ym, 6, 7))
  base <- lr[[1]] * 0 + 1
  
  lr2 <- stack(
    lr,
    year      = base * yr,
    month_sin = base * sin(2*pi*mo/12),
    month_cos = base * cos(2*pi*mo/12)
  )
  names(lr2) <- c(
    paste0(storage_vars, "_cm"),
    paste0(flux_vars,    "_cm_month"),
    "year", "month_sin", "month_cos"
  )
  lr2
}

# Predict & compute residuals at 1 degree
residuals_lr <- lapply(seq_along(gldas_anoms_stacked), function(i) {
  lr  <- gldas_anoms_stacked[[i]]
  gr  <- grace_clipped_list[[i]]
  ym  <- year_months[i]
  lr2 <- make_coarse_stack(lr, ym)
  
  pred_lr  <- raster::predict(lr2, model, na.rm=TRUE)  # 1° prediction
  resid_lr <- gr - pred_lr                             # 1° residual
  resid_lr
})

# Upsample using the first layer of gldas025_anoms_stacked as target grid
template_hr <- gldas025_anoms_stacked[[1]][[1]]

residuals_hr <- lapply(residuals_lr, function(r1) {
  resample(r1, template_hr, method="bilinear")
})

# Save and reload residuals for entire CONUS
save(residuals_hr, file = "residuals_025.RData", compress = "xz")
load("residuals_025.RData")

#Keep only valid months
keep <- which(!is.na(year_months))

#Subset all time‐indexed lists
year_months             <- year_months[keep]
grace_clipped_list      <- grace_clipped_list[keep]
gldas025_anoms_stacked  <- gldas025_anoms_stacked[keep]
gldas_anoms_stacked     <- gldas_anoms_stacked[keep]
residuals_hr            <- residuals_hr[keep]


#Define Extents & Spatial Objects for Aquifer

#Mississippi Embayment
aquifer_extent <- extent(-94.5, -87.25, 29.75, 37.25)
aquifer_sf <- st_as_sfc(st_bbox(c(xmin = -94.5, xmax = -87.25, ymin = 29.75, ymax = 37.25), crs = st_crs(4326)))
#Ozark Plateaus
# aquifer_extent <- extent(-95.3, -88.6, 34.8, 39.4)
# aquifer_sf<- st_as_sfc(st_bbox(c(xmin = -95.3, xmax = -88.6,ymin = 34.8, ymax = 39.4), crs = st_crs(4326)))


# Residual Correction & Clipping to desired extent
clip_predict_correct <- function(i,
                                 anom_stack_025,
                                 residuals_hr,
                                 year_months,
                                 clip_extent = NULL,
                                 clip_shape  = NULL   # sf POLYGON
) {
  # Convert sf to spatial if needed
  if (!is.null(clip_shape)) {
    clip_shape <- as(clip_shape, "Spatial")
  }
  
  # Clip anomalies + residual
  hr_anom <- anom_stack_025[[i]]
  resid   <- residuals_hr[[i]]
  
  if (!is.null(clip_extent)) {
    hr_anom <- crop(hr_anom, clip_extent)
    resid   <- crop(resid,   clip_extent)
  }
  if (!is.null(clip_shape)) {
    hr_anom <- mask(hr_anom, clip_shape)
    resid   <- mask(resid,   clip_shape)
  }
  
  ym   <- year_months[i]
  yr   <- as.integer(substr(ym, 1, 4))
  mo   <- as.integer(substr(ym, 6, 7))
  base <- hr_anom[[1]] * 0 + 1
  
  hr2 <- stack(
    hr_anom,
    base * yr,
    base * sin(2 * pi * mo / 12),
    base * cos(2 * pi * mo / 12)
  )
  names(hr2) <- c(
    paste0(storage_vars, "_cm"),
    paste0(flux_vars,    "_cm_month"),
    "year", "month_sin", "month_cos"
  )
  
  # predict and add residual
  pred_hr   <- raster::predict(hr2, model, na.rm = TRUE)
  corrected <- pred_hr + resid
  
  # mask to exact footprint
  if (!is.null(clip_shape)) {
    corrected <- mask(corrected, clip_shape)
  }
  
  corrected
}


#FLEXIBLE FOR ANY AQUIFER EXTENTS
aquifer_corrected <- lapply(
  seq_along(gldas025_anoms_stacked),
  function(i) clip_predict_correct(
    i,
    anom_stack_025 = gldas025_anoms_stacked,
    residuals_hr    = residuals_hr,
    year_months     = year_months,
    clip_extent     = aquifer_extent,
    clip_shape      = aquifer_sf
  )
)


corrected_rasters <- Filter(Negate(is.null), aquifer_corrected)
names(corrected_rasters) <- year_months[!vapply(aquifer_corrected, is.null, logical(1))]

saveRDS(corrected_rasters, "corrected_mississippi_embayment.rds")
corrected_rasters <- readRDS("corrected_mississippi_embayment.rds")

plot(corrected_rasters[[1]], main = names(corrected_rasters)[1])


#STEP 6 GWS ANOMALY CALCULATION
# Assign variable names to every element of storage_anoms
cm_names <- paste0(storage_vars, "_cm")
storage_anoms <- lapply(storage_anoms, function(rs) {
  names(rs) <- cm_names
  rs
})

calculate_gws_anomalies <- function(corrected_tws, storage_anoms) {
  mapply(function(tws, stor_stack) {
    # 1) Crop & align
    s_crop  <- crop(stor_stack,    extent(tws))
    s_align <- resample(s_crop,    tws, method = "bilinear")
    
    # 2) Define the layers to subtract by name
    want     <- c("SWE_inst_cm", "RootMoist_inst_cm")
    # extract those layers
    sel      <- s_align[[ want ]]  # this gives you a RasterStack of just those two
    
    # 3) Sum them
    surface_anom <- calc(sel, sum, na.rm=TRUE)
    
    # 4) Subtract from TWS anomaly
    tws - surface_anom
  },
  corrected_tws,
  storage_anoms,
  SIMPLIFY = FALSE)
}

gws_anomalies <- calculate_gws_anomalies(corrected_rasters, storage_anoms)
names(gws_anomalies) <- names(corrected_rasters)


# Prepare state boundaries for visualization
library(spData)
state_shape <- st_as_sf(us_states)
state_shape <- st_transform(state_shape, crs = 4326)
state_shape <- st_make_valid(state_shape)


# (re)crop state boundaries to that region:
state_boundaries <- st_crop(state_shape, aquifer_sf)
region_sf <- aquifer_sf

#STEP 7: Visualization, region‐flexible
plot_results <- function(month_index, region_sf) {
  date_label <- names(gws_anomalies_op)[month_index]
  gws_stars  <- st_as_stars(gws_anomalies_op[[month_index]])
  
  # dynamic bounding box from the region
  bb <- st_bbox(region_sf)
  
  ggplot() +
    geom_stars(data = gws_stars, aes(fill = layer)) +
    scale_fill_viridis(option = "plasma", direction = -1, name = "cm") +
    geom_sf(data = state_boundaries, fill = NA, color = "black", linewidth = 0.5) +
    coord_sf(
      xlim   = c(bb["xmin"], bb["xmax"]),
      ylim   = c(bb["ymin"], bb["ymax"]),
      crs    = st_crs(region_sf),
      expand = FALSE
    ) +
    labs(
      title    = sprintf("GWS Anomaly for %s", date_label),
      subtitle = "Ozark Plateaus"
    ) +
    theme_minimal()
}

# call it:
plot_results(220, region_sf)

# stats
layer_stats <- function(r) {
  qs <- quantile(r[], c(0.25, 0.5, 0.75), na.rm = TRUE) 
  c(
    Min    = cellStats(r, min,    na.rm = TRUE),
    Q1     = qs[1],
    Median = qs[2],
    Mean   = cellStats(r, mean,   na.rm = TRUE),
    Q3     = qs[3],
    SD     = cellStats(r, sd,     na.rm = TRUE),
    Max    = cellStats(r, max,    na.rm = TRUE)
  )
}

generate_stats <- function(month_index) {
  # pull the three layers
  tws     <- corrected_rasters[[month_index]]
  gws     <- gws_anomalies[[month_index]]
  storage <- storage_anoms[[month_index]]
  date    <- names(corrected_rasters)[month_index]
  
  # compute stats
  s_tws     <- layer_stats(tws)
  s_storage <- layer_stats(storage)
  s_gws     <- layer_stats(gws)
  
  # build a data.frame with 3 rows and all stats as columns
  df <- data.frame(
    Month  = rep(date, 3),
    Metric = c("TWS", "Storage_Anomaly", "GWS_Anomaly"),
    rbind(s_tws, s_storage, s_gws),
    row.names = NULL,
    check.names = FALSE
  )
  return(df)
}

all_stats <- do.call(rbind, lapply(seq_along(corrected_rasters), generate_stats))
head(all_stats)
