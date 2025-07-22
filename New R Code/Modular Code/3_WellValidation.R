
# STEP 3: WELL VALIDATION 
# This step takes the cleaned well log data (see STEP 2.5) and creates a validation workflow for 
# smoothing and clustering wells to plot alongside the GRACE-derived GWS

library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(sf)
library(ggplot2)
library(geosphere)
library(cluster)

# Reference  tibble
sy_aq <- tribble(
  ~AquiferType,                                 ~SY_frac,
  "Unconsolidated sand and gravel aquifers",     0.20,
  "Semiconsolidated sand aquifers",              0.15,
  "Sandstone aquifers",                          0.08,
  "Sandstone and carbonate-rock aquifers",       0.075,
  "Carbonate-rock aquifers",                     0.03,
  "Igneous and metamorphic-rock aquifers",       0.005,
)


# Load & preprocess well log data table
well_csv <- "C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/ozark_plateaus_clean_yearmonth.csv"
wells_raw <- read.csv(well_csv)

wells <- wells_raw %>%
  filter(WellPurpose == "Dedicated Monitoring/Observation") %>%
  mutate(ObsDate = ymd(Time)) %>%
  group_by(SiteNo, AquiferType) %>%
  filter(n() >= 60,
         difftime(max(ObsDate), min(ObsDate), units = "days") >= 5*365) %>%
  ungroup() %>%
  mutate(YearMonth = floor_date(ObsDate, "month"))

wells_single <- wells %>%
  group_by(SiteNo, AquiferType, YearMonth) %>%
  summarise(head_ft = mean(WaterLevel_NAVD88_ft, na.rm=TRUE), .groups="drop")

# SY value for a given aquifer type (MS embayment is "Unconsolidated Sand and Gravel")
SY_global <- 0.20

wells <- wells %>%
  group_by(SiteNo, YearMonth, Latitude, Longitude, AquiferType) %>%
  filter(AquiferType == "UNCONFINED") %>%
  summarise(head_ft = mean(WaterLevel_NAVD88_ft, na.rm=TRUE), .groups="drop") %>%
  group_by(SiteNo) %>%
  mutate(head_anom_ft = head_ft - mean(head_ft, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    head_anom_cm = head_anom_ft * 0.3048 * 100,
    storage_cm = head_anom_cm * SY_global
  )

# Extract GWS anomalies into wells table
gws_lookup <- gws_anomalies
names(gws_lookup) <- names(gws_anomalies)
wells$YearMonth_chr <- format(wells$YearMonth, "%Y-%m")
wells$gws_anom <- mapply(function(lon, lat, ym) {
  r <- gws_lookup[[ym]]
  if (is.null(r)) return(NA)
  raster::extract(r, matrix(c(lon, lat), ncol=2))
}, wells$Longitude, wells$Latitude, wells$YearMonth_chr)

well_stats_raw <- wells %>%
  group_by(SiteNo, AquiferType) %>%
  summarise(
    n    = sum(!is.na(storage_cm) & !is.na(gws_anom)),
    r    = cor(storage_cm, gws_anom, use="pairwise.complete.obs"),
    rmse = sqrt(mean((storage_cm - gws_anom)^2, na.rm=TRUE)),
    .groups="drop"
  )

# Build smoothed monthly series & filter out bunk wells (exceptionally high error)
all_months <- seq.Date(min(wells_single$YearMonth), max(wells_single$YearMonth), by="month")

wells_monthly <- wells_single %>%
  group_by(SiteNo, AquiferType) %>%
  complete(YearMonth = all_months) %>%
  arrange(SiteNo, YearMonth) %>%
  mutate(head_ft = na.approx(head_ft, x=YearMonth, na.rm=FALSE)) %>%
  ungroup() %>%
  filter(!is.na(head_ft)) %>%
  group_by(SiteNo, AquiferType) %>%
  mutate(
    head_anom_ft  = head_ft - mean(head_ft, na.rm=TRUE),
    head_anom_cm  = head_anom_ft * 0.3048 * 100,
    storage_cm    = head_anom_cm * SY_global,
    storage_ma2   = rollapply(storage_cm, 2, mean, fill=NA, align="right")
  ) %>%
  ungroup() %>%
  left_join(
    wells %>% dplyr::select(SiteNo, Latitude, Longitude) %>% distinct(),
    by = "SiteNo"
  ) %>%
  mutate(
    YearMonth_chr = format(YearMonth, "%Y-%m"),
    gws_anom = mapply(function(x, y, ym) {
      r <- gws_lookup[[ym]]
      if (is.null(r)) return(NA)
      vals <- raster::extract(r, cbind(x,y), buffer=0.125*111000)[[1]]
      mean(vals, na.rm=TRUE)
    }, Longitude, Latitude, YearMonth_chr),
    gws_ma2 = rollapply(gws_anom, 2, mean, fill=NA, align="right")
  ) %>%
  filter(YearMonth_chr %in% names(gws_lookup)) %>%
  filter(!is.na(storage_ma2) & !is.na(gws_ma2))

rmse_cutoff <- quantile(well_stats_raw$rmse, 0.9)
bad_sites <- well_stats_raw %>% filter(rmse > rmse_cutoff) %>% pull(SiteNo)
wells_monthly <- wells_monthly %>% filter(!SiteNo %in% bad_sites)

save(wells_monthly, file = "wells_monthly_clean.RData")
load("wells_monthly_clean.RData")

# K-means clustering
run_clustering <- function(wells_monthly,
                           α      = 0.10,   # temporal vs spatial weight
                           w_sil  = 0.5,    # silhouette weight (tightness)
                           w_fit  = 0.5,    # fit weight (correlation)
                           λ      = 0.2,    # cluster‐penalty weight
                           δ      = 0.1     # shortlist tolerance (top 90%)
) {
  # Setup
  n_sites <- wells_monthly %>% distinct(SiteNo) %>% nrow()
  k_range <- 3:floor(sqrt(n_sites))
  
  # Temporal distance
  ts_wide <- wells_monthly %>%
    dplyr::select(SiteNo, YearMonth, storage_ma2) %>%
    pivot_wider(names_from=SiteNo, values_from=storage_ma2) %>%
    arrange(YearMonth)
  D_t <- dist(t(scale(as.matrix(ts_wide[,-1]))))
  
  # Spatial distance
  coords   <- wells_monthly %>%
    distinct(SiteNo, Longitude, Latitude) %>%
    arrange(SiteNo)
  D_s_mat <- distm(coords[,c("Longitude","Latitude")], fun=distHaversine)/1000
  D_s     <- as.dist(D_s_mat)
  
  # Composite distance & clustering
  D_comb <- α * D_t + (1 - α) * (D_s / sd(D_s))
  hc     <- hclust(D_comb, method="ward.D2")
  
  # Score each k by silhouette & fit
  sil_scores <- sapply(k_range, function(k) {
    cl <- cutree(hc, k=k)
    mean(silhouette(cl, D_comb)[, "sil_width"])
  })
  fit_scores <- sapply(k_range, function(k) {
    cl     <- cutree(hc, k=k)
    lookup <- tibble(SiteNo=as.numeric(names(cl)), cluster=cl)
    ws     <- wells_monthly %>% left_join(lookup, by="SiteNo")
    r_per_cl <- ws %>%
      group_by(cluster, YearMonth) %>%
      summarise(ms = mean(storage_ma2, na.rm=TRUE),
                mg = mean(gws_ma2,     na.rm=TRUE),
                .groups="drop") %>%
      group_by(cluster) %>%
      summarise(r = cor(ms, mg, use="pairwise.complete.obs"), .groups="drop") %>%
      pull(r)
    sum(abs(r_per_cl))
  })
  
  # Normalize & penalize by k
  sil_n <- (sil_scores - min(sil_scores)) / diff(range(sil_scores))
  fit_n <- (fit_scores - min(fit_scores)) / diff(range(fit_scores))
  k_n   <- (k_range - min(k_range)) / diff(range(k_range))
  combo <- w_sil*sil_n + w_fit*fit_n - λ*k_n
  
  # Shortlist & pick smallest k from list
  best_val  <- max(combo)
  shortlist <- k_range[ combo >= best_val * (1 - δ) ]
  best_k    <- min(shortlist)
  message(sprintf("Chose k = %d  (combo=%.3f, tol=%.1f%%)",
                  best_k, best_val, δ*100))
  
  # Assign clusters
  clusters   <- cutree(hc, k=best_k)
  wells_clust <- wells_monthly %>%
    mutate(cluster = clusters[as.character(SiteNo)])
  
  list(
    wells_clustered = wells_clust,
    hc              = hc,
    best_k          = best_k,
    sil_scores      = sil_scores,
    fit_scores      = fit_scores,
    combo_scores    = combo,
    shortlist       = shortlist
  )
}


summarize_clusters <- function(wells_clust) {
  perf <- wells_clust %>%
    group_by(cluster, YearMonth) %>%
    summarise(
      ms = mean(storage_ma2, na.rm=TRUE),
      mg = mean(gws_ma2),
      .groups="drop"
    ) %>%
    group_by(cluster) %>%
    summarise(
      r    = cor(ms, mg, use="pairwise.complete.obs"),
      rmse = sqrt(mean((ms - mg)^2, na.rm=TRUE)),
      .groups="drop"
    )
  
  well_stats <- wells_clust %>%
    group_by(cluster, SiteNo) %>%
    summarise(
      r    = cor(storage_ma2, gws_ma2, use="pairwise.complete.obs"),
      rmse = sqrt(mean((storage_ma2 - gws_ma2)^2, na.rm=TRUE)),
      .groups="drop"
    )
  
  list(cluster_perf = perf, well_stats = well_stats)
}


# Run clustering & summary
clust_res <- run_clustering(wells_monthly)
perf_res  <- summarize_clusters(clust_res$wells_clustered)

print(perf_res$cluster_perf)
print(perf_res$well_stats)

clusters    <- clust_res$wells_clustered
well_stats  <- perf_res$well_stats     
cluster_stats <- perf_res$cluster_perf

# join them 
wells_clustered <- clusters %>%
  left_join(
    well_stats %>% 
      rename(R = r, MRSE = rmse),
    by = c("SiteNo","cluster")
  )


# Plot a time series
plot_time_series <- function(cluster_id, wells_clust) {
  df      <- wells_clust %>% filter(cluster == cluster_id)
  mean_ts <- df %>%
    group_by(YearMonth) %>%
    summarise(
      mean_storage = mean(storage_ma2, na.rm=TRUE),
      mean_gws     = mean(gws_ma2,    na.rm=TRUE),
      .groups      = "drop"
    )
  
  # Adding descriptive stats to chart
  stats <- cluster_stats %>% filter(cluster == cluster_id)
  subtitle_text <- sprintf("R = %.2f   |   RMSE = %.1f cm", stats$r, stats$rmse)
  
  ggplot() +
    geom_line(
      data = df,
      aes(x = YearMonth, y = storage_ma2, group = SiteNo),
      color = "grey80", alpha = 0.6
    ) +
    geom_line(
      data = mean_ts,
      aes(x = YearMonth, y = mean_storage),
      color = "red", size = 1.2
    ) +
    geom_line(
      data = mean_ts,
      aes(x = YearMonth, y = mean_gws),
      color = "blue", size = 1.2
    ) +
    scale_y_continuous(
      name = "Mean well‐log anomaly (cm)",
      sec.axis = sec_axis(~ ., name = "GWS anomaly (cm)")
    ) +
    labs(
      title    = sprintf("Cluster %d: Well Log vs GRACE GWS", cluster_id),
      subtitle = subtitle_text,
      x        = "Year–Month"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.title.y.left  = element_text(color = "red"),
      axis.title.y.right = element_text(color = "blue"),
      plot.subtitle      = element_text(face = "italic")
    )
}

# Plot:
plot_time_series(3, clust_res$wells_clustered)



plot_clusters_map <- function(wells_clust,
                              state_boundaries,
                              which_clusters = NULL,
                              point_size     = 2,
                              alpha          = 0.8) {
  
  # turn to sf
  wells_sf <- wells_clust %>%
    { if (!is.null(which_clusters)) filter(., cluster %in% which_clusters) else . } %>%
    distinct(SiteNo, cluster, Latitude, Longitude) %>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
  
  ggplot() +
    geom_sf(data = state_boundaries, fill = NA, color = "black", lwd = 0.5) +
    geom_sf(data = wells_sf,
            aes(color = factor(cluster)),
            size = point_size,
            alpha = alpha) +
    scale_color_brewer("Cluster", palette = "Set2") +
    labs(
      title = if (is.null(which_clusters)) {
        "All Well Clusters"
      } else {
        paste("Well Clusters", paste(which_clusters, collapse = ", "))
      }
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      plot.title      = element_text(face = "bold")
    )
}

# Plot all cluster locations:
plot_clusters_map(clust_res$wells_clustered, state_boundaries)

# plot one cluster (or group of clusters)):
plot_clusters_map(clust_res$wells_clustered, state_boundaries, which_clusters = 4)

