# STEP 2.5: WELL DATA CLEANING AND PREP
# This step relies on two csv files per Aquifer, acquired through the 
# National Groundwater Monitoring Network: WATERLEVEL & SITE_INFO.   

library(dplyr)
library(readr)
library(sf)
library(lubridate)


# Read in files
water_level <- read_csv("C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/WATERLEVEL.csv")
site_info <- read_csv("C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/SITE_INFO.csv")

# Select Site, Time, and Waterlevel data from WATERLEVEL
water_level_clean <- water_level %>%
  select(SiteNo, Time, 
         `Depth to Water Below Land Surface in ft.`, 
         `Water level in feet relative to NAVD88`)

# Select Site, Location, and Aquifer data from SITE_INFO
site_info_clean <- site_info %>%
  select(SiteNo, SiteName, DecLatVa, DecLongVa, 
         WellDepth, StateNm, AquiferType, WlWellPurposeDesc)

# Merge the cleaned datasets
final_well_data <- water_level_clean %>%
  left_join(site_info_clean, by = "SiteNo") %>%
  # Clean up column names
  rename(
    DepthToWater_ft = `Depth to Water Below Land Surface in ft.`,
    WaterLevel_NAVD88_ft = `Water level in feet relative to NAVD88`,
    Longitude = DecLongVa,
    Latitude = DecLatVa,
    WellPurpose = WlWellPurposeDesc
  ) %>%
  # Convert Time to datetime format
  mutate(Time = as.POSIXct(Time)) %>%
  # Arrange by SiteNo and Time
  arrange(SiteNo, Time)

glimpse(final_well_data)

# Save the cleaned data
write_csv(final_well_data, 
          "C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/ozark_plateaus_clean.csv")


# Prepare Well Date information
well_data <- final_well_data %>%  
  mutate(
    Time = as.Date(Time),
    YearMonth = format(Time, "%Y-%m")  # Create year-month for matching
  )

write_csv(well_data, 
          "C:/Users/gmwil/OneDrive/Desktop/Ph.D/GRACE 2.0/Well_log/Ozark_Plateaus/ozark_plateaus_clean_yearmonth.csv")

# Create spatial version
wells_sf <- well_data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  

# Per-aquifer summary stats
summary_stats <- final_well_data %>%
  group_by(SiteNo, SiteName, StateNm, AquiferType, WellPurpose) %>%
  summarise(
    N_Measurements = n(),
    First_Date = min(Time),
    Last_Date = max(Time),
    Mean_Depth_ft = mean(DepthToWater_ft, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)
