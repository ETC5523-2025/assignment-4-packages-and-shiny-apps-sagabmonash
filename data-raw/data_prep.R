# 1. Load Required Packages
library(neonUtilities)
library(dplyr)
library(tidyr)
library(lubridate)
library(padr)

# 2. Define Sites and Download Cache Data
neon_sites <- c("ARIK", "LEWI", "CARI")

# Define a directory to cache the raw downloads
user_base_dir <- Sys.getenv("USERPROFILE")
save_dir <- file.path(user_base_dir, "Documents", "waq_nitrate_analysis_cache")
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Download/Load Water Quality (waq)
if (file.exists(file.path(save_dir, "waq.rda"))) {
  waq <- readRDS(file.path(save_dir, "waq.rda"))
} else {
  print("Downloading water quality data (DP1.20288.001)...")
  waq <- neonUtilities::loadByProduct(
    dpID = "DP1.20288.001",
    site = neon_sites,
    startdate = "2018-01",
    enddate = "2019-12",
    package = "expanded",
    check.size = FALSE
  )
  saveRDS(waq, file.path(save_dir, "waq.rda"))
}

# Download/Load Nitrate (nsw)
if (file.exists(file.path(save_dir, "nsw.rda"))) {
  nsw <- readRDS(file.path(save_dir, "nsw.rda"))
} else {
  print("Downloading nitrate data (DP1.20033.001)...")
  nsw <-  neonUtilities::loadByProduct(
    dpID = "DP1.20033.001",
    site = neon_sites,
    startdate = "2018-01",
    enddate = "2019-12",
    package = "expanded",
    check.size = FALSE
  )
  saveRDS(nsw, file.path(save_dir, "nsw.rda"))
}

# Download/Load Temperature (temp_all)
if (file.exists(file.path(save_dir, "temp_all.rda"))) {
  temp_all <- readRDS(file.path(save_dir, "temp_all.rda"))
} else {
  print("Downloading temperature data (DP1.20053.001)...")
  temp_all <-  neonUtilities::loadByProduct(
    dpID = "DP1.20053.001",
    site = neon_sites,
    startdate = "2018-01",
    enddate = "2019-12",
    package = "expanded",
    check.size = FALSE
  )
  saveRDS(temp_all, file.path(save_dir, "temp_all.rda"))
}

# Download/Load Elevation (swe_all)
if (file.exists(file.path(save_dir, "swe_all.rda"))) {
  swe_all <- readRDS(file.path(save_dir, "swe_all.rda"))
} else {
  print("Downloading elevation data (DP1.20016.001)...")
  swe_all <-  neonUtilities::loadByProduct(
    dpID = "DP1.20016.001",
    site = neon_sites,
    startdate = "2018-01",
    enddate = "2019-12",
    package = "expanded",
    check.size = FALSE
  )
  saveRDS(swe_all, file.path(save_dir, "swe_all.rda"))
}

print("All raw data downloaded and cached.")

# 3. Process and Combine Data
# List to store the clean data for each site
lsModlData <- base::list()

# Site-specific sensor locations (from the paper)
dataSetup <- base::data.frame(
  site = c("ARIK", "CARI", "LEWI"),
  elevHor = c("101", "102", "102"), # Use 101 at ARIK
  tempHor = c("101", "102", "102"), # Use 101 at ARIK
  defaultHor = "102"
)

for (site in neon_sites) {
  print(paste("Processing data for:", site))
  
  # Get sensor location for this site
  site_setup <- filter(dataSetup, site == !!site)
  
  # Extract Water Quality (1-min)
  water_quality_site <- waq$waq_instantaneous %>%
    base::subset(siteID == site) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(horizontalPosition == site_setup$defaultHor) %>%
    dplyr::select(
      startDateTime,
      specificConductance, specificCondFinalQF,
      dissolvedOxygen, dissolvedOxygenFinalQF,
      turbidity, turbidityFinalQF
    ) %>%
    dplyr::mutate(startDateTime = lubridate::ymd_hms(startDateTime) - lubridate::second(startDateTime))
  
  # Extract Nitrate (15-min)
  nitrate_site <- nsw$NSW_15_minute %>%
    base::subset(siteID == site) %>%
    as_tibble() %>%
    mutate(startDateTime = lubridate::ymd_hms(startDateTime) - second(startDateTime)) %>%
    select(startDateTime, surfWaterNitrateMean, finalQF) %>%
    rename(nitrate_mean = surfWaterNitrateMean, label_nitrate_mean = finalQF)
  
  # Extract Temperature (5-min)
  temp_site <- temp_all$TSW_5min %>%
    base::subset(siteID == site) %>%
    as_tibble() %>%
    filter(horizontalPosition == site_setup$tempHor) %>%
    mutate(startDateTime = ymd_hms(startDateTime) - second(startDateTime)) %>%
    select(startDateTime, surfWaterTempMean, finalQF) %>%
    rename(temp_mean = surfWaterTempMean, label_temp_mean = finalQF)
  
  # Extract Elevation (5-min)
  elev_site <- swe_all$EOS_5_min %>%
    base::subset(siteID == site) %>%
    as_tibble() %>%
    filter(horizontalPosition == site_setup$elevHor) %>%
    mutate(startDateTime = ymd_hms(startDateTime) - second(startDateTime)) %>%
    select(startDateTime, surfacewaterElevMean, sWatElevFinalQF) %>%
    rename(elev = surfacewaterElevMean, label_elev_mean = sWatElevFinalQF)
  
  # Aggregate 1-min and 5-min data to 15-min
  
  # Helper function to aggregate
  aggregate_to_15min <- function(df, value_col, qf_col) {
    df %>%
      # Use the quality flag to set bad data to NA
      mutate(value = if_else({{ qf_col }} == 1, NA_real_, {{ value_col }})) %>%
      mutate(time_15min = floor_date(startDateTime, "15 minutes")) %>%
      group_by(time_15min) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  }
  
  wq_15min <- water_quality_site %>%
    mutate(
      specificConductance = if_else(specificCondFinalQF == 1, NA_real_, specificConductance),
      dissolvedOxygen = if_else(dissolvedOxygenFinalQF == 1, NA_real_, dissolvedOxygen),
      turbidity = if_else(turbidityFinalQF == 1, NA_real_, turbidity)
    ) %>%
    mutate(time_15min = floor_date(startDateTime, "15 minutes")) %>%
    group_by(time_15min) %>%
    summarise(
      spec_cond = mean(specificConductance, na.rm = TRUE),
      oxygen = mean(dissolvedOxygen, na.rm = TRUE),
      turbidity = mean(turbidity, na.rm = TRUE),
      .groups = "drop"
    )
  
  temp_15min <- aggregate_to_15min(temp_site, temp_mean, label_temp_mean) %>%
    rename(temp_mean = value, startDateTime = time_15min)
  
  elev_15min <- aggregate_to_15min(elev_site, elev, label_elev_mean) %>%
    rename(elev = value, startDateTime = time_15min)
  
  # Join all 15-min datasets
  
  # Start with nitrate (our base)
  data_site <- nitrate_site %>%
    filter(label_nitrate_mean == 0) %>% # Keep only good nitrate data
    select(startDateTime, nitrate_mean) %>%
    
    left_join(wq_15min, by = c("startDateTime" = "time_15min")) %>%
    left_join(temp_15min, by = "startDateTime") %>%
    left_join(elev_15min, by = "startDateTime") %>%
    distinct(startDateTime, .keep_all = TRUE)
  
  # Final cleaning (from paper)
  data_site$turbidity <- if_else(data_site$turbidity < 0, NA_real_, data_site$turbidity)
  data_site$nitrate_mean <- if_else(data_site$nitrate_mean <= 0, NA_real_, data_site$nitrate_mean)
  
  if (site == "CARI") {
    data_site$turbidity[data_site$turbidity > 500] <- NA_real_
    data_site$nitrate_mean <- if_else(data_site$nitrate_mean > 50, NA_real_, data_site$nitrate_mean)
  } else if (site == "ARIK") {
    data_site$spec_cond <- if_else(data_site$spec_cond < 100, NA_real_, data_site$spec_cond)
    data_site$nitrate_mean <- if_else(data_site$nitrate_mean > 25, NA_real_, data_site$nitrate_mean)
  } else if (site == "LEWI") {
    data_site$nitrate_mean <- if_else(data_site$nitrate_mean <= 5, NA_real_, data_site$nitrate_mean)
    data_site$temp_mean <- if_else(data_site$temp_mean < 0, NA_real_, data_site$temp_mean)
  }
  
  # Add site ID and store in the list
  data_site$siteID <- site
  lsModlData[[site]] <- data_site
}

# 4. Combine and Save File Data
# Combine the data from all 3 sites into one big data frame
neon_water_quality <- data.table::rbindlist(lsModlData) %>%
  # Add the log_turbidity and siteName columns for convenience
  mutate(
    log_turbidity = log(turbidity + 1),
    siteName = case_when(
      siteID == "ARIK" ~ "Arikaree River",
      siteID == "CARI" ~ "Caribou Creek",
      siteID == "LEWI" ~ "Lewis Run",
      TRUE ~ siteID
    )
  ) %>%
  # Rename columns to be more user-friendly
  rename(
    datetime = startDateTime,
    nitrate = nitrate_mean,
    temperature = temp_mean,
    specific_conductance = spec_cond,
    dissolved_oxygen = oxygen,
    elevation = elev
  ) %>%
  # Re-order columns
  select(
    siteID, siteName, datetime,
    nitrate, temperature, dissolved_oxygen,
    specific_conductance, turbidity, log_turbidity,
    elevation
  ) %>%
  # Remove any rows with NA values
  na.omit()

print("Data processing complete. Saving to package...")

# 5. Save to Package
usethis::use_data(neon_water_quality, overwrite = TRUE)
print("Done. 'neon_water_quality.rda' is saved in the 'data/' folder.")