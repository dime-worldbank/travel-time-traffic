# Copy Files from smarTTrans Dropbox to Travel Time & Traffic Dropbox

# Paths ------------------------------------------------------------------------
smarttrans_data_dir <- "~/Dropbox/World Bank/IEs/CrashMap-Nairobi/Data"

# IN Files
nbo_mb_traffic_in <- file.path(smarttrans_data_dir, "Google Mapbox Traffic", "FinalData", "mapbox_daily_data")
nbo_gl_traffic_in <- file.path(smarttrans_data_dir, "Google Mapbox Traffic", "FinalData", "google_individual_rasters")

nbo_mb_tt_in <- file.path(smarttrans_data_dir, "Travel Time", "Mapbox Travel Time", "FinalData", "daily_data")
nbo_gl_tt_in <- file.path(smarttrans_data_dir, "Travel Time", "Google Travel Time", "FinalData", "daily_data")

# OUT Files
nbo_mb_traffic_out <- file.path(traffic_dir, "mapbox_daily_data_nairobi")
nbo_gl_traffic_out <- file.path(traffic_dir, "google_individual_rasters_nairobi")

nbo_mb_tt_out <- file.path(tt_dir, "mapbox_daily_data_nairobi")
nbo_gl_tt_out <- file.path(tt_dir, "google_daily_data_nairobi")

# Copy -------------------------------------------------------------------------
#### Traffic
for(file_i in list.files(nbo_mb_traffic_in, pattern = "*.Rds")){
  file.copy(from = file.path(nbo_mb_traffic_in, file_i),
            to = file.path(nbo_mb_traffic_out, file_i))
}

for(file_i in list.files(nbo_gl_traffic_in, pattern = "*.tiff")){
  file.copy(from = file.path(nbo_gl_traffic_in, file_i),
            to = file.path(nbo_gl_traffic_out, file_i))
}

#### Travel Time
for(file_i in list.files(nbo_mb_tt_in, pattern = "*.Rds")){
  file.copy(from = file.path(nbo_mb_tt_in, file_i),
            to = file.path(nbo_mb_tt_out, file_i))
}

for(file_i in list.files(nbo_gl_tt_in, pattern = "*.Rds")){
  file.copy(from = file.path(nbo_gl_tt_in, file_i),
            to = file.path(nbo_gl_tt_out, file_i))
}




