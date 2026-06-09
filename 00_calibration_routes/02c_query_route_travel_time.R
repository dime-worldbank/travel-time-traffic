# Query live travel times using the Google Routes API (Compute Routes)
# Docs: https://developers.google.com/maps/documentation/routes/compute_route_directions

library(tidyverse)
library(httr2)
source("~/Documents/github/travel-time-traffic/_main.R")

GOOGLE_API_KEY <- read_csv("~/Dropbox/World Bank/Webscraping/Files for Server/api_keys.csv") %>%
  dplyr::filter(Account == "robmarty3@gmail.com",
                Service == "Google Routes API") %>%
  pull(Key)

# 1. Load OD pairs from 02b ----------------------------------------------------

routes_df <- readRDS(file.path(data_dir, "Travel Time Routes 2026", "od_pairs.Rds"))

# 2. Function: query Google Routes API for a single OD pair --------------------

query_google_route <- function(lat_orig, lon_orig, lat_dest, lon_dest, api_key) {

  body <- list(
    origin = list(
      location = list(latLng = list(latitude = lat_orig, longitude = lon_orig))
    ),
    destination = list(
      location = list(latLng = list(latitude = lat_dest, longitude = lon_dest))
    ),
    travelMode            = "DRIVE",
    routingPreference     = "TRAFFIC_AWARE_OPTIMAL",  # uses live traffic
    computeAlternativeRoutes = FALSE,
    routeModifiers        = list(avoidTolls = TRUE)
  )

  resp <- tryCatch({
    request("https://routes.googleapis.com/directions/v2:computeRoutes") %>%
      req_url_query(key = api_key) %>%
      req_headers(
        "Content-Type"    = "application/json",
        # Field mask controls what the API returns (and what it bills for)
        "X-Goog-FieldMask" = paste(
          "routes.duration",                  # travel time with live traffic
          "routes.staticDuration",            # travel time without traffic
          "routes.distanceMeters",
          "routes.polyline.encodedPolyline",  # route geometry
          sep = ","
        )
      ) %>%
      req_body_json(body) %>%
      req_error(is_error = \(resp) FALSE) %>%  # handle HTTP errors manually
      req_perform()
  }, error = function(e) {
    message("Request failed: ", e$message)
    return(NULL)
  })

  na_row <- tibble(duration_in_traffic_s = NA_real_,
                   duration_typical_s    = NA_real_,
                   distance_m            = NA_real_,
                   encoded_polyline      = NA_character_,
                   api_error             = NA_character_)

  if (is.null(resp)) {
    return(na_row %>% mutate(api_error = "request_failed"))
  }

  result <- resp %>% resp_body_json()

  if (!is.null(result$error)) {
    message("API error: ", result$error$message)
    return(na_row %>% mutate(api_error = result$error$message))
  }

  if (length(result$routes) == 0) {
    return(na_row %>% mutate(api_error = "no_routes_returned"))
  }

  route <- result$routes[[1]]
  
  Sys.sleep(0.25)

  # Durations are returned as strings like "1234s"; strip the trailing "s"
  tibble(
    query_time_utc        = Sys.time() %>% lubridate::with_tz("UTC"),
    duration_in_traffic_s = as.numeric(sub("s$", "", route$duration)),
    duration_typical_s    = as.numeric(sub("s$", "", route$staticDuration)),
    distance_m            = as.numeric(route$distanceMeters),
    encoded_polyline      = route$polyline$encodedPolyline,
    api_error             = NA_character_
  )

}

# 3. Apply function across all OD pairs ----------------------------------------

query_datetime_eat <- lubridate::with_tz(Sys.time(), "Africa/Nairobi")

routes_results <- routes_df %>%
  mutate(
    result = pmap(
      list(origin_lat, origin_lon, dest_lat, dest_lon),
      \(a, b, c, d) query_google_route(a, b, c, d, api_key = GOOGLE_API_KEY)
    )
  ) %>%
  unnest(result) %>%
  mutate(
    query_datetime_eat          = query_datetime_eat,
    duration_in_traffic_min = duration_in_traffic_s / 60,
    duration_typical_min    = duration_typical_s    / 60,
    distance_km             = distance_m            / 1000,
    speed_in_traffic_kmh    = distance_km / (duration_in_traffic_min / 60)
  )

# 4. Export --------------------------------------------------------------------

out_name <- paste0("tt_", format(query_datetime_eat, "%Y-%m-%d_%H-%M-%S"), ".Rds")
out_dir  <- file.path(data_dir, "Travel Time Routes 2026", "Travel Time Data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(routes_results, file.path(out_dir, out_name))

# 5. Preview results -----------------------------------------------------------

routes_results %>%
  select(name, fclass, toward_cbd,
         distance_km,
         duration_typical_min,
         duration_in_traffic_min,
         speed_in_traffic_kmh,
         api_error) %>%
  print(n = Inf)
