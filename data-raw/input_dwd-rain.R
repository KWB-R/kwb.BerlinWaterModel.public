install.packages("rdwd")
library(dplyr)
rdwd::updateRdwd()

# read Berlin area to derive middle point for proximity search of rain stations
landesgrenze <- read_wfs(dataset_id = "alkis_land")

bbox <- landesgrenze %>%
  sf::st_transform(4326) %>%
  sf::st_point_on_surface() %>%
  sf::st_bbox()

# get list of DWD rain stations in and around Berlin (26 km around center)
Berlin_stations <- rdwd::nearbyStations(
  lat = 52.48,
  lon = 13.38,
  res = "hourly",
  var = "precipitation",
  per = "historical",
  radius = 26
)

Berlin_rain_hourly_stations <- Berlin_stations %>%
  dplyr::filter(Stations_id == 433,
                bis_datum >= "2022-12-31",
                von_datum <= "2002-01-01",
                Bundesland == "Berlin") %>%
  sf::st_as_sf(coords = c("geoLaenge", "geoBreite"), crs = 4326) %>%
  dplyr::mutate(label = sprintf(
    "%d: %s (%s - %s)",
    Stations_id,
    Stationsname,
    von_datum,
    bis_datum
  ),
  rainstation_id = sprintf("DWD_%04d", Stations_id))

# read hourly rain data of Berlin rain stations
Berlin_rain_hourly_list <- stats::setNames(lapply(Berlin_rain_hourly_stations$url, function(url) {
  rdwd::dataDWD(url)
}),
nm = Berlin_rain_hourly_stations$rainstation_id)

Berlin_rain_hourly_df <- dplyr::bind_rows(Berlin_rain_hourly_list, .id = "rainstation_id")

rain <- Berlin_rain_hourly_df %>%
  dplyr::select(rainstation_id, MESS_DATUM, R1) %>%
  tidyr::pivot_wider(names_from = rainstation_id, values_from = R1) %>%
  dplyr::rename(datetime = MESS_DATUM)

usethis::use_data(rain, overwrite = TRUE)
