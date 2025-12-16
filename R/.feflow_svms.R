path_rch <- "Y:/GRW_Department/PROJECTS/IMPETUS/Work-packages/WP4_Demonstration_KWB/CS-Berlin/04_Modelling/Grundwasser/Feflow Balance/GIS"

shapefiles_rch <- list.files(path = path_rch,
                             full.names = TRUE,
                             pattern = "GWN\\.shp$")


path_ezg_fri <- file.path(path_rch, "catchments/FRIWUHKAU/Shapefiles_Selected/MS0/Edges")


shapefiles_ezg_fri <- list.files(path = path_ezg_fri,
                                 full.names = TRUE,
                                 pattern = "Edge\\.shp$")


rch_fri <- sf::read_sf(shapefiles_rch[2])

rch_fri %>%
  sf::st_zm() %>%
  ggplot2::ggplot(mapping = ggplot2::aes(col = RCH)) +
  ggplot2::geom_sf()



lakes_berlin <- sf::read_sf("lakes_berlin/Gewaesser_Berlin_Flaechen.shp",
                            options = "ENCODING=WINDOWS-1252") %>%
  sf::st_transform(25833) %>%
  sf::st_make_valid()


lakes_berlin_soldner <- lakes_berlin %>%
  sf::st_transform(crs = 3068) %>%
  sf::st_union()


rch_fri_intersects <- lapply(1:10, function(i) {

edges_sf <- shapefiles_ezg_fri[i] %>%
  sf::read_sf() %>%
  sf::st_set_crs(3068) %>%
  sf::st_zm()

# 5) Linien vereinigen (union), um eine Gesamtgeometrie zu bekommen:
lines_union <- sf::st_union(edges_sf)

# 6) "Noden": st_node (aus lwgeom) erzeugt Schnittpunkte & segmentiert
lines_noded <- sf::st_node(lines_union)

# 7) Polygonisieren
polygons <- sf::st_polygonize(lines_noded)

# 8) Nur POLYGON-Elemente extrahieren (häufig GEOMETRYCOLLECTION)
polygons_only <- sf::st_collection_extract(polygons, "POLYGON") %>%
  sf::st_transform(crs = 3068)


if (crop_waterbodies) {
  polygons_only  <- polygons_only  %>%
    sf::st_difference(lakes_berlin_soldner)
}


# polygons_only %>%
#   sf::st_as_sf() %>%
#   dplyr::rename(geometry = x) %>%
#   sf::st_intersects(rch_fri %>% sf::st_zm())


rch_fri_match <- polygons_only %>%
  sf::st_intersects(rch_fri %>% sf::st_zm())

sf::st_intersection(rch_fri[unlist(rch_fri_match), ] %>% sf::st_zm()) %>%
  dplyr::mutate(area_m2 = sf::st_area(.))
})


gg <- shapefiles_ezg_fri[i] %>%
  sf::read_sf() %>%
  sf::st_set_crs(3068) %>%
  sf::st_zm() %>%
  # 5) Linien vereinigen (union), um eine Gesamtgeometrie zu bekommen:
  dplyr::mutate(geometry = geometry %>%
                  sf::st_union() %>%
                  sf::st_node() %>%
                  sf::st_polygonize() %>%
                  sf::st_collection_extract(type = "POLYGON")) %>%
  sf::st_zm() %>%
  #ggplot2::ggplot(mapping = ggplot2::aes(col = RCH)) +
  ggplot2::geom_sf()


# 5) Linien vereinigen (union), um eine Gesamtgeometrie zu bekommen:
lines_union <- shapefiles_ezg_fri[i] %>%
  sf::read_sf() %>%
  sf::st_set_crs(3068) %>%
  st_union(edges_sf)

# 6) "Noden": st_node (aus lwgeom) erzeugt Schnittpunkte & segmentiert
lines_noded <- st_node(lines_union)

# 7) Polygonisieren
polygons <- st_polygonize(lines_noded)

# 8) Nur POLYGON-Elemente extrahieren (häufig GEOMETRYCOLLECTION)
polygons_only <- st_collection_extract(polygons, "POLYGON")

gg
})


rch_fri %>%
  sf::st_zm() %>%
  #dplyr::filter(ELEMENT %in% gg$data$EDGEID) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(col = RCH)) +
  ggplot2::geom_sf()

i <- 1


files <- shapefiles_ezg_fri

fri_ezg <- stats::setNames(lapply(files, function(file) {

efl_intersects <- rch_fri %>%
  sf::st_zm() %>%
  sf::st_intersection(file %>%
                        sf::read_sf() %>%
                        sf::st_set_crs(3068)) %>%
  dplyr::as_tibble() %>%
  dplyr::group_by(ELEMENT) %>%
  dplyr::summarise(RCH = dplyr::first(RCH)) %>%
  dplyr::left_join(rch_fri %>%  dplyr::select(ELEMENT, geometry)) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(area_m2 = sf::st_area(.))
}), nm = basename(files))


fri <- fri_ezg %>%
  dplyr::bind_rows(.id = "filename") %>%
  sf::st_zm() %>%
  dplyr::group_by(filename) %>%
  dplyr::summarise(RCH = sum(RCH * area_m2) / sum(area_m2),
                   area_m2 = sum(area_m2))

fri$area_m2


efl_intersects %>%
  sf::st_zm() %>%
  ggplot2::ggplot(mapping = ggplot2::aes(col = RCH)) +
  ggplot2::geom_sf()

plotly::ggplotly(gg)


shapefiles_ezg_fri[1] %>%
  sf::read_sf() %>%
  sf::st_zm() %>%
  sf::st_set_crs(3068) %>%
  sf::st_join(rch_fri %>% sf::st_zm())
