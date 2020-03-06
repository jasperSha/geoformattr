#' Geographical Coordinates Converter
#'
#' Splits WKT formatted coordinates into separate longitude/latitude columns
#' @param dataframe, only POINT-type
#' @keywords WKT, longitude, latitude, geometry, geography
#' @example geom_to_lonlat(dataframe)
#' @return dataframe columns long/lat
#' @name geom_to_lonlat
#' @export
#'
#'
geom_to_lonlat <- function(x) {
  if (any(sf::st_geometry_type(x) != "POINT")) {
    stop("Selecting non-points is not implemented.")
  }
  coord_df <- sf::st_transform(x, sf::st_crs("+proj=longlat +datum=WGS84"))%>%
    sf::st_coordinates()%>%
    dplyr::as_tibble()%>%
    dplyr::select(X, Y) %>%
    dplyr::rename(lon = X, lat = Y)
  out <- sf::st_set_geometry(x, NULL) %>%
    dplyr::bind_cols(coord_df)
  return(out)
}

