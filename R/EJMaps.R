#' Produces an interactive map that displays all facilities.
#'
#' @param input_data Required. Screening object returned from an EJFunction() call.
#' @param indic_option EJSCREEN indicators to display? Options are 'total','environmental','demographic'. Default is 'total'.
#' @param geography National or state percentiles used in map? Options are 'state' or 'US' (default).
#' @param threshold Percentile threshold for map coloring. Default is 80.
#' @param facil_name Column name in LOI input data denoting LOI name. String only.
#'
#' @return A list containing leaflet map object(s).
#' @export
#'
#' @examples
#' maps <- EJMaps(input_data = z, geography = 'US', save.option = F)
EJMaps <- function(input_data, indic_option = NULL, geography = NULL,
                   threshold = 80, facil_name = NULL){

  ## 3 possible color schemes: by env., demo. or total above 80th
  if(is.null(indic_option)){
    indic_option <- 'total' #default
  } else if(!(indic_option %in% c('total', 'environmental','demographic'))){
    stop("Indicator option not supported. Please specify one of the types:
         total, environmental, demographic")
  }

  ## Use percentile at national or state level?
  if(is.null(geography)){
    geography <- 'US' #default
  } else if(!(geography %in% c('US', 'state'))){
    stop('Accepted geographies are "US" and "state".')
  }

  ## Pull list of demographic variables
  demVarList <- names(input_data$EJ.loi.data[[1]]
                      )[names(input_data$EJ.loi.data[[1]]) %in%
                          c('PEOPCOLORPCT', 'MINORPCT', 'LOWINCPCT', 'LINGISOPCT',
                            'UNEMPPCT', 'UNDER5PCT', 'LESSHSPCT', 'OVER64PCT', 'LIFEEXPPCT')]
  envVarList <- names(input_data$EJ.loi.data[[1]]
                      )[names(input_data$EJ.loi.data[[1]]) %in%
                          c('PM25', 'OZONE', 'DSLPM', 'CANCER', 'RESP', 'RSEI_AIR', 'PTRAF',
                            'PNPL', 'PRMP', 'PRE1960PCT', 'PTSDF', 'PWDIS', 'UST')]

  if (is.null(facil_name)) {
    LOIList <- 'shape_ID'
  } else if (facil_name %in% names(input_data$EJ.loi.data[[1]])) {
    LOIList <- facil_name
  } else {
    stop('facil_name is not a valid column name in input_data.')
  }

  map.list <- vector(mode = 'list', length = length(input_data$EJ.loi.data))
  for (i in 1:length(input_data$EJ.loi.data)){
    local.dta <- input_data$EJ.loi.data[[i]] %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(!!paste0('Env. indicators above ',threshold,'th %ile') :=
                      as.numeric(rowSums(dplyr::select(as.data.frame(input_data$EJ.loi.data[[1]]),
                                                      paste0("P_",envVarList,"_",geography)) > threshold,
                                        na.rm = TRUE))) %>%
      dplyr::mutate(!!paste0('Dem. indicators above ',threshold,'th %ile') :=
                      as.numeric(rowSums(dplyr::select(as.data.frame(input_data$EJ.loi.data[[1]]),
                                                       paste0("P_",demVarList,"_",geography)) > threshold,
                                         na.rm = TRUE))) %>%
      dplyr::mutate(!!paste0('Total indicators above ',threshold,'th %ile') :=
                      rowSums(dplyr::select(.,dplyr::ends_with('%ile')))) %>%
      dplyr::left_join(input_data$EJ.loi.data[[i]] %>%
                         dplyr::select(shape_ID)) %>%
      sf::st_as_sf()

    #Label the column on which the palette should be based.
    if (indic_option == 'demographic') {
      map.data <- local.dta %>%
        dplyr::select(c(dplyr::all_of(LOIList), paste0("P_",demVarList,"_",geography)),
                      dplyr::starts_with('Dem. indicators'))

      col.keep <- map.data %>%
        dplyr::select(dplyr::starts_with('Dem. indicators')) %>%
        sf::st_drop_geometry() %>%
        names()
    } else if (indic_option == 'environmental') {
      map.data <- local.dta %>%
        dplyr::select(c(dplyr::all_of(LOIList), paste0("P_",envVarList,"_",geography)),
                      dplyr::starts_with('Env. indicators'))

      col.keep <- map.data %>%
        dplyr::select(dplyr::starts_with('Env. indicators')) %>%
        sf::st_drop_geometry() %>%
        names()
    } else {
      map.data <- local.dta %>%
        dplyr::select(c(dplyr::all_of(LOIList), paste0("P_",demVarList,"_",geography),
                        paste0("P_",envVarList,"_",geography)),
                      dplyr::starts_with('Total indicators'))

      col.keep <- map.data %>%
        dplyr::select(dplyr::starts_with('Total indicators')) %>%
        sf::st_drop_geometry() %>%
        names()
    }

    #Color palette for the map.
    pal <- leaflet::colorNumeric(
      palette = 'Reds',
      domain = map.data[[col.keep]])

    # Hacky -- fix later. Look at first geom_type in data
    if (sf::st_geometry_type(map.data)[1] == 'POINT'){
      #Map for point objects
      map.list[[i]] <- leaflet::leaflet(data = map.data) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(map.data)[,1]),
                           lat1 = min(sf::st_coordinates(map.data)[,2]),
                           lng2 = max(sf::st_coordinates(map.data)[,1]),
                           lat2 = max(sf::st_coordinates(map.data)[,2])) %>%
        leaflet::addCircleMarkers(radius = 5,
                                  color = ~ pal(get(col.keep)),
                                  opacity = 0.75,
                                  layerId = map.data$shape_ID,
                                  popup = leafpop::popupTable(map.data %>%
                                                                sf::st_set_geometry(NULL) %>%
                                                                dplyr::rename_with(stringr::str_replace,
                                                                                   pattern = "P_",
                                                                                   replacement = "") %>%
                                                                dplyr::rename_with(stringr::str_replace,
                                                                                   pattern = paste0("_",geography),
                                                                                   replacement = ""),
                                                              feature.id = F,
                                                              row.numbers = F)) %>%
        leaflet::addLegend(pal = pal, values = ~get(col.keep),
                           title = col.keep, position = "bottomright")
    } else if (sf::st_geometry_type(map.data)[1] %in% c('LINESTRING','MULTILINESTRING')) {
      #Map for non-point objects
      map.list[[i]] <- leaflet::leaflet(data = map.data) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(map.data)[,1]),
                           lat1 = min(sf::st_coordinates(map.data)[,2]),
                           lng2 = max(sf::st_coordinates(map.data)[,1]),
                           lat2 = max(sf::st_coordinates(map.data)[,2])) %>%
        leaflet::addPolylines(color = ~ pal(get(col.keep)),
                             weight = 2,
                             opacity = 1,
                             layerId = map.data$shape_ID,
                             popup = leafpop::popupTable(map.data %>%
                                                           sf::st_set_geometry(NULL) %>%
                                                           dplyr::rename_with(stringr::str_replace,
                                                                              pattern = "P_",
                                                                              replacement = "") %>%
                                                           dplyr::rename_with(stringr::str_replace,
                                                                              pattern = paste0("_",geography),
                                                                              replacement = ""),
                                                         feature.id = F,
                                                         row.numbers = F)) %>%
        leaflet::addLegend(pal = pal, values = ~get(col.keep),
                           title = col.keep, position = "bottomright")
    } else {
      #Map for non-point objects
      map.list[[i]] <- leaflet::leaflet(data = map.data) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(map.data)[,1]),
                           lat1 = min(sf::st_coordinates(map.data)[,2]),
                           lng2 = max(sf::st_coordinates(map.data)[,1]),
                           lat2 = max(sf::st_coordinates(map.data)[,2])) %>%
        leaflet::addPolygons(color = ~ pal(get(col.keep)),
                             weight = 2,
                             opacity = 1,
                             fillOpacity = 0.5,
                             fillColor = ~ pal(get(col.keep)),
                             layerId = map.data$shape_ID,
                             popup = leafpop::popupTable(map.data %>%
                                                           sf::st_set_geometry(NULL) %>%
                                                           dplyr::rename_with(stringr::str_replace,
                                                                              pattern = "P_",
                                                                              replacement = "") %>%
                                                           dplyr::rename_with(stringr::str_replace,
                                                                              pattern = paste0("_",geography),
                                                                              replacement = ""),
                                                         feature.id = F,
                                                         row.numbers = F)) %>%
        leaflet::addLegend(pal = pal, values = ~get(col.keep),
                           title = col.keep, position = "bottomright")
    }
  }

  return(map.list)
}
