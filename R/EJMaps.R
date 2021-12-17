#' EJ Maps
#'
#' Produces an interactive map that displays all facilities. Facility names (user
#' defined) are displayed for each facility. Use national or state percentiles (default: US)
#'
#' @param input_data
#' @param indic_option 'total', 'environmental','demographic'. 'total' is default.
#' @param perc_geog 'state' or 'US'. Default is 'US'.
#' @param save_option Option to save map to a folder in working directory. Default is FALSE.
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
#' maps <- EJMaps(input_data = z, perc_geog = 'US', save.option = F)
EJMaps <- function(input_data, indic_option = NULL, perc_geog = NULL, save_option = F, directory){

  ## 3 possible color schemes: by env., demo. or total above 80th
  if(is.null(indic_option)){
    ind.option <- 'total' #default values
  } else if (indic_option %in% c('total', 'environmental','demographic')){
    ind.option <- indic_option  #user inputted values that override default
  } else {
    stop("Indicator option not supported. Please specify one of the types:
         total, environmental, demographic")
  }

  ## Use percentile at national or state level?
  if(is.null(perc_geog)){
    geog.ind <- 'US'
  } else if (perc_geog %in% c('US', 'state')){
    geog.ind <- perc_geog
  } else {
    stop('Accepted geographies are US and state.')
  }
  
  # Searching the variable name string by character index to extract threshold
  thrshld <- input_data$EJ.facil.data[[1]] %>% 
    dplyr::select(starts_with('Env. indicators')) %>% 
    names() %>% 
    gsub(".*above (.+)th.*",'\\1',.) %>% 
    as.numeric()

  EJ.maps <- list()

  for (i in 1:length(input_data$EJ.facil.data)){

    map.data <- input_data$EJ.facil.data[[i]] %>%
      dplyr::mutate_at(vars(dplyr::ends_with('%ile')), funs(as.integer(as.character(.)))) %>%
      dplyr::mutate(!!paste0('Total indicators above ',thrshld,'th %ile') :=
                      rowSums(dplyr::select(., dplyr::ends_with('%ile')))) %>%
      dplyr::filter(!is.na(geometry)) %>%
      dplyr::filter(geography == geog.ind) %>%
      st_as_sf(crs = 4326)
    
    if (ind.option == 'total'){
      
      col.keep <- map.data %>% 
        dplyr::select(starts_with('Total indicators')) %>% 
        st_drop_geometry() %>%
        names()
      
      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")), # NOTE: brewer.pal can't go over 11 :(
        domain = map.data[[col.keep]])

      # Leaflet object
      EJ.maps[[stringr::str_sub(names(input_data$EJ.facil.data),
                                start = 7)[i]]] <-
        leaflet::leaflet(data = map.data) %>%
        leaflet::addTiles() %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(map.data)[,1]),
                  lat1 = min(sf::st_coordinates(map.data)[,2]),
                  lng2 = max(sf::st_coordinates(map.data)[,1]),
                  lat2 = max(sf::st_coordinates(map.data)[,2])) %>%
        leaflet::addCircleMarkers(radius = 5,
                         color = ~ pal(get(col.keep)),
                         opacity = 0.75,
                         popup = leafpop::popupTable(map.data,
                                            feature.id = F, row.numbers = F,
                                            zcol = names(map.data)[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~get(col.keep), 
                           title = col.keep, position = "bottomright")
    }

    if (ind.option == 'environmental'){
      
      col.keep <- map.data %>% 
        dplyr::select(starts_with('Env. indicators')) %>% 
        st_drop_geometry() %>%
        names()

      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")),
        domain = map.data[[col.keep]])

      # Leaflet object
      EJ.maps[[stringr::str_sub(names(input_data$EJ.facil.data),
                                start = 7)[i]]] <-
        leaflet::leaflet(data = map.data) %>%
        leaflet::addTiles() %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(map.data)[,1]),
                           lat1 = min(sf::st_coordinates(map.data)[,2]),
                           lng2 = max(sf::st_coordinates(map.data)[,1]),
                           lat2 = max(sf::st_coordinates(map.data)[,2])) %>%
        leaflet::addCircleMarkers(radius = 5,
                                  color = ~ pal(get(col.keep)),
                                  opacity = 0.75,
                                  popup = leafpop::popupTable(map.data,
                                                              feature.id = F, row.numbers = F,
                                                              zcol = names(map.data)[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~get(col.keep), 
                           title = col.keep, position = "bottomright")
    }

    if (ind.option == 'demographic'){
      
      col.keep <- map.data %>% 
        dplyr::select(starts_with('Demo. indicators')) %>% 
        st_drop_geometry() %>%
        names()
      
      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")),
        domain = map.data[[col.keep]])

      # Leaflet object
      EJ.maps[[stringr::str_sub(names(input_data$EJ.facil.data),
                                start = 7)[i]]] <-
        leaflet::leaflet(data = map.data) %>%
        leaflet::addTiles() %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(map.data)[,1]),
                           lat1 = min(sf::st_coordinates(map.data)[,2]),
                           lng2 = max(sf::st_coordinates(map.data)[,1]),
                           lat2 = max(sf::st_coordinates(map.data)[,2])) %>%
        leaflet::addCircleMarkers(radius = 5,
                                  color = ~ pal(get(col.keep)),
                                  opacity = 0.75,
                                  popup = leafpop::popupTable(map.data,
                                                              feature.id = F, row.numbers = F,
                                                              zcol = names(map.data)[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~get(col.keep), 
                           title = col.keep, position = "bottomright")
    }

    # Save me
    if (save_option == T) {
      ifelse(!dir.exists(file.path(directory,"EJmaps")),
             dir.create(file.path(directory,"EJmaps")), FALSE)

      htmlwidgets::saveWidget(EJ.maps[[stringr::str_sub(names(input_data$EJ.facil.data),
                                                        start = 7)[i]]],
                 file= paste0(directory,'/EJmaps/',indic_option,
                              '_',geog.ind,'_map_',
                              stringr::str_sub(names(input_data$EJ.facil.data),
                                               start = 7)[i],'.html'))
      mapview::mapshot(EJ.maps[[stringr::str_sub(names(input_data$EJ.facil.data),
                                                 start = 7)[i]]],
              file = paste0(directory,'/EJmaps/',indic_option,
                            '_',geog.ind,'_map_',
                            stringr::str_sub(names(input_data$EJ.facil.data),
                                             start = 7)[i],".png"))
    }
  }

  return(EJ.maps)
}
