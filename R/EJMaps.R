#' EJ Maps
#'
#' Produces an interactive map that displays all facilities. Facility names (user
#' defined) are displayed for each facility. Use national or state percentiles (default: US)
#'
#' @param input_data
#' @param indic_option 'total', 'environmental','demographic'. 'total' is default.
#' @param perc_geog State or US. Default is US.
#' @param save_option Option to save map to a folder in working directory. Default is FALSE.
#'
#' @return
#' @export
#'
#' @examples
#' maps <- EJMaps(input_data = z, perc_geog = 'US', save.option = F)
EJMaps <- function(input_data, indic_option = NULL, perc_geog = NULL, save_option = F, working_dir){

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

  EJ.maps <- list()

  for (i in 1:length(input_data$EJ.facil.data)){

    map.data <- input_data$EJ.facil.data[[i]] %>%
      dplyr::mutate(`Total indicators above 80th %ile` = as.factor(
        as.numeric(as.character(`Env. indicators above 80th %ile`)) +
          as.numeric(as.character(`Demo. indicators above 80th %ile`))
      )) %>%
      dplyr::filter(!is.na(geometry)) %>%
      dplyr::filter(geography == geog.ind) %>%
      st_as_sf(crs = 4326)

    if (ind.option == 'total'){
      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")), # NOTE: brewer.pal can't go over 11 :(
        domain = map.data$`Total indicators above 80th %ile`)

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
                         color = ~ pal(`Total indicators above 80th %ile`),
                         opacity = 0.75,
                         popup = leafpop::popupTable(map.data,
                                            feature.id = F, row.numbers = F,
                                            zcol = names(map.data)[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~`Total indicators above 80th %ile`, position = "bottomright")
    }

    if (ind.option == 'environmental'){

      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")),
        domain = map.data$`Env. indicators above 80th %ile`)

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
                         color = ~ pal(`Env. indicators above 80th %ile`),
                         opacity = 0.75,
                         popup = leafpop::popupTable(map.data,
                                            feature.id = F, row.numbers = F,
                                            zcol = names(map.data)[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~`Env. indicators above 80th %ile`, position = "bottomright")
    }

    if (ind.option == 'demographic'){

      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=6, "Spectral")),
        domain = map.data$`Demo. indicators above 80th %ile`)

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
                         opacity = 0.75,
                         color = ~ pal(`Demo. indicators above 80th %ile`),
                         popup = leafpop::popupTable(map.data,
                                            feature.id = F, row.numbers = F,
                                            zcol = names(map.data)[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~`Demo. indicators above 80th %ile`, position = "bottomright")
    }

    # Save me
    if (save_option == T) {
      ifelse(!dir.exists(working_dir,Sys.time(),"EJmaps"),
             dir.create(file.path(working_dir,Sys.time(),"EJmaps")), FALSE)

      htmlwidgets::saveWidget(EJ.maps[[stringr::str_sub(names(input_data$EJ.facil.data),
                                                        start = 7)[i]]],
                 file= paste0(Sys.time(),'/EJmaps/',indic_option,'_map_',
                              stringr::str_sub(names(input_data$EJ.facil.data),
                                               start = 7)[i],'.html'))
      mapview::mapshot(EJ.maps[[stringr::str_sub(names(input_data$EJ.facil.data),
                                                 start = 7)[i]]],
              file = paste0(Sys.time(),'/EJmaps/',indic_option,'_map_',
                            stringr::str_sub(names(input_data$EJ.facil.data),
                                             start = 7)[i],".png"))
    }
  }

  return(EJ.maps)
}
