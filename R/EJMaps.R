#' EJ Maps
#'
#' Produces an interactive map that displays all facilities. Facility names (user
#' defined) are displayed for each facility. Use national or state percentiles (default: US)
#'
#' @param input.data
#' @param indic.option Options are 'total', 'environmental','demographic'. 'total' is default.
#' @param perc.geog
#' @param save.options
#'
#' @return
#' @export
#'
#' @examples
#' maps <- EJMaps(input.data = z, perc.geog = 'US', save.option = F)
EJMaps <- function(input.data, indic.option = NULL, perc.geog = NULL,
                   input.name = NULL, save.options = F){

  ## 3 possible color schemes: by env., demo. or total above 80th
  if(is.null(indic.option)){
    ind.option <- 'total' #default values
  } else if (indic.option %in% c('total', 'environmental','demographic')){
    ind.option <- indic.option  #user inputted values that override default
  } else {
    stop("Indicator option not supported. Please specify one of the types:
         total, environmental, demographic")
  }

  ## Use percentile at national or state level?
  if(is.null(perc.geog)){
    geog.ind <- 'US'
  } else if (perc.geog %in% c('US', 'state')){
    geog.ind <- perc.geog
  } else{
    stop('Accepted geographies are US and state.')
  }

  EJ.maps <- list()

  for (i in 1:length(input.data$EJ.facil.data)){
    # Colors:
    if (ind.option == 'total'){
      map.data <- input.data$EJ.facil.data[[i]] %>%
        dplyr::mutate(`Total indicators above 80th %ile` = as.factor(
          as.numeric(as.character(`Env. indicators above 80th %ile`)) +
            as.numeric(as.character(`Demo. indicators above 80th %ile`))
        )
        )

      # Add facility name if given by user.
      if(!is.null(input.name) &
         (length(input.name) == dim(input.data$EJ.facil.data[[i]])[1]/2)){
        name.id <- as.data.frame(input.name) %>%
          tibble::rowid_to_column('shape_ID')
        names(name.id)[2] <- 'LocationID'
        map.data <- map.data %>%
          dplyr::inner_join(name.id) %>%
          dplyr::relocate(LocationID)
      }

      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")),
        domain = na.omit(map.data)$`Indexes above 80th %ile`)

      # Leaflet object
      EJ.maps[[names(input.data$EJ.facil.data)[i]]] <-
        leaflet::leaflet(data = sf::st_as_sf(dplyr::filter(na.omit(map.data), geography == 'US'),
                                crs = 4326)) %>% 
        leaflet::addTiles() %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(sf::st_as_sf(map.data))[,1]),
                  lat1 = min(sf::st_coordinates(sf::st_as_sf(map.data))[,2]),
                  lng2 = max(sf::st_coordinates(sf::st_as_sf(map.data))[,1]),
                  lat2 = max(sf::st_coordinates(sf::st_as_sf(map.data))[,2])) %>%
        leaflet::addCircleMarkers(radius = 5,
                         color = ~ pal(`Total indicators above 80th %ile`),
                         opacity = 0.75,
                         popup = popupTable(dplyr::filter(na.omit(map.data), geography == geog.ind),
                                            feature.id = F, row.numbers = F,
                                            zcol = names(na.omit(map.data))[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~`Total indicators above 80th %ile`, position = "bottomright")
    }

    if (ind.option == 'environmental'){

      map.data <- input.data$EJ.facil.data[[i]]

      # Add facility name if given by user.
      if(!is.null(input.name) &
         (length(input.name) == dim(input.data$EJ.facil.data[[i]])[1]/2)){
        name.id <- as.data.frame(input.name) %>%
          tibble::rowid_to_column('shape_ID')
        names(name.id)[2] <- 'LocationID'
        map.data <- map.data %>%
          dplyr::inner_join(name.id) %>%
          dplyr::relocate(LocationID)
      }

      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")),
        domain = na.omit(input.data$EJ.facil.data[[i]])$`Env. indicators above 80th %ile`)

      # Leaflet object
      EJ.maps[[names(input.data$EJ.facil.data)[i]]] <-
        leaflet::leaflet(data = sf::st_as_sf(dplyr::filter(na.omit(input.data$EJ.facil.data[[i]]), geography == 'US'),
                                crs = 4326)) %>% 
        leaflet::addTiles() %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,1]),
                  lat1 = min(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,2]),
                  lng2 = max(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,1]),
                  lat2 = max(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,2])) %>%
        leaflet::addCircleMarkers(radius = 5,
                         color = ~ pal(`Env. indicators above 80th %ile`),
                         opacity = 0.75,
                         #clusterOptions = markerClusterOptions(),
                         popup = popupTable(dplyr::filter(na.omit(input.data$EJ.facil.data[[i]]), geography == geog.ind),
                                            feature.id = F, row.numbers = F,
                                            zcol = names(na.omit(input.data$EJ.facil.data[[i]]))[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~`Env. indicators above 80th %ile`, position = "bottomright")
    }

    if (ind.option == 'demographic'){

      map.data <- input.data$EJ.facil.data

      # Add facility name if given by user.
      if(!is.null(input.name) &
         (length(input.name) == dim(input.data$EJ.facil.data[[i]])[1]/2)){
        name.id <- as.data.frame(input.name) %>%
          tibble::rowid_to_column('shape_ID')
        names(name.id)[2] <- 'LocationID'
        map.data <- map.data %>%
          dplyr::inner_join(name.id) %>%
          dplyr::relocate(LocationID)
      }

      # Color palette
      pal <- leaflet::colorFactor(
        rev(RColorBrewer::brewer.pal(n=11, "Spectral")),
        domain = na.omit(input.data$EJ.facil.data[[i]])$`Demo. indicators above 80th %ile`)

      # Leaflet object
      EJ.maps[[names(input.data$EJ.facil.data)[i]]] <-
        leaflet::leaflet(data = sf::st_as_sf(dplyr::filter(na.omit(input.data$EJ.facil.data[[i]]), geography == 'US'),
                                crs = 4326)) %>% 
        leaflet::addTiles() %>%
        leaflet::fitBounds(lng1 = min(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,1]),
                  lat1 = min(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,2]),
                  lng2 = max(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,1]),
                  lat2 = max(sf::st_coordinates(sf::st_as_sf(input.data$EJ.facil.data[[i]]))[,2])) %>%
        leaflet::addCircleMarkers(radius = 5,
                         opacity = 0.75,
                         color = ~ pal(`Demo. indicators above 80th %ile`),
                         #clusterOptions = markerClusterOptions(),
                         popup = popupTable(dplyr::filter(na.omit(input.data$EJ.facil.data[[i]]), geography == geog.ind),
                                            feature.id = F, row.numbers = F,
                                            zcol = names(na.omit(input.data$EJ.facil.data[[i]]))[1:20])) %>%
        leaflet::addLegend(pal = pal, values = ~`Demo. indicators above 80th %ile`, position = "bottomright")
    }

    # Save me
    if (save.options == T) {
      ifelse(!dir.exists(file.path(getwd(),"EJmaps/")),
             dir.create(file.path(getwd(),"EJmaps/")), FALSE)

      htmlwidgets::saveWidget(EJ.maps[[names(input.data$EJ.facil.data)[i]]],
                 file= paste0('EJmaps/', names(input.data$EJ.facil.data)[i],'.html'))
      mapview::mapshot(EJ.maps[[names(input.data$EJ.facil.data)[i]]],
              file = paste0('EJmaps/', names(input.data$EJ.facil.data)[i],".png"))
    }
  }

  return(EJ.maps)
}
