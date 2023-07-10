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
#' maps <- EJMaps(input_data = z, geography = 'US', save.option = F)
EJMaps <- function(input_data, indic_option = NULL, geography = NULL, 
                   threshold, = 80, save_option = F){

  ## 3 possible color schemes: by env., demo. or total above 80th
  if(is.null(indic_option)){
    ind.option <- 'total' #default values
  } else if(!(indic_option %in% c('total', 'environmental','demographic'))){
    stop("Indicator option not supported. Please specify one of the types:
         total, environmental, demographic")
  } 
  
  ## Use percentile at national or state level?
  if(is.null(geography)){
    geography <- 'US'
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
  local.dta <- input_data$EJ.loi.data[[1]] %>%
    dplyr::mutate(lat = sf::st_coordinates(.)[,2],
           long = sf::st_coordinates(.)[,1]) %>%
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
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
    
  map.data <- local.dta
  
  col.keep <- map.data %>% 
    dplyr::select(dplyr::starts_with('Total indicators')) %>% 
    sf::st_drop_geometry() %>%
    names()
  
  pal <- leaflet::colorNumeric(
    palette = 'Reds', # NOTE: brewer.pal can't go over 11 :(
    domain = map.data[[col.keep]])
  
  map.data
  
  leaflet::leaflet(data = map.data) %>%
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
                                                            dplyr::select(shape_ID,
                                                                          dplyr::contains(envVarList), 
                                                                          dplyr::contains(demVarList)) %>%
                                                            dplyr::rename_with(~stringr::str_remove(., 'P_')) %>%
                                                            dplyr::rename_at(c(envVarList, demVarList)
                                                                             ,paste0, "_Raw") %>%
                                                            dplyr::rename_with(~stringr::str_remove(., '_AIR')) %>%
                                                            tidyr::pivot_longer(-c(shape_ID, geometry),
                                                                                names_to = c("Variable Name",".value"),
                                                                                names_sep = "_"),
                                                          feature.id = F, row.numbers = F)) %>%#,
                                                          #zcol = names(map.data)[1:20])) %>%
    leaflet::addLegend(pal = pal, values = ~get(col.keep), 
                       title = col.keep, position = "bottomright")

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
