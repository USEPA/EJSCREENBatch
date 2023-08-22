#' Support function to call EJSCREEN API
#'
#' This is a canned loop function for calling the EJSCREEN API and returning the fire-hose of data as a tidy data.frame (one row per input coordinate/shape).
#'
#' @param input_data facility location data as sf data.frame (point, (multi-)linestring, (multi-)polygon).
#' @param dist a numeric value to designate the buffer distance in miles.
#'
#' @return a tidy data.frame
#' @export
#'
#' @examples
EJSCREENBufferAPI <- function(input_data, dist){

  data <- input_data %>%
    tibble::rowid_to_column()

  # API POST urls:
  base <- 'https://ejscreen.epa.gov/arcgis/rest/services/ejscreen/'
  surl <- list(
    "ejquery/MapServer/exts/EJScreen_DemogReports/Get2021DemogACS",
    "ejscreen_v2023_with_as_cnmi_gu_vi/MapServer/exts/EJCensusReports/GetEJScreen",
    "ejscreen_extra/MapServer/exts/EJCensusReports/GetEJExtra"
  )
  post.urls <- as.list(paste0(base, surl))

  # API call loop
  api.call <- function(x){
    jsonlite::fromJSON(
      httr::content(
        httr::VERB('POST', url = x, body = post.body),
        as = "text", encoding = "UTF-8")
    )
  }

  ## Run through the EJSCREEN api
  dta.list <- vector(mode = 'list', length = dim(data)[1])

  for (i in 1:dim(data)[1]){
    print(paste0('Iteration #: ',i))

    # Determine geometry type and appropriate API string.
    if (sf::st_geometry_type(data[i,]) == 'POINT'){
      dta <- data[i,] %>%
        dplyr::mutate(apilong = sf::st_coordinates(.)[,1],
               apilat = sf::st_coordinates(.)[,2])

      geom.string <- paste0('{"spatialReference":{"wkid":4326},"x":',
                            dta$apilong, ',"y":', dta$apilat,'}')
    } else if (sf::st_geometry_type(data[1,]) %in% c('LINESTRING','MULTILINESTRING')){
      dta <- data[i,]

      geom.string <- paste0('{"spatialReference":{"wkid":4326},"paths":[[',
                            sf::st_as_text(dta[1,]$geometry) %>%
                              gsub(".*\\(\\(","",.) %>%
                              gsub("\\)\\)","",.) %>%
                              strsplit(.,',') %>%
                              unlist() %>%
                              trimws(which = 'left') %>%
                              gsub(" ",",",.) %>%
                              paste0("[",.,"]") %>%
                              paste(., collapse=","),
                            ']]}')
    } else if (sf::st_geometry_type(data[1,]) %in% c('POLYGON','MULTIPOLYGON')){
      dta <- data[i,]

      geom.string <- paste0('{"spatialReference":{"wkid":4326},"rings":[[',
                            sf::st_as_text(dta[1,]$geometry) %>%
                              gsub(".*\\(\\(","",.) %>%
                              gsub("\\)\\)","",.) %>%
                              strsplit(.,',') %>%
                              unlist() %>%
                              trimws(which = 'left') %>%
                              gsub(" ",",",.) %>%
                              paste0("[",.,"]") %>%
                              paste(., collapse=","),
                            ']]}')
    } else {
      print('This function only accepts sf objects of class: POINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON.')
      next
    }

    # Information to send in post query.
    post.body <- list(namestr='',
                      geometry=geom.string,
                      distance=as.character(dist),
                      unit='9035',
                      areatype='',
                      areaid='',
                      f='json')

    # Call APIs, merge output from the 3 separate calls
    dta.list[[i]] <- tryCatch(
      {
        df <- lapply(post.urls, api.call)
        df.together <- cbind(as.data.frame(t(as.data.frame(df[[1]]$statGroupList$statList) %>%
                                               tibble::column_to_rownames(var = 'name'))) %>%
                               tibble::remove_rownames(),
                             as.data.frame(t(unlist(df[[2]]))),
                             as.data.frame(t(unlist(df[[3]])))
        )
        df.together
      },
      error=function(cond){
        message(cond)
        return(NULL)
      },
      warning=function(cond){
        message(cond)
        return(NULL)
      }
    )
    }

  ## Merge into data.frame
  #  drop duplicated columns
  #  EJSCREEN fields here: https://ejscreen.epa.gov/mapper/ejsoefielddesc.html
  api.datalist <- data.table::rbindlist(dta.list, fill = T, idcol = 'rowid')
  api.datalist <- api.datalist[, which(duplicated(names(api.datalist))) := NULL]

  return(data %>%
           dplyr::left_join(api.datalist, by = 'rowid') %>%
           dplyr::select(-c(rowid, dplyr::starts_with("geometry"))))
}
