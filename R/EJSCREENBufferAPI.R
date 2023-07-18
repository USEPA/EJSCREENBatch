## EPA--NCEE 2023
## This function calls the EJSCREEN API


## Inputs:
# data: facility location data (as sf object)
# dist: a numeric value to designate the buffer distance.

## Output: a single data.frame

EJSCREENBufferAPI <- function(input_data, dist){
  
  data <- input_data %>%
    dplyr::mutate(apilong = sf::st_coordinates(.)[,1],
           apilat = sf::st_coordinates(.)[,2]) %>%
    tibble::rowid_to_column()

  ## Run through the EJSCREEN api
  dta.list <- vector(mode = 'list', length = dim(data)[1]) 
  
  for (i in 1:dim(data)[1]){   
    print(paste0('Iteration #: ',i))   
    ej.api <- httr::GET(     
      paste0('https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=&',
             'geometry={"spatialReference":{"wkid":4326},"x":',
             data$apilong[[i]], ',"y":', data$apilat[[i]],
             '}&distance=',
             dist,'&unit=9035&areatype=&areaid=&f=pjson'
             )   
      )$content %>%     
      rawToChar() %>%     
      jsonlite::fromJSON()
    
    ej.data <- lapply(ej.api$data, as.data.table)
    if(length(ej.data) > 0){
      ej.merged <- cbind(ej.data[[1]],ej.data[[2]][1,],ej.data[[3]][1,]) %>%
        data.table::as.data.table()
      ej.together <- ej.merged[, which(duplicated(names(ej.merged))) := NULL]
    } else {
      ej.together <- NULL
    }
    
    # Only return data from calls without errors   
    # Note: api doesn't return values for coords in highly nonpopulated areas.   
    if (!(is.null(ej.together))){     
      dta.list[[i]] <- unique(ej.together[, geometry := NULL])   
      }     
    rm(ej.data) 
    } 
  
  ## Merge into data.frame
  #  EJSCREEN fields here: https://ejscreen.epa.gov/mapper/ejsoefielddesc.html
  api.datalist <- data.table::rbindlist(dta.list, fill = T, idcol = 'rowid')
  
  return.me <- data %>%
    dplyr::left_join(api.datalist, by = 'rowid') %>%
    dplyr::select(-c(apilong, apilat, rowid))
  
  return(return.me)
}
