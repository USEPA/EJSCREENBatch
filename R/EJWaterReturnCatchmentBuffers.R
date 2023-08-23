#' EJ Return Water Catchment Buffers
#'
#' @param input_data Required. An sf POINT data.frame with coordinates falling in the US.
#' @param ds_us_mode Option for upstream or downstream. Default is downstream, main ("DM"). Other options: "DD","UT","UM".
#' @param ds_us_dist Distance up/downstream. Default is 10 miles.
#'
#' @return This function returns: (1) a SF data.frame containing up/downstream flowlines of a user-specified distance and (2) a list of downstream catchment IDs
#' @export
#'
#' @examples
#'

EJWaterReturnCatchmentBuffers <-  function(input_data, ds_us_mode = 'DM', ds_us_dist = 10){
  # Determine the input_data type:
  # (in future could have this determine object type (sf, numeric list, etc.) without user input)

  if(is(input_data,'sf')){

    if(!is(sf::st_geometry(input_data),"sfc_POINT")){
      stop('Input data must be an sf POINT data.frame or a data.frame with a column titled "comid" containing catchment IDs')
    }

    # Make sure data is 4326 for consistency
    input_data <- sf::st_transform(input_data, crs = 4326)

    # Unfortunately must transform input_data to row-wise list for get_nhdplus()
    doFuture::registerDoFuture()
    future::plan(future::multisession, workers = parallel::detectCores()-2)
    loi.list <- foreach::foreach(i = 1:dim(input_data)[1], .packages='sf') %dorng% {
      input_data[i, ]
    }

    # Function calls USGS API, returns comid if valid, NA otherwise
    parallel.getnhdplus <- function(input){
      tryCatch(
        {
          suppressMessages(nhdplusTools::get_nhdplus(AOI = input,
                                    realization = 'catchment')$featureid)
        },
        error=function(cond) {
          return(1)
        },
        warning=function(cond) {
          return(1)
        }
      )
    }

    # Fills in an empty feature.id vector through parallelized API calls
    feature.id <- furrr::future_pmap(list(loi.list), parallel.getnhdplus,
                                     .options = furrr::furrr_options(seed = T))
    feature.id <- unlist(feature.id)
    future::plan(future::sequential)
  } else {
    # Vector of catchments
    if(is(input_data, 'data.frame') &
       ('comid' %in% names(input_data))){
      feature.id <- input_data$comid
    } else {
      stop('Input data must be an sf POINT data.frame or a data.frame with a column titled "comid" containing catchment IDs')
    }
  }

  # Combine function to use with foreach loop (returns 3 lists)
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }

  # Loop through catchment IDs to extract down/upstream buffer polygons
  future::plan(future::multisession, workers = parallel::detectCores()-2)
  nhd.buffs <- foreach::foreach(i = 1:length(feature.id),
                           .packages = c('nhdplusTools','sf'),
                           .combine = comb,
                           .multicombine = T,
                           .init = list(list(), list())
  ) %dorng% {
    # If comid was returned in last call, enter loop to return up/downstream comids
    if (feature.id[i] != 1) {

      # Create NLDI feature object
      nldi.feature <- list(featureSource = 'comid', featureID = feature.id[i])

      # Make sure something meaningful is going to be returned
      if(length(nhdplusTools::get_nldi_feature(nldi.feature)) > 0) {
        # Try to return the following objects:
        tryCatch({nldi.temp <- nhdplusTools::navigate_nldi(nldi.feature,
                                                           mode = ds_us_mode,
                                                           distance_km = round(ds_us_dist*1.60934))[[2]]

        # First object returned: shapefile for buffered area
        feature.list <- nldi.temp %>%
          sf::st_union() %>%
          sf::st_as_sf()

        # Second object returned: list of relevant comids
        return.catchments <- nldi.temp$nhdplus_comid #pulls out all ComIDs

        },
        error=function(error){
          feature.list <- NULL
          return.catchments <- NULL
        })

      } else {
        feature.list <- NULL
        return.catchments <- NULL
      }
    } else {
      feature.list <- NULL
      return.catchments <- NULL
    }

    # Return the 2 lists
    return(list(feature.list, return.catchments))
  }
  # Stop parallel operations
  future::plan(future::sequential)

  # Cast all shapes to multi-linestring for merging
  castLine <- function (data_in) {
    return(tryCatch(sf::st_cast(data_in, 'MULTILINESTRING'), error=function(e) NULL))
  }

  multi <- lapply(nhd.buffs[[1]], castLine)

  # First element to return: sf data.frame with the flowline
  feature.fl <- data.table::rbindlist(multi, idcol = T) %>%
    dplyr::rename(shape_ID = 1,
                  geometry = 2)

  feature.tog <- input_data %>%
    tibble::rowid_to_column('shape_ID') %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(feature.fl, by = 'shape_ID') %>%
    dplyr::select(-shape_ID) %>%
    sf::st_as_sf(crs = 4326)

  # Second element to return: list of comid vectors
  return.catchments <- nhd.buffs[[2]]

  return.me <- list(feature.tog, return.catchments)
  names(return.me) <- c('flowline_geoms',"nhd_comids")
  return(return.me)
}
