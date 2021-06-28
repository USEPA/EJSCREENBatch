#' EJ Return Water Catchment Buffers
#'
#' This function returns:
#' (1) a SF data.frame containing user-specified buffers around catchments
#' (2) a data.table containing raw ATTAINs API data (OPTIONAL)
#'
#' @param input_data
#' @param ds_us_mode Option for upstream or downstream. Default is downstream.
#' @param ds_us_dist Distance up/downstream. Default is 50 miles.
#' @param buff_dist Distance in miles to buffer out. Default is 1 mile.
#' @param input_type Type of data inputted. Options limited to 'sf' and 'catchment'
#' @param attains
#'
#' @return
#' @export
#'
#' @examples

EJWaterReturnCatchmentBuffers <-  function(input.data, ds_us_mode, ds_us_dist, buff_dist, input_type, attains){
  # Determine the input.data type:
  # (in future could have this determine object type (sf, numeric list, etc.) without user input)
  if (input_type == 'sf'){
    input.data <- st_transform(input.data, crs = 4326)
    feature.id <- vector(mode = "list", length = dim(input.data)[1])
    for (i in 1:dim(input.data)[1]){
      feature.id[[i]] <- tryCatch(
        {
        nhdplusTools::get_nhdplus(AOI = input.data[i,],
                                     realization = 'catchment')$featureid
        },
        error=function(cond) {
          message("Note: is input a sf data.frame with all obs in continental US?")
          message(paste0("Original error message: ", cond))
          # Choose a return value in case of error
          return(as.integer(1))
        },
        warning=function(cond) {
          message("Original warning message:")
          message(cond)
          return(as.integer(1))
        }
        )
    }
    feature.id <- as.numeric(feature.id)
  } else if (input_type == 'catchment'){
    # List of catchments
    feature.id <- input.data$V1

    # State shapefile for matching to start catchment
    state.shapes <- spData::us_states %>% st_as_sf() %>%
      st_transform(crs="ESRI:102005") %>%
      dplyr::select('NAME') %>%
      rename(facility_state = NAME)

    # Loop through catchmentIDs and extract centroid lat/lon of waterbody
    hold.me <- vector(mode = 'list', length = length(input.data))
    for (k in 1:dim(input.data)[1]){
      hold.me[[k]] <- get_nldi_feature(list(featureSource = 'comid', featureID = feature.id[k])) %>%
        st_centroid() %>%
        st_transform(crs='ESRI:102005') %>%
        st_join(state.shapes, join=st_intersects) %>%
        dplyr::select(comid, facility_state, geometry)
    }
    hold.together <- do.call(rbind, hold.me)
    #feature.id <- hold.together$comid
  }

  # Loop through catchment IDs to extract down/upstream buffer polygons
  geo.base <- 'https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3' #For ATTAINS API
  feature.list <- vector(mode = "list", length = length(feature.id))
  nhd.catchment <- vector(mode = 'list', length = length(feature.id))
  for (i in 1:length(feature.id)){
    nldi.feature <- list(featureSource = 'comid', featureID = feature.id[i])
    if(length(get_nldi_feature(nldi.feature)) > 0){
      nldi.temp <- navigate_nldi(nldi.feature,
                                 mode = ds_us_mode,
                                 distance_km = round(ds_us_dist*1.60934))$DD_flowlines
      feature.list[[i]] <- nldi.temp  %>%
        st_union() %>%
        st_transform("ESRI:102005") %>%
        st_buffer(dist = set_units(buff_dist,"mi")) %>%
        st_as_sf

      # Call ATTAINs database on all down/upstream catchments
      if (attains == T) {
        sql.statement <- sql_where(NHDPlusID = as.numeric(nldi.temp$nhdplus_comid), rel_op = "IN")
        nhd.catchment[[i]] <- get_spatial_layer(geo.base, where = sql.statement)
        if (dim(nhd.catchment[[i]])[1] < 1) {
          nhd.catchment[[i]] <- NULL
        }
      }
    } else {
      feature.list[[i]] <- NULL
    }
  }

  feature.buff <- rbindlist(feature.list, idcol = T) %>%
    mutate(start_catchment = feature.id[.id]) %>%
    rename(shape_ID = 1,
           geometry=x) %>%
    st_as_sf()

  if (attains == T){
    nhd.attains <- rbindlist(nhd.catchment, idcol = T)

    # this is slightly messy:
    # (since ATTAINS only returns geography at catchment, not assessment level)
    # take the max ATTAINs status for a given catchment, facility pair.
    summary.attains <- nhd.attains %>% as.data.table()
    summary.attains <- summary.attains[, .(.id, OBJECTID, nhdplusid, assessmentunitidentifier,
                 ircategory, areasqkm)
             ][, irflag := as.integer(substring(ircategory,1,1))]

    summary.attains <- summary.attains[summary.attains[, .I[which.max(irflag)], by = .(.id, nhdplusid)]$V1
                  ][, irflag := NULL
                    ][, total_area := sum(areasqkm, na.rm = T), by = .id
                      ][, unassess.temp := as.integer(ircategory == '3')
                        ][, unassess_area := sum(areasqkm * unassess.temp, na.rm = T), by = .id
                          ][, unassess.temp := NULL
                            ][, tmdl.comp.temp := as.integer(ircategory == '4A')
                              ][, tmdl_complete_area := sum(areasqkm * tmdl.comp.temp, na.rm = T), by = .id
        ][, tmdl.comp.temp := NULL
          ][, tmdl.4b4c.temp := as.integer(ircategory %in% c('4B', '4C'))
            ][, tmdl_4b4c_area := sum(areasqkm * tmdl.4b4c.temp, na.rm = T), by = .id
              ][, tmdl.4b4c.temp := NULL
                ][, listed.303d.temp := as.integer(ircategory == '5')
                  ][, listed_303d_area := sum(areasqkm * listed.303d.temp, na.rm = T), by = .id
        ][, listed.303d.temp := NULL
          ][, attainment_area := total_area - unassess_area - tmdl_complete_area -
              tmdl_4b4c_area - listed_303d_area
            ][, .(total_area, attainment_area, unassess_area, tmdl_complete_area, tmdl_4b4c_area,
                  listed_303d_area), by = .id] %>%
              unique() %>%
              mutate_if(is.numeric, round, digits = 3)
    if (input_type == 'catchment'){
      return.me <- list(feature.buff, nhd.attains, summary.attains, hold.together)
    } else {
      return.me <- list(feature.buff, nhd.attains, summary.attains, NULL)
    }
  } else {
    if (input_type == 'catchment'){
      return.me <- list(feature.buff, NULL, NULL, hold.together)
    } else {
      return.me <- list(feature.buff, NULL, NULL, NULL)
    }
  }
  names(return.me) <- c('buffer_geoms','attains_catchments', 'attains_summary',
                        'catchment_state')
  return(return.me)
}
