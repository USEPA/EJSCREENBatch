#' Primary function that performs EJ/demographic screening.
#'
#' @param LOI_data Required. Location of interest. Location data must be an SF object to undergo screening analysis.
#' @param data_year Option for which EJSCREEN (and corresponding ACS) data vintage to return. Default is 2023.
#' @param buffer Distance(s) used to create buffers (miles). Default is 1, 3, and 5 miles for points/linestrings and 0 miles for polygons.
#' @param raster Block or raster apportioning for buffer? Default is T. If set to false, uses EJSCREEN's block approach.
#' @param state User can restrict screening to particular states. Default is to screen for entire US.
#'
#' @import data.table
#' @import doRNG
#'
#' @return A list containing 2 sublists: (1) a LOI-buffer-level screening summary, and (2) a CBG-level screening summary for all block groups within affected communities.
#' @export
#'
#' @examples
#'
EJfunction <- function(LOI_data,
                       data_year = NULL,
                       buffer = NULL,
                       raster = T,
                       state = NULL){

  `%notin%` = Negate(`%in%`)

  #=============================================================================
  #------------------------------CHECK INPUTS-----------------------------------
  #=============================================================================
  #check to make sure data type is currently supported in tool
  if(!is(LOI_data,'sf')){
    stop('Input data must be an sf data.frame.')
  }

  #=============================================================================
  #-------------------------------CHECK DATA------------------------------------
  #=============================================================================

  # Create internal function facility ID (in case user doesn't)
  LOI_data <- LOI_data %>%
    tibble::rowid_to_column("shape_ID") %>%
    sf::st_transform(crs = 4326)

  # Determine most common geometry type in the input sf dataframe
  facil.geom.type <- unique(as.character(sf::st_geometry_type(LOI_data)))
  facil.geom.type <- facil.geom.type[which.max(tabulate(match(sf::st_geometry_type(LOI_data),
                                                              facil.geom.type)))]

  #For each data type, make sure GIS methods make sense.
  #set radii to draw around areas/points of interest
  if(is.null(buffer) &
     facil.geom.type %in% c('POINT','LINESTRING','MULTIPOINT','MULTILINESTRING')){
    buffers <-  c(1,3,5)  #default values: points, lines
  } else if(is.null(buffer) &
            facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
    buffers <- 0 #default value: polygons
  } else if(facil.geom.type %notin% c('POINT','LINESTRING','MULTIPOINT',
                                      'MULTILINESTRING','POLYGON', 'MULTIPOLYGON')){
    stop('All geometries must be (multi-) points, lines, or polygons.')
  } else {
    buffers <- buffer  #user inputted values that override default
  }

  sf::sf_use_s2(FALSE)

  #=============================================================================
  #-------------------------------BRING IN DATA---------------------------------
  #=============================================================================

  # Pull the appropriate variable list
  ejvarlist <- fetch_ejvars(year = data_year)

  # Bring in & join EJSCREEN and ACS data
  if ("data.tog" %in% ls(envir = .GlobalEnv)){
    data <- get('data.tog', env = .GlobalEnv)
    if(!is.null(state)){
      data.tog <- data %>%
        dplyr::filter(ST_ABBREV %in% state)
    } else {
      data.tog <- data
    }
  } else {
    acs.cbg.data <- fetch_acs_data(year = ejvarlist[[2]], state)
    data.state.uspr <- fetch_data_ej(year = data_year, state)
    data.tog <- data.state.uspr %>%
      dplyr::left_join(acs.cbg.data, by = c('ID' = 'GEOID')) %>%
      sf::st_transform(crs = 4326) %>%
      dplyr::mutate(ID = as.numeric(ID)) %>%
      dplyr::select(dplyr::any_of(c('shape_ID','ST_ABB',
                                    ejvarlist[[1]],
                                    paste0("P_",ejvarlist[[1]][-(1:4)],"_US"),
                                    paste0("P_",ejvarlist[[1]][-(1:4)],"_state"),
                                    'med_inc','frac_white', 'frac_black','frac_amerind',
                                    'frac_asian','frac_pacisl','frac_hisp','frac_pov99','frac_pov199')))
    assign("data.tog", data.tog, envir=globalenv())
    rm(acs.cbg.data, data.state.uspr)
  }

  #=============================================================================
  #---------------------SCREENING ANALYSES--------------------------------------
  #=============================================================================

  #create empty lists to store lists/DFs/DTs
  EJ.loi.data = vector(mode = 'list', length = length(buffers))
  EJ.cbg.data = vector(mode = 'list', length = length(buffers))

  for(i in 1:length(buffers)){
    message(paste0('Calculating for buffer distance: ', buffers[i], ' mi...\n'))

    #if lat-lons provided, draw buffers around points
    #if polygon provided, default is to use polygon without buffer but can add buffer if desired
    if(facil.geom.type %in% c('POINT','LINESTRING','MULTIPOINT','MULTILINESTRING')){
      if (buffers[i] > 0){
        facility_buff <- st_dynamic_buffer(LOI_data, buff_dist = buffers[i])
      } else {
        stop('Buffer around points required.')
      }
    } else if(facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
      if (buffers[i] > 0){
        facility_buff <- st_dynamic_buffer(LOI_data, buff_dist = buffers[i])
      } else if (buffers[i] == 0) {
        facility_buff <- LOI_data
      } else {
        stop('Buffer distance(s) must be numeric and non-negative.')
      }
    }

    #perform buffering using the Cblock methodology
    if (raster == F){
      blockbuffer <- EJSCREENBufferBlocks(dta_year = data_year,
                                          buff_dta = facility_buff,
                                          ejscreen_bgs_data = data.tog,
                                          ejvarlist = ejvarlist)

      #LOI data.frame
      #merge together, join back to facility data
      EJ.loi.data[[i]] <- LOI_data %>%
        dplyr::left_join(
          blockbuffer[[1]],
          by = 'shape_ID'
        )
      names(EJ.loi.data)[i] = paste0("LOI_radius_",ejvarlist[[3]],"_",buffers[i],"mi")

      #CBG data.frame
      #merge together, join back to facility data
      EJ.cbg.data[[i]] <- blockbuffer[[2]]
      names(EJ.cbg.data)[i] = paste0("CBG_radius_",ejvarlist[[3]],"_",buffers[i],"mi")

      rm(blockbuffer)
    } else {
      rasterbuffer <- EJSCREENBufferRaster(ejscreen_bgs_data = data.tog,
                                           facility_buff = facility_buff,
                                           ejvarlist = ejvarlist,
                                           year = data_year)

      #LOI data.frame
      #merge together, join back to facility data
      EJ.loi.data[[i]] <- LOI_data %>%
        dplyr::left_join(
          rasterbuffer[[1]],
          by = 'shape_ID'
        )
      names(EJ.loi.data)[i] = paste0("LOI_radius_",ejvarlist[[3]],"_",buffers[i],"mi")

      #CBG data.frame
      #merge together, join back to facility data
      EJ.cbg.data[[i]] <- rasterbuffer[[2]]
      names(EJ.cbg.data)[i] = paste0("CBG_radius_",ejvarlist[[3]],"_",buffers[i],"mi")

      rm(rasterbuffer)
    }


  }
  # Clean up table
  EJ.cbg.data <- Filter(Negate(is.null), EJ.cbg.data)
  EJ.loi.data <- Filter(Negate(is.null), EJ.loi.data)

  # Return these objects as functional output
  return.me <- sapply(objects(pattern="^EJ", envir = environment()),
                      get, envir = environment(), simplify=F, USE.NAMES=T)
  return.me <- return.me[unlist(lapply(return.me,class))!="function"]

  # Push.
  return(return.me)
}
