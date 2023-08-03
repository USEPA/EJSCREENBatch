#' EJ tool
#'
#' Main function that performs screening (land and water-based).
#' Input must be an SF object! User must make this transformation.
#'
#' @param LOI_data Required. Location of interest. Locational data to undergo screening analysis.
#' @param buffer Distance(s) used to create buffers (miles). Default is 1, 3, and 5 miles for points and 0 miles for polygons.
#' @param state User can restrict screening to particular states. Default is to screen for entire contiguous US.
#' @param data_year Option for which year of EJSCREEN data to return (and corresponding ACS data). Default is 2023.
#'
#' @return
#' @export
#'
#' @examples
#'
EJfunction <- function(LOI_data,
                       buffer=NULL, 
                       state=NULL, 
                       data_year = NULL){

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
    tibble::rowid_to_column("shape_ID")

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
  }
  
  # Bring in Census Block centroids
  if ("block.data" %in% ls(envir = .GlobalEnv)){
    data <- get('block.data', env = .GlobalEnv)
    block.data <- data
  } else {
    block.data <- fetch_blockcents(data_year)
    assign('block.data', block.data, envir = globalenv())
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
    
    message('Performing spatial intersections.')
    #intersect buffer area with CBG-level EJSCREEN data
    area.intersection <- facility_buff %>%
      sf::st_join(data.tog,
                  join=sf::st_intersects)

    #filter Cblock list down only to relevant based on intersected CBGs
    cb.sf <- block.data %>%
      dplyr::filter(as.character(bg_id) %in% 
                      as.character(unique(area.intersection$ID))) %>%
      dplyr::group_by(bg_id) %>%
      dplyr::mutate(bg_pop = sum(pop)) %>%
      sf::st_as_sf(coords = c('longitude','latitude'), crs = 4326)
    
    #intersect Cblocks with buffer area
    #could this be parallelized to speed things up?!
    cb.intersect <- facility_buff %>%
      sf::st_join(cb.sf, join = sf::st_intersects) %>%
      sf::st_drop_geometry()

    #########################################
    ## 1. RETURN FACILITY LEVEL DATA SUMMARY
    #sum pop in buffer by facil + CBG
    message('Calculating within-buffer apportionment % for LOIs...')
    area_cb_intersect <- cb.intersect %>%
      dplyr::group_by(bg_id, shape_ID) %>%
      dplyr::summarise(POP_inbuffer = sum(pop)/mean(bg_pop), .groups = 'drop') %>%
      dplyr::mutate_at(dplyr::vars(POP_inbuffer), ~replace(., is.nan(.), 0)) %>%
      dplyr::mutate(bg_id = as.character(bg_id))
    
    #merge Cblock pop counts in buffer with demographics at CBG resolution
    area_together <- area.intersection %>% 
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::left_join(area_cb_intersect, by = c('shape_ID' = 'shape_ID', 
                                          'ID' = 'bg_id')) %>%
      data.table::as.data.table()
    
    #columns to keep in the facility level table
    colsToKeep <- c(ejvarlist[[1]][-(1:4)],
                    'med_inc','frac_white', 'frac_black', 'frac_amerind',
                    'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov99', 'frac_pov199', 
                    'POP_sum')
    
    #function to calculate modal value for LOI's state assignment (for %iles)
    #? for future: how to best treat buffer areas that span multiple states?
    Modal <- function(x) {
      val <- unique(x)
      return(val[which.max(tabulate(match(x, val)))])
    }
    
    message('Creating LOI-level summary table.')
    #clean tables... weighted average of values at the LOI level.
    loi_clean <- area_together[is.na(POP_inbuffer), POP_inbuffer := 0L
                                 ][, POP_sum := round(sum(POP_inbuffer*ACSTOTPOP)),
                                   by = shape_ID
                                   ][, ST_ABB := Modal(ST_ABBREV),
                                       by = shape_ID
                                       ][, lapply(.SD, weighted.mean, w=POP_inbuffer*ACSTOTPOP, na.rm = T),
                                         by = list(shape_ID, ST_ABB),
                                         .SDcols = colsToKeep]
    
    #calculate percentiles using the raw data distributions
    message('Computing LOI-level percentiles...')
    
    #national percentiles
    temp_intersect <- loi_clean %>%
      dplyr::select(dplyr::any_of(c('shape_ID','ST_ABB',colsToKeep))) %>%
      dplyr::mutate(dplyr::across(colsToKeep[-length(colsToKeep)],
                           list(~round(ecdf(data.tog %>%
                                              sf::st_drop_geometry() %>%
                                              as.data.frame() %>%
                                              dplyr::select(cur_column()) %>%
                                              unlist() %>%
                                              as.numeric())(.)*100
                                       ,0)),
                           .names="P_{.col}_US"))
    
    #state percentiles
    states <- na.omit(unique(temp_intersect$ST_ABB))
    temp_state <- lapply(states, function(x){
      temp_intersect %>%
        dplyr::filter(ST_ABB==x) %>%
        dplyr::filter(!is.na(shape_ID))  %>%
        dplyr::mutate(dplyr::across(colsToKeep[-length(colsToKeep)],
                             list(~round(ecdf(na.omit(data.tog %>%
                                                        sf::st_drop_geometry() %>%
                                                        as.data.frame() %>%
                                                        dplyr::filter(ST_ABBREV==x) %>%
                                                        dplyr::select(cur_column())) %>%
                                                unlist() %>%
                                                as.numeric())(.)*100
                                         ,0)),
                             .names="P_{.col}_state"))
    })
    
    #merge together, join back to facility data
    EJ.loi.data[[i]] <- LOI_data %>%
      dplyr::left_join(
        data.table::rbindlist(temp_state),
        by = 'shape_ID'
      )
    names(EJ.loi.data)[i] = paste0("LOI_radius_",ejvarlist[[3]],"_",buffers[i],"mi")
    
    #clear some memory
    rm(temp_state, states, temp_intersect, loi_clean,
       area_together, area_cb_intersect)
    
    ####################################
    ## 2. RETURN CBG LEVEL DATA SUMMARY
    #sum pop in buffer across all facilities.
    message('Calculating within-buffer apportionment % for CBGs...')
    all_cb_intersect <- cb.intersect %>%
      dplyr::select(block_id, pop, bg_id, bg_pop) %>%
      dplyr::distinct() %>%
      dplyr::group_by(bg_id) %>%
      dplyr::summarise(POP_inbuffer = sum(pop)/mean(bg_pop), .groups = 'drop') %>%
      dplyr::mutate_at(dplyr::vars(POP_inbuffer), ~replace(., is.nan(.), 0))
    
    allColsToKeep <- c(ejvarlist[[1]],
                       paste0("P_",ejvarlist[[1]][-(1:4)],"_US"),
                       paste0("P_",ejvarlist[[1]][-(1:4)],"_state"),
                       'med_inc','frac_white', 'frac_black','frac_amerind',
                       'frac_asian','frac_pacisl','frac_hisp','frac_pov99','frac_pov199')
    
    message('Creating CBG-level summary table.')
    all_together <- area.intersection %>% 
      sf::st_drop_geometry() %>%
      dplyr::select(dplyr::any_of(c(allColsToKeep))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      dplyr::inner_join(all_cb_intersect %>%
                          dplyr::mutate(bg_id = as.character(bg_id)), 
                        by = c('ID' = 'bg_id')) %>%
      dplyr::mutate(POP_sum = round(POP_inbuffer*ACSTOTPOP)) %>%
      dplyr::select(-c('POP_inbuffer','ACSTOTPOP')) %>%
      dplyr::filter(POP_sum > 0) %>%
      as.data.table()
    
    #calculate percentiles using the raw data distributions
    message('Computing CBG-level percentiles...')
    
    #national percentiles
    temp_all <- all_together %>%
      dplyr::mutate(dplyr::across(c('med_inc','frac_white', 'frac_black','frac_amerind',
                                    'frac_asian','frac_pacisl','frac_hisp','frac_pov99','frac_pov199'),
                                  list(~round(ecdf(data.tog %>%
                                                     sf::st_drop_geometry() %>%
                                                     as.data.frame() %>%
                                                     dplyr::select(cur_column()) %>%
                                                     unlist() %>%
                                                     as.numeric())(.)*100
                                              ,0)),
                                  .names="P_{.col}_US"))
    
    #state percentiles
    states <- na.omit(unique(temp_all$ST_ABBREV))
    temp_state <- lapply(states, function(x){
       temp_all %>%
        dplyr::filter(ST_ABBREV==x) %>%
        dplyr::mutate(across(c('med_inc','frac_white', 'frac_black','frac_amerind',
                               'frac_asian','frac_pacisl','frac_hisp','frac_pov99','frac_pov199'),
                             list(~round(ecdf(na.omit(data.tog %>%
                                                        sf::st_drop_geometry() %>%
                                                        as.data.frame() %>%
                                                        dplyr::filter(ST_ABBREV==x) %>%
                                                        dplyr::select(cur_column())) %>%
                                                unlist() %>%
                                                as.numeric())(.)*100
                                         ,0)),
                             .names="P_{.col}_state"))
    })
    
    #merge together, join back to facility data
    EJ.cbg.data[[i]] <- data.table::rbindlist(temp_state)
    names(EJ.cbg.data)[i] = paste0("CBG_radius_",ejvarlist[[3]],"_",buffers[i],"mi")
  }
  # Clean up table
  EJ.cbg.data <- Filter(Negate(is.null), EJ.cbg.data)
  EJ.loi.data <- Filter(Negate(is.null), EJ.loi.data)
  
  # Return these objects as functional output
  return.me <- sapply(objects(pattern="^EJ", envir = environment()), 
                      get, envir = environment(), simplify=F, USE.NAMES=T)
  return.me <- return.me[unlist(lapply(return.me,class))!="function"]
  
  return(return.me)
}
