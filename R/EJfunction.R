#' EJ tool
#'
#' Main function that performs screening (land and water-based).
#' Input must be an SF object! User must make this transformation.
#'
#' @param data_type Required. Either "landbased" (coordinate locations) or "waterbased" (sf or catchments).
#' @param LOI_data Required. Location of interest. Locational data to undergo screening analysis.
#' @param input_type Required if data_type == "waterbased". Input must be "sf" object(s) or list of catchments (ComIDs)
#' @param buffer Distance(s) used to create buffers (miles). Default is 1, 3, and 5 miles for points and 0 miles for polygons.
#' @param threshold User specified threshold to represent potential concern. Default is 80%.
#' @param state User can restrict screening to particular states. Default is to screen for entire contiguous US.
#' @param ds_mode Set upstream ('UM','UT') or downstream ('DD','DM') flow direction for water-based screening. Default is 'DD'.
#' @param ds_dist Set distance upstream/downstream along flow path for water-based screening. Default is 50 miles.
#' @param input_name Vector of names for facilities
#' @param attains Option to return impairment data for flow path from ATTAINS database. Default is F.
#' @param working_dir
#'
#' @return
#' @export
#'
#' @examples
#'
#' #===============================================================================
#' #=====================FEATURE 1: LAND-BASED ANALYSIS============================
#' #===============================================================================
#' # Demonstration using meat and poultry plant facility information
#' # How it works:
#' # Provide lat lons to tool and specify data type (facility_latlons)
#' # options to consider
#' # 1) gis_option. Three options available: fast, robust, all.
#' #    Instersection is default.
#' # 2) buffer. Radius to use around facilities
#' # 3) Threshold for EJ consideration. EJScreen uses 80 as default.
#' # 4) states. Can restrict analysis to specific states.
#' # bring in data for contiguous US
#' a1 <- EJfunction(data_type="landbased", LOI_data = facilities, gis_option="fast",
#'                 buffer = 5)
#'
#' #===============================================================================
#' #=======================FEATURE 2: WATER-BASED ANALYSIS=========================
#' #===============================================================================
#' # Demonstration using random set of catchments
#' # How it works:
#' # Provide COMIDs to tool and specify data type (water_catchments)
#' # options to consider
#' # 1) ds.us.mode. Upstream/downstream option
#' #    mode DD is ds w/ diversions, DM is ds mainstem, UT is us w/ tributaries, UM is us mainstem.
#' #    Mode DD is default.
#' # 2) ds.us.dist. Upstream/downstream distance. 50miles is default.
#' # 3) buff.dist. Buffer distance around catchments in miles. 0 mile buffer is default.
#' # 4) Attains. Call attains API for data? (T/F). Default is False
#'
#' c <- EJfunction(data_type="waterbased", LOI_data=facilities,
#'                 input_type = 'sf', attains = F)
#'
EJfunction <- function(data_type, LOI_data, input_type = NULL,
                       buffer=NULL, threshold=NULL, state=NULL, ds_mode=NULL,
                       ds_dist=NULL, input_name=NULL, attains=NULL, 
                       working_dir=NULL){

  `%notin%` = Negate(`%in%`)

  #=============================================================================
  #------------------------------CHECK INPUTS-----------------------------------
  #=============================================================================
  #check to make sure data type is currently supported in tool
  if(data_type %notin% c("landbased", "waterbased")){
    stop("Data type not supported. Please specify one of the following data types:
         landbased OR waterbased.")
  }

  #check whether user-requested working directory exists
  if(!is.null(working_dir)){
    if(dir.exists(working_dir) == FALSE){
      stop("Working directory requested by user does not exist. Check directory name.")
    }
  } else {
    working_dir <- getwd()
  }

  ifelse(!dir.exists(file.path(working_dir,str_remove_all(Sys.time(),":"))),
         dir.create(file.path(working_dir,str_remove_all(Sys.time(),":"))), FALSE)

  output_path <- file.path(working_dir,str_remove_all(Sys.time(),":"))

  #If conducting waterbased analysis, need to know input type
  if(data_type=="waterbased"){
    if(input_type %notin% c("sf", "catchment")){
      stop("Input type not supported. Please specify one of the following data types:
         sf OR catchment.")
    } else {
      in.type <- input_type
    }

    # Convert list to data.frame if catchmentIDs provided.
    if(in.type == 'catchment'){
      if (!is.null(input_name)){
        stop("When input_type == 'catchment', provision of an input_name is not permitted. ComID serves as the identifying name. Please set input_name = NULL.")
      }
      LOI_data <- as.data.frame(LOI_data)
      names(LOI_data) <- 'catchment_ID'
    }
  } else {
    in.type <- 'sf'
  }

  #set threshold
  if(is.null(threshold)){
    Thresh <-  80 #default values
  } else {
    Thresh <- threshold   #user inputted values that override default
  }

  #=============================================================================
  #-------------------------------CHECK DATA------------------------------------
  #=============================================================================

  # Create internal function facility ID (in case user doesn't)
  if(data_type == 'waterbased' & in.type == 'catchment') {
    LOI_data <- LOI_data %>%
      tibble::rowid_to_column("shape_ID")
    facility_name <- LOI_data
  } else {
    if((class(LOI_data)!="sf")[1]){
      stop("User must provide a spatial object. Please check to make sure to convert the input into a sf object using st_as_sf().
           See Vignette for more information.")
    }
    LOI_data <- LOI_data %>%
      tibble::rowid_to_column("shape_ID")
  }

  # Create internal facility name mapping (if provided by user)
  if (!is.null(input_name)){
    if(input_name %notin% colnames(LOI_data)){
      stop('Input_name must be a variable in LOI_data.')
    }
    facility_name <- LOI_data %>%
      as.data.frame() %>%
      dplyr::select(all_of(input_name),shape_ID, -geometry)
  }

  # Determine most common geometry type in the input sf dataframe
  if (in.type != 'catchment'){
    facil.geom.type <- unique(as.character(st_geometry_type(LOI_data)))
    facil.geom.type <- facil.geom.type[which.max(tabulate(match(st_geometry_type(LOI_data), facil.geom.type)))]
  }

  #=============================================================================
  #-------------------------------BRING IN DATA---------------------------------
  #=============================================================================

  # Bring in EJ Screen Data
  if ("data.state.uspr" %in% ls(envir = .GlobalEnv)) {
    data <- get("data.state.uspr", envir = .GlobalEnv)
    if(!is.null(state)){
      data.state.uspr <- data %>%
        filter(ST_ABBREV %in% state)
    } else {
      data.state.uspr <- data
    }
  } else {
    data.state.uspr <- fetch_data_ej(year = data_year, state)
    assign("data.state.uspr", data.state.uspr, envir=globalenv())
  }
  
  # Bring in ACS Data
  if ("acs.cbg.data" %in% ls(envir = .GlobalEnv)) {
    data <- get('acs.cbg.data', env = .GlobalEnv)
    acs.cbg.data <- data
  } else {
    acs.cbg.data <- fetch_acs_data(year = (data_year),state)
    assign('acs.cbg.data', acs.cbg.data, envir=globalenv())
  }
  
  # Join EJSCREEN + ACS Data
  data.tog <- data.state.uspr %>% 
    dplyr::left_join(acs.cbg.data, by = c('ID' = 'GEOID')) %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(ID = as.numeric(ID))
  
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
  #For each data type, make sure GIS methods make sense.
  if(data_type=="landbased"){

    # Determine most common geometry type in the input sf dataframe
    facil.geom.type <- unique(as.character(sf::st_geometry_type(LOI_data)))
    facil.geom.type <- facil.geom.type[which.max(tabulate(match(sf::st_geometry_type(LOI_data), facil.geom.type)))]

    #set radii to draw around areas/points of interest
    if(is.null(buffer) &
       facil.geom.type %in% c('POINT','LINESTRING','MULTIPOINT','MULTILINESTRING')){
      buffers <-  c(1,3,5)  #default values: points
    } else if(is.null(buffer) &
              facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
      buffers <- 0 #default value: polygons
    } else if(facil.geom.type %notin% c('POINT','LINESTRING','MULTIPOINT',
                                        'MULTILINESTRING','POLYGON', 'MULTIPOLYGON')){
      stop('All geometries must be (multi-) points, lines, or polygons.')
    } else {
      buffers <- buffer  #user inputted values that override default
    }

    #create empty lists to store lists/DFs/DTs
    EJ.list.data <- list()
    EJ.index.data <- list()
    EJ.demographics.data <- list()
    EJ.corrplots.data <- list()
    EJ.facil.data <- list()

    for(i in buffers){
      print(message(paste0('Calculating for buffer distance: ', i, ' mi...\n')))

      #if lat-lons provided, draw buffers around points
      #if polygon provided, default is to use polygon without buffer but can add buffer if desired
      if(facil.geom.type %in% c('POINT','LINESTRING','MULTIPOINT','MULTILINESTRING')){
        if (i > 0){
          facility_buff <- st_dynamic_buffer(LOI_data, buff_dist = i)
        } else {
          stop('Buffer around points required.')
        }
      } else if(facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
        if (i > 0){
          facility_buff <- st_dynamic_buffer(LOI_data, buff_dist = i)
        } else if (i == 0) {
          facility_buff <- LOI_data
        } else {
          stop('Buffer distance(s) must be numeric and non-negative.')
        }
      }
      
      area_intersection <- facility_buff %>%
        sf::st_join(data.tog, join=st_intersects)
      
      cb.sf <- block.data %>%
        dplyr::filter(as.character(bg_id) %in% 
                        as.character(unique(area_intersection$ID))) %>%
        dplyr::group_by(bg_id) %>%
        dplyr::mutate(bg_pop = sum(pop)) %>%
        sf::st_as_sf(coords = c('longitude','latitude'), crs = 4326)
      
      area_cb_intersect <- facility_buff %>%
        sf::st_join(cb.sf, join = st_intersects) %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(bg_id, shape_ID) %>%
        dplyr::summarise(POP_inbuffer = sum(pop)/mean(bg_pop), .groups = 'drop') %>%
        dplyr::mutate_at(vars(POP_inbuffer), ~replace(., is.nan(.), 0)) %>%
        dplyr::mutate(bg_id = as.character(bg_id))
      
      
      area_together <- area_intersection %>% 
        dplyr::mutate(ID = as.character(ID)) %>%
        left_join(area_cb_intersect, by = c('shape_ID' = 'shape_ID', 
                                            'ID' = 'bg_id')) %>%
        as.data.table()
      
      #stopped here.

      area_intersection <- sf::st_intersection(facility_buff, data.tog) %>%
        dplyr::mutate(area_geo = sf::st_area(geometry)) %>%
        dplyr::mutate(percent_area = area_geo/area_bg*100) %>%
        dplyr::select(-geometry) %>%
        as.data.frame()
      
      # Trim down list.data to key variables.
      list.keep <- c('shape_ID', 'ID', 'STATE_NAME', 'ST_ABBREV', 'ACSTOTPOP',
                     'PM25', 'OZONE', 'DSLPM', 'CANCER', 'RESP', 'PTRAF', 'PNPL', 'PRMP',
                     'PRE1960PCT', 'PTSDF', 'PWDIS', 'VULEOPCT', 'MINORPCT', 'LOWINCPCT',
                     'UNDER5PCT', 'LESSHSPCT', 'OVER64PCT', 'LINGISOPCT',
                     'med_inc', 'frac_white', 'frac_black', 'frac_amerind',
                     'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov50', 'frac_pov99')
      
      temp_intersect <- area_intersection %>%
        dplyr::select(-contains('_D2_')) %>%
        dplyr::select(any_of(list.keep), starts_with('P_')) %>%
        dplyr::mutate(across(c('med_inc', 'frac_white', 'frac_black', 'frac_amerind',
                               'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov50',
                               'frac_pov99'),
                             list(~round(ecdf(acs.cbg.data %>%
                                                as.data.frame() %>%
                                                dplyr::select(cur_column()) %>%
                                                unlist() %>%
                                                as.numeric())(.)*100
                                         ,0)),
                             .names="P_{.col}_US"))
      
      print(message('Computing percentiles...'))
      # State percentiles
      states <- na.omit(unique(temp_intersect$ST_ABBREV))
      temp_state <- lapply(states, function(x){
        temp_intersect2 <- temp_intersect %>%
          dplyr::filter(ST_ABBREV==x) %>%
          dplyr::filter(!is.na(shape_ID))  %>%
          dplyr::mutate(across(c('med_inc', 'frac_white', 'frac_black', 'frac_amerind',
                                 'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov50',
                                 'frac_pov99'),
                               list(~round(ecdf(na.omit(acs.cbg.data %>%
                                                          as.data.frame() %>%
                                                          dplyr::filter(state==x) %>%
                                                          dplyr::select(cur_column())) %>%
                                                  unlist() %>%
                                                  as.numeric())(.)*100
                                           ,0)),
                               .names="P_{.col}_state"))
      })
      
      #Merge together, join back to facility data
      temp_intersect <- data.table::rbindlist(temp_state) %>%
        dplyr::left_join(if(class(LOI_data)[1]=="sf"){
          LOI_data %>%
            sf::st_drop_geometry()
        } else {
          LOI_data
        },
        by = 'shape_ID')
      
      EJ.list.data[[j]] <- temp_intersect
      names(EJ.list.data)[j] = paste0("area3_robust_radius",i,"mi")
      
      
      
      EJ.index.data[[paste0("Indexes_robust_radius",i,"mi")]] <-
        EJIndexes(area3_intersection, gis_method="robust" , buffer=i, threshold=Thresh, directory = output_path)
      EJ.demographics.data[[paste0("demographics_robust_radius",i,"mi")]] <-
        EJdemographics(area3_intersection, gis_method="robust" , buffer=i, threshold=Thresh, directory = output_path)
      EJ.corrplots.data[[paste0("corrplots_robust_radius",i,"mi")]] <-
        EJCorrPlots(area3_intersection, gis_method ="robust" , buffer=i, threshold=Thresh, directory = output_path)
      
      ### Areal apportionment using circular buffers around facilities
      # Extract the state associated with each facility
      state.shapes <- tigris::states() %>% st_as_sf() %>%
        st_transform(crs="ESRI:102005") %>%
        dplyr::select('NAME') %>%
        rename(facility_state = NAME)
      facility_buff <- st_join(facility_buff, state.shapes, join=st_intersects, largest = T)
      
      rm(state.shapes)
      
      if (!is.null(input_name)) {
        EJ.facil.data[[paste0('facil_robust_radius',i,'mi')]] <-
          areal_apportionment(ejscreen_bgs_data = data.tog,
                              facility_buff = facility_buff,
                              facil_data = LOI_data,
                              path_raster_layer = raster_data,
                              thrshld = Thresh) %>%
          dplyr::inner_join(facility_name, by = 'shape_ID') %>%
          dplyr::relocate(input_name)
      } else {
        EJ.facil.data[[paste0('facil_robust_radius',i,'mi')]] <-
          areal_apportionment(ejscreen_bgs_data = data.tog,
                              facility_buff = facility_buff,
                              facil_data = LOI_data,
                              path_raster_layer = raster_data,
                              thrshld = Thresh)
      }
    }

    # Clean up table
    EJ.list.data <- Filter(Negate(is.null), EJ.list.data)
    EJ.facil.data <- Filter(Negate(is.null), EJ.facil.data)

    # Drop unnecessary lists for final output.
    rm(EJ.demographics.data, EJ.index.data, EJ.corrplots.data)

    # Return these objects as functional output
    return.me <- sapply(objects(pattern="^EJ", envir = environment()),get, envir = environment(), simplify=F, USE.NAMES=T)
    return.me <- return.me[unlist(lapply(return.me,class))!="function"]

    return(return.me)

    #--------------------------------------------------------------------------#
    #--------------------------------------------------------------------------#
    #--------------------------------------------------------------------------#
  } else if(data_type=="waterbased") {

    ## Can come back and add all option later if demand exists.
    if(gis_option == 'all'){
      stop('Please choose ONLY ONE of (fast, robust) for water-based analysis.')
    }

    # Set Upstream/downstream option
    if(is.null(ds_mode)){
      ds.us.mode <- 'DD'  #default value
    } else {
      ds.us.mode <-  ds_mode  #user inputted values that overrides default
    }

    # Distances: all in miles
    # set distance Upstream/downstream option
    if(is.null(ds_dist)){
      ds.us.dist <- 50  #default value
    } else {
      ds.us.dist <-  ds_dist  #user inputted values that overrides default
    }

    # Set buffer distance
    if(is.null(buffer)){
      buffer <- 1  #default value
    } else {
      buffer <-  buffer  #user inputted values that overrides default
    }

    #ATTAINS
    if(is.null(attains)){
      attains.check <- F  #default value
    } else {
      attains.check <-  attains  #user inputted values that overrides default
    }

    EJ.demographics.data <- list()
    EJ.corrplots.data <- list()
    EJ.facil.data <- list()
    EJ.list.data <- list()
    EJ.index.data <- list()
    EJ.buffer.shapes <- list()
    EJ.attains.data <- list()


    for (i in buffer){
      ## This returns:
      #(1) shape of downstream buffered area
      #(2) full return from attains API
      #(3) summary of attains data
      #(4) if input is catchment#, the lat/lon coords of segment centroid
      print(message(paste0('Calculating for buffer distance: ', i)))
      catchment.polygons <- EJWaterReturnCatchmentBuffers(LOI_data, ds.us.mode, ds.us.dist,
                                                          i, in.type, attains.check)

      if (in.type == 'sf') {
        catch.facil.data <- catchment.polygons[[1]] %>%
          as.data.frame() %>%
          inner_join(dplyr::select(as.data.frame(LOI_data), -geometry),
                     by = 'shape_ID') %>%
          st_as_sf()
      } else {
        catch.facil.data <- catchment.polygons[[1]] %>%
          st_as_sf()
      }

      #############
      ## This section intersects/contains facility buffered areas and CBGs
      area <- catchment.polygons[[1]] %>%
        sf::st_join(data.tog, join = st_intersects) %>%
        dplyr::filter(!is.na(shape_ID)) %>%
        sf::st_drop_geometry()

      # Trim down list.data to key variables.
      list.keep <- c('shape_ID', 'ID', 'STATE_NAME', 'ST_ABBREV', 'ACSTOTPOP',
                     'PM25', 'OZONE', 'DSLPM', 'CANCER', 'RESP', 'PTRAF', 'PNPL', 'PRMP',
                     'PRE1960PCT', 'PTSDF', 'PWDIS', 'VULEOPCT', 'MINORPCT', 'LOWINCPCT',
                     'UNDER5PCT', 'LESSHSPCT', 'OVER64PCT', 'LINGISOPCT',
                     'med_inc', 'frac_white', 'frac_black', 'frac_amerind',
                     'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov50', 'frac_pov99')

      temp_intersect <- area %>%
        dplyr::select(-contains('_D2_')) %>%
        dplyr::select(list.keep, starts_with('P_')) %>%
        dplyr::mutate(across(c('med_inc', 'frac_white', 'frac_black', 'frac_amerind',
                               'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov50',
                               'frac_pov99'),
                             list(~round(ecdf(acs.cbg.data %>%
                                                as.data.frame() %>%
                                                dplyr::select(cur_column()) %>%
                                                unlist() %>%
                                                as.numeric())(.)*100
                                         ,0)),
                             .names="P_{.col}_US"))

      # State percentiles
      states <- na.omit(unique(temp_intersect$ST_ABBREV))
      temp_state <- lapply(states, function(x){
        temp_intersect2 <- temp_intersect %>%
          dplyr::filter(ST_ABBREV==x) %>%
          dplyr::filter(!is.na(shape_ID))  %>%
          dplyr::mutate(across(c('med_inc', 'frac_white', 'frac_black', 'frac_amerind',
                                 'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov50',
                                 'frac_pov99'),
                               list(~round(ecdf(na.omit(acs.cbg.data %>%
                                                          as.data.frame() %>%
                                                          dplyr::filter(state==x) %>%
                                                          dplyr::select(cur_column())) %>%
                                                  unlist() %>%
                                                  as.numeric())(.)*100
                                           ,0)),
                               .names="P_{.col}_state"))
      })

      #Merge together, join back to facility data
      temp_intersect <- data.table::rbindlist(temp_state) %>%
        dplyr::left_join(if(class(LOI_data)[1]=="sf"){
                            LOI_data %>%
                              sf::st_drop_geometry()
                          } else {
                            LOI_data
                          },
                         by = 'shape_ID')

      EJ.list.data[[paste0('area1_',gis_option,'_radius',i,'mi')]] <- temp_intersect

      EJ.index.data[[paste0("Indexes_",gis_option,"_buffer",i,"mi")]] <-
        EJIndexes(area, gis_method = gis_option, buffer=i, threshold=Thresh, directory = output_path)

      EJ.demographics.data[[paste0("demographics_",gis_option,"_buffer",i,"mi")]] <-
        EJdemographics(area, gis_method = gis_option, buffer=i, threshold=Thresh, directory = output_path)

      tryCatch({
        EJ.corrplots.data[[paste0("corrplots_",gis_option,"_buffer",i,"mi")]] <-
          EJCorrPlots(area, gis_method = gis_option , buffer=i, threshold=Thresh,
                      directory = output_path)
      },
      error=function(error){
        #print(error)
        EJ.corrplots.data[[paste0("corrplots_",gis_option,"_buffer",i,"mi")]] <- NULL
      })

      #############
      ## This returns facility level summaries for
      if(gis_option %in% c('fast')){
        if (in.type == 'sf'){
          if (!is.null(input_name)) {
            EJ.facil.data[[paste0('facil_',gis_option,'_radius',i,'mi')]] <-
              EJFacilLevel(list_data = area,
                           facil_data = st_transform(LOI_data, crs = 4326),
                           ejscreen_data = data.state.uspr,
                           acs_data = acs.cbg.data,
                           thrshld = Thresh) %>%
              dplyr::inner_join(facility_name, by = 'shape_ID') %>%
              dplyr::relocate(input_name)
          } else {
            EJ.facil.data[[paste0('facil_',gis_option,'_radius',i,'mi')]] <-
              EJFacilLevel(list_data = area,
                           facil_data = st_transform(LOI_data, crs = 4326),
                           ejscreen_data = data.state.uspr,
                           acs_data = acs.cbg.data,
                           thrshld = Thresh)
          }
        } else if (in.type == 'catchment'){
          temp.mat <- as.data.frame(catchment.polygons$catchment_state)  %>%
            mutate(comid = as.numeric(comid)) %>%
            inner_join(LOI_data, by = c('comid' = 'catchment_ID')) %>%
            st_as_sf()

          EJ.facil.data[[paste0('facil_',gis_option,'_radius',i,'mi')]] <-
            EJFacilLevel(list_data = area,
                         facil_data = st_transform(temp.mat, crs = 4326),
                         ejscreen_data = data.state.uspr,
                         acs_data = acs.cbg.data,
                         thrshld = Thresh) %>%
            dplyr::inner_join(facility_name, by = 'shape_ID') %>%
            dplyr::relocate(catchment_ID)

          rm(temp.mat)
        }

        ## AREAL APPORTIONMENT for user-selected buffer around stream from facility
        ## This yields pop-weighted average data for a given facility
      } else if(gis_option == 'robust'){

        state.shapes <- tigris::states() %>% st_as_sf() %>%
          st_transform(crs="ESRI:102005") %>%
          dplyr::select('NAME') %>%
          rename(facility_state = NAME)

        if(in.type == 'sf'){
          facility_buff <- st_join(LOI_data, state.shapes, join=st_intersects, largest = T) %>%
            dplyr::select(shape_ID, facility_state) %>%
            st_drop_geometry() %>%
            inner_join(catchment.polygons[[1]], by = 'shape_ID') %>%
            st_as_sf()

          if (!is.null(input_name)) {
            EJ.facil.data[[paste0('facil_',gis_option,'_radius',i,'mi')]] <-
              EJ.facil.data[[paste0('facil_robust_radius',i,'mi')]] <-
              areal_apportionment(ejscreen_bgs_data = data.tog,
                                  facility_buff = facility_buff,
                                  facil_data = LOI_data,
                                  path_raster_layer = raster_data,
                                  thrshld = Thresh) %>%
              dplyr::inner_join(facility_name, by = 'shape_ID') %>%
              dplyr::relocate(input_name)
          } else {
            EJ.facil.data[[paste0('facil_',gis_option,'_radius',i,'mi')]] <-
              EJ.facil.data[[paste0('facil_robust_radius',i,'mi')]] <-
              areal_apportionment(ejscreen_bgs_data = data.tog,
                                  facility_buff = facility_buff,
                                  facil_data = LOI_data,
                                  path_raster_layer = raster_data,
                                  thrshld = Thresh)
          }

        } else if (in.type == 'catchment') {

          ## Shapefile for downstream (/upstream?) buffer
          facility_buff <- catchment.polygons[[5]] %>%
            mutate(comid = as.numeric(comid)) %>%
            inner_join(LOI_data, by = c('comid' = 'catchment_ID')) %>%
            dplyr::select(shape_ID, facility_state) %>%
            st_drop_geometry() %>%
            inner_join(catchment.polygons[[1]], by = 'shape_ID') %>%
            st_as_sf()

          ## Shapefile with lat/lon of catchmentID waterbody centroid
          temp.mat <- as.data.frame(catchment.polygons$catchment_state) %>%
            mutate(comid = as.numeric(comid)) %>%
            inner_join(LOI_data, by = c('comid' = 'catchment_ID')) %>%
            st_as_sf() %>%
            st_transform(crs = 4326)

          EJ.facil.data[[paste0('facil_robust_radius',i,'mi')]] <-
            areal_apportionment(ejscreen_bgs_data = data.tog,
                                facility_buff = facility_buff,
                                facil_data = temp.mat,
                                path_raster_layer = raster_data,
                                thrshld = Thresh) %>%
              dplyr::inner_join(facility_name, by = 'shape_ID') %>%
              dplyr::relocate(catchment_ID)
        }
        rm(state.shapes)

      }

      if (attains.check == T){
        EJ.buffer.shapes[[paste0('buffer_shape_radius',i,'mi')]] <-
          inner_join(catchment.polygons[[1]], catchment.polygons[[4]],
                     by = c('shape_ID' = '.id'))
        EJ.attains.data[[paste0('attains_raw_radius', i, 'mi')]] <-
          catchment.polygons[[3]]
      } else {
        EJ.buffer.shapes[[paste0('buffer_shape_radius',i,'mi')]] <-
          catchment.polygons[[1]]
      }
    }

    if(attains.check == F){
      return.me <- list(EJ.facil.data, EJ.list.data, EJ.buffer.shapes,
                        catchment.polygons[[2]])
      #EJ.demographics.data, EJ.corrplots.data, EJ.index.data,

      names(return.me) <- c('EJ.facil.data', 'EJ.list.data','EJ.buffer.summary',
                            'EJ.nhd.comids')
      #'EJ.demographics.data', 'EJ.corrplots.data','EJ.index.data',
    } else {
      return.me <- list(EJ.facil.data, EJ.list.data, EJ.buffer.shapes,
                        catchment.polygons[[2]], EJ.attains.data)
      names(return.me) <- c('EJ.facil.data', 'EJ.list.data', 'EJ.buffer.summary',
                            'EJ.nhd.comids','EJ.attainsdata.raw')
    }

    return(return.me)

  }

}
