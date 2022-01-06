#' EJ tool
#'
#' Main function that performs screening (land and water-based).
#' Input must be an SF object! User must make this transformation.
#'
#' @param data_type Required. Either "landbased" (coordinate locations) or "waterbased" (sf or catchments).
#' @param LOI_data Required. Location of interest. Locational data to undergo screening analysis.
#' @param input_type Required if data_type == "waterbased". Input must be "sf" object(s) or list of catchments (ComIDs)
#' @param gis_option User specified method of creating buffers around areas of interest ("fast", "robust", or "all"). Default is "robust", which using population weighting to apportion demographic and environmental data. Note: running multiple GIS options at a time is currently not available for water-based analyses.
#' @param buffer Distance(s) used to create buffers (miles). Default is 1, 3, and 5 miles for points and 0 miles for polygons.
#' @param threshold User specified threshold to represent potential concern. Default is 80%.
#' @param state User can restrict screening to particular states. Default is to screen for entire contiguous US.
#' @param ds_mode Set upstream ('UM','UT') or downstream ('DD','DM') flow direction for water-based screening. Default is 'DD'.
#' @param ds_dist Set distance upstream/downstream along flow path for water-based screening. Default is 50 miles.
#' @param input_name Vector of names for facilities
#' @param maps_perc_geog State or US. Default is US.
#' @param attains Option to return impairment data for flow path from ATTAINS database. Default is F.
#' @param produce_ancillary_tables Option to return secondary tables/figures. Default is FALSE.
#' @param heat_table_type Locations to include in Heat Table. Options include "all", "single", or "topn". If "topn", user must also provide a value for parameter heat_table_topN.
#' @param heat_table_geog_lvl "State" or "US". Default is "US".
#' @param heat_table_input_name shape_ID Option to keep row ID number of location. Recommednd if type = 'single'
#' @param heat_table_topN Number of locations with highest median CBG values to return in Heat table.
#' @param rank_type Ranking table type, either "location" or "cbg".
#' @param rank_geography_type "State" or "US".
#' @param rank_count Number of locations or CBGs to return in ranking table.
#' @param raster_data Path to dasymetric raster data. Recommend using 1kmX1km raster
#'                    data from NASA's Socioeconomic Data and Applications Center (SEDAC)
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
EJfunction <- function(data_type, LOI_data, working_dir=NULL, input_type = NULL,
                       gis_option="robust", buffer=NULL,
                       threshold=NULL, state=NULL, ds_mode=NULL, ds_dist=NULL,
                       produce_ancillary_tables = NULL, web=F,
                       heat_table_type=NULL, heat_table_geog_lvl=NULL,
                       heat_table_input_name=NULL, heat_table_topN=NULL,
                       rank_type = NULL, rank_geography_type = NULL,
                       rank_count = NULL, maps_perc_geog='US',
                       input_name=NULL, attains=NULL, raster_data = NULL){

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



  #produce heat table and ranking table?
  if(is.null(produce_ancillary_tables)){
    produce_ancillary_tables = FALSE
  }

  #heat table checks
  if(!is.null(heat_table_type)){
    if(heat_table_type  %notin% c("all","single","topn")){
      stop("Heat table type must be one of the following: 'all', 'single',or 'topn'. Default is 'all'")
    }
  }

  if(!is.null(heat_table_geog_lvl)){
    if(heat_table_geog_lvl  %notin% c("state","US")){
      stop("Heat table type must be one of the following: 'state' or 'US'. Default is 'state'")
    }
  }

  if(is.null(heat_table_type)){
    heat_table_type <- 'all'
  }

  if(is.null(heat_table_geog_lvl)){
    heat_table_geog_lvl <- 'state'
  }

  if(heat_table_type == "topn" & is.null(heat_table_topN)){
    stop("Must specify number of locations to include for option 'topn'")
  }

  #Ranking table checks
  if(!is.null(rank_type)){
    if(rank_type  %notin% c("location","cbg")){
      stop("Heat table type must be one of the following: 'location' or 'cbg'. Default is 'location'")
    }
  }

  if(!is.null(rank_geography_type)){
    if(rank_geography_type  %notin% c("US","state")){
      stop("Heat table type must be one of the following: 'state' or 'US'. Default is 'US'")
    }
  }

  if(is.null(rank_type)){
    rank_type <- 'location'
  }

  if(is.null(rank_geography_type)){
    rank_geography_type <- 'US'
  }

  if(is.null(rank_count)){
    rank_count = 10
  }


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

  #Check for raster data. Only needed if running robust method. This data
  #needs to be pre-downloaded.
  if(is.null(gis_option) || gis_option=="robust" || gis_option=="all"){
    if(is.null(raster_data)){
      stop("Buffering using robust method requires raster data for areal
           apportionment. Please provide path to raster data.")
    }
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
      tibble::rowid_to_column("shape_ID") %>%
      st_transform("ESRI:102005")
  }

  # Create internal facility name mapping (if provided by user)
  if (!is.null(input_name)){  #& (length(input_name) == dim(LOI_data)[1])
    if(input_name %notin% colnames(LOI_data)){
      stop('Input_name must be a variable in LOI_data.')
    }
    facility_name <- LOI_data %>%
      as.data.frame() %>%
      dplyr::select(input_name,shape_ID, -geometry)
  }

  # Determine most common geometry type in the input sf dataframe
  if (in.type != 'catchment'){
    facil.geom.type <- unique(as.character(st_geometry_type(LOI_data)))
    facil.geom.type <- facil.geom.type[which.max(tabulate(match(st_geometry_type(LOI_data), facil.geom.type)))]
  }

  #=============================================================================
  #-------------------------------BRING IN DATA---------------------------------
  #=============================================================================

  if(web = T){
    mydb <- dbPool(
      RSQLite::SQLite(),
      dbname="ejscreen_db.sqlite"
    )
    data.state.uspr <- dbGetQuery(mydb, 'SELECT * FROM "data.state.uspr"')
    acs.cbg.data <- dbGetQuery(mydb, 'SELECT * FROM "acs.cbg.data"')
  } else {
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
      data.state.uspr <- fetch_data_ej(working_dir, state)
      assign("data.state.uspr", data.state.uspr, envir=globalenv())
    }

    # Bring in ACS Data
    if ("acs.cbg.data" %in% ls(envir = .GlobalEnv)) {
      data <- get('acs.cbg.data', env = .GlobalEnv)
      acs.cbg.data <- data
    } else {
      acs.cbg.data <- fetch_acs_data(working_dir,state)
      assign('acs.cbg.data', acs.cbg.data, envir=globalenv())
    }

  }

  # Join EJSCREEN + ACS Data
  data.tog <- data.state.uspr %>% dplyr::left_join(acs.cbg.data, by = c('ID' = 'GEOID'))


  #=============================================================================
  #---------------------SCREENING ANALYSES--------------------------------------
  #=============================================================================
  #For each data type, make sure GIS methods make sense.
  if(data_type=="landbased"){

    #set default to robust method
    if(is.na(gis_option)){gis_option=="robust"}

    #users can specify alternative options.
    if(gis_option %notin% c("all", "fast", "robust")){
      stop("Please provide one of the following buffer options: all, fast, robust")
    }

    # Determine most common geometry type in the input sf dataframe
    facil.geom.type <- unique(as.character(st_geometry_type(LOI_data)))
    facil.geom.type <- facil.geom.type[which.max(tabulate(match(st_geometry_type(LOI_data), facil.geom.type)))]

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

    j=1
    for(i in buffers){
      print(message(paste0('Calculating for buffer distance: ', i, ' mi...\n')))

      #if lat-lons provided, draw buffers around points
      #if polygon provided, default is to use polygon without buffer but can add buffer if desired
      if(facil.geom.type %in% c('POINT','LINESTRING','MULTIPOINT','MULTILINESTRING')){
        if (i > 0){
          facility_buff <- st_buffer(LOI_data %>%  st_transform("ESRI:102005"),
                                     dist = units::set_units(i,"mi"))
        } else {
          stop('Buffer around points required.')
        }
      } else if(facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
        if (i > 0){
          facility_buff <- st_buffer(LOI_data %>%  st_transform("ESRI:102005"),
                                     dist = units::set_units(i,"mi"))
        } else if (i == 0) {
          facility_buff <- LOI_data
        } else {
          stop('Buffer distance(s) must be numeric and non-negative.')
        }
      }

      if(gis_option %in% c("all", "fast")){
        print(message('Fast method...'))
        area1_intersect <- facility_buff %>%
          sf::st_join(data.tog, join=st_intersects) %>%
          dplyr::select(-geometry) %>%
          as.data.frame()

        # Trim down list.data to key variables.
        list.keep <- c('shape_ID', 'ID', 'STATE_NAME', 'ST_ABBREV', 'ACSTOTPOP',
                       'PM25', 'OZONE', 'DSLPM', 'CANCER', 'RESP', 'PTRAF', 'PNPL', 'PRMP',
                       'PRE1960PCT', 'PTSDF', 'PWDIS', 'VULEOPCT', 'MINORPCT', 'LOWINCPCT',
                       'UNDER5PCT', 'LESSHSPCT', 'OVER64PCT', 'LINGISOPCT',
                       'med_inc', 'frac_white', 'frac_black', 'frac_amerind',
                       'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov50', 'frac_pov99')

        temp_intersect <- area1_intersect %>%
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
        names(EJ.list.data)[j] = paste0("area1_fast_radius",i,"mi")

        EJ.index.data[[paste0("Indexes_fast_radius",i,"mi")]] <-
          EJIndexes(area1_intersect, gis_method="fast" , buffer=i, threshold=Thresh, directory = output_path)
        EJ.demographics.data[[paste0("demographics_fast_radius",i,"mi")]] <-
          EJdemographics(area1_intersect, gis_method="fast" , buffer=i, threshold=Thresh, directory = output_path)
        EJ.corrplots.data[[paste0("corrplots_fast_radius",i,"mi")]] <-
          EJCorrPlots(area1_intersect, gis_method ="fast" , buffer=i, threshold=Thresh, directory = output_path)

        if (!is.null(input_name)) {
          EJ.facil.data[[paste0('facil_fast_radius',i,'mi')]] <-
            EJFacilLevel(list_data = EJ.list.data[[j]],
                         facil_data = st_transform(LOI_data, crs = 4326),
                         ejscreen_data = data.state.uspr,
                         acs_data = acs.cbg.data,
                         thrshld = Thresh) %>%
            dplyr::inner_join(facility_name, by = 'shape_ID') %>%
            dplyr::relocate(input_name)
        } else {
          EJ.facil.data[[paste0('facil_fast_radius',i,'mi')]] <-
            EJFacilLevel(list_data = EJ.list.data[[j]],
                         facil_data = st_transform(LOI_data, crs = 4326),
                         ejscreen_data = data.state.uspr,
                         acs_data = acs.cbg.data,
                         thrshld = Thresh)
        }
      }

      if(gis_option %in% c("all", "robust")){
        print(message('Robust method...'))
        j=j+1
        area3_intersection <- sf::st_intersection(facility_buff, sf::st_buffer(data.tog,0)) %>%
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

        temp_intersect <- area3_intersection %>%
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
        state.shapes <- spData::us_states %>% st_as_sf() %>%
          st_transform(crs="ESRI:102005") %>%
          dplyr::select('NAME') %>%
          rename(facility_state = NAME)
        facility_buff <- st_join(LOI_data, state.shapes, join=st_intersects) %>%
          st_buffer(dist = units::set_units(i,"mi"))

        rm(state.shapes)

        if (!is.null(input_name)) {
          EJ.facil.data[[paste0('facil_robust_radius',i,'mi')]] <-
            areal_apportionment(ejscreen_bgs_data = data.tog,
                                facility_buff = facility_buff,
                                facil_data = LOI_data,
                                path_raster_layer = raster_data) %>%
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
      j=j+1
    }

    # Clean up table
    EJ.list.data <- Filter(Negate(is.null), EJ.list.data)
    EJ.facil.data <- Filter(Negate(is.null), EJ.facil.data)

    # Drop unnecessary lists for final output.
    rm(EJ.demographics.data, EJ.index.data, EJ.corrplots.data)

    # Return these objects as functional output
    return.me <- sapply(objects(pattern="^EJ", envir = environment()),get, envir = environment(), simplify=F, USE.NAMES=T)
    return.me <- return.me[unlist(lapply(return.me,class))!="function"]

    # If user wants all tables/figures returned, then:
    if(produce_ancillary_tables==TRUE){
      EJHeatTables(input_data = return.me, heat_table_type = heat_table_type,
                   heat_table_geog_lvl = heat_table_geog_lvl,
                   heat_table_input_name = heat_table_input_name,
                   heat_table_topN = heat_table_topN, save_option=T, directory = output_path)

      EJRanking(input_data = return.me,
                rank_type = rank_type,
                rank_geography_type = rank_geography_type,
                rank_count = rank_count,
                save_option=T,directory = output_path)

      EJCountTable(input_data = return.me, save_option = T, directory = output_path)

      EJMaps(input_data = return.me, perc_geog = maps_perc_geog, save_option = T,
             directory = output_path)
    }

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
      print(paste0('Calculating for buffer distance: ', i))
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

        state.shapes <- spData::us_states %>% st_as_sf() %>%
          st_transform(crs="ESRI:102005") %>%
          dplyr::select('NAME') %>%
          rename(facility_state = NAME)

        if(in.type == 'sf'){
          facility_buff <- st_join(LOI_data, state.shapes, join=st_intersects) %>%
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

    if(produce_ancillary_tables==TRUE){
      EJHeatTables(input_data = return.me, heat_table_type = heat_table_type,
                   heat_table_geog_lvl = heat_table_geog_lvl,
                   heat_table_input_name = heat_table_input_name,
                   heat_table_topN = heat_table_topN, save_option=T)

      EJRanking(input_data = return.me,
                rank_type = rank_type,
                rank_geography_type = rank_geography_type,
                rank_count = rank_count,
                save_option=T, directory = output_path)

      EJCountTable(input_data = return.me, save_option = T, directory = output_path)

      EJMaps(input_data = return.me, perc_geog = maps_perc_geog, save_option = T,
             directory = output_path)
    }
    return(return.me)

  }

}
