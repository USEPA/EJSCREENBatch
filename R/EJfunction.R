#' EJ tool
#'
#' Main function that performs screening (land and water-based).
#' Input must be an SF object! User must make this transformation.
#'
#' @param data_type Required. Either "landbased" or "waterbased"
#' @param facility_data Required.
#' @param gis_option User specified method of creating buffers around areas of interest (intersect, centroid, intersection). Default is intersection.
#' @param buff_dist Distance(s) used to create buffers (miles). Default is 1, 3, and 5 miles.
#' @param threshold User specified threshold to represent potential concern. Default is 80\%.
#' @param state User can restrict screening to particular states. Default is to screen for entire contiguous US.
#' @param ds_mode Set Upstream/downstream option for water-based screening. Default is downstream.
#' @param ds_dist Set distance to examine areas upstream/downstream for water-based screening. Default is 50 miles.
#' @param input_type
#' @param attains Option to pull data from the attains database. Default is FALSE.
#' @param raster_data Dasymetric raster data. Default is to use 1kmX1km raster
#'                    data from NASA's Socioeconomic Data and Applications Center (SEDAC)
#'
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
#' # 1) gis_option. Three options available: intersect, centroid, intersection.
#' #    Instersection is default.
#' # 2) buff_dist. Radius to use around facilities
#' # 3) Threshold for EJ consideration. EJScreen uses 80 as default.
#' # 4) states. Can restrict analysis to specific states.
#' # bring in data for contiguous US
#' a1 <- EJfunction(data_type="landbased", facility_data = facilities, gis_option="centroid",
#'                 buff_dist = 5)
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
#' # 3) buff.dist. Buffer distance around catchments in miles. 1 mile is default.
#' # 4) Attains. Call attains API for data? (T/F). Default is False
#'
#' c <- EJfunction(data_type="waterbased", facility_data=facilities,
#'                 input_type = 'sf', attains = F)
#'
EJfunction <- function(data_type, facility_data, gis_option=NULL, buff_dist=NULL,
                       threshold=NULL, state=NULL, ds_mode=NULL, ds_dist=NULL,
                       input_type = NULL, attains=NULL, raster_data = "data/US Census Grid_SF2010_TIFF"){


  `%notin%` = Negate(`%in%`)
  #check to make sure data type is currently supported in tool
  if(data_type %notin% c("landbased", "waterbased")){
    stop("Data type not supported. Please specify one of the following data types:
         landbased OR waterbased.")
  }


  # Bring in EJ Screen Data
  if ("data.state.uspr" %in% ls(envir = .GlobalEnv)) {
    get("data.state.uspr", envir = .GlobalEnv)
  } else {
    data.state.uspr <- fetch_data_ej(state)
    assign("data.state.uspr", data.state.uspr, envir=globalenv())
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
      facility_data <- as.data.frame(facility_data)
      names(facility_data) <- 'V1'
    }
  }

  #Check for raster data. Only needed if running intersection method. This data
  #needs to be pre-downloaded.
  if(is.null(gis_option) || gis_option=="intersection" || gis_option=="all"){
    if(is.null(raster_data)){
      stop("Buffering using intersection method requires raster data for areal
           apportionment. Please provide path to raster data.")
    }
  }



  # Create internal function facility ID (in case user doesn't)
  facility_data <- facility_data %>%
    tibble::rowid_to_column("shape_ID")

  # Determine most common geometry type in the input sf dataframe
  facil.geom.type <- unique(as.character(st_geometry_type(facility_data)))
  facil.geom.type <- facil.geom.type[which.max(tabulate(match(st_geometry_type(facility_data), facil.geom.type)))]

  #set threshold
  if(is.null(threshold)){
    Thresh <-  80 #default values
  } else {
    Thresh <- threshold   #user inputted values that override default
  }

  #For each data type, make sure GIS methods make sense.
  if(data_type=="landbased"){

    #set default to intersection method
    if(is.na(gis_option)){gis_option=="intersection"}

    #users can specify alternative options.
    if(gis_option %notin% c("all", "intersect", "centroid", "intersection")){
      stop("Please provide one of the following buffer options: all, intersect, centroid, intersection")
    }

    # Determine most common geometry type in the input sf dataframe
    facil.geom.type <- unique(as.character(st_geometry_type(facility_data)))
    facil.geom.type <- facil.geom.type[which.max(tabulate(match(st_geometry_type(facility_data), facil.geom.type)))]


    #set radii to draw around areas/points of interest
    if(is.null(buff_dist) &
       facil.geom.type %in% c('POINT','LINESTRING','MULTIPOINT','MULTILINESTRING')){
      buffers <-  c(1,3,5)  #default values: points
    } else if(is.null(buff_dist) &
              facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
      buffers <- 0 #default value: polygons
    } else if(facil.geom.type %notin% c('POINT','LINESTRING','MULTIPOINT',
                                        'MULTILINESTRING','POLYGON', 'MULTIPOLYGON')){
      stop('All geometries must be (multi-) points, lines, or polygons.')
    } else {
      buffers <- buff_dist  #user inputted values that override default
    }

    #create empty lists to store lists/DFs/DTs
    EJ.list.data <- list()
    EJ.index.data <- list()
    EJ.demographics.data <- list()
    EJ.CorrPlots.data <- list()
    EJ.facil.data <- list()

    j=1
    for(i in buffers){
      print(paste0('Calculating for buffer distance: ', i, ' mi...'))

      #if lat-lons provided, draw buffers around points
      #if polygon provided, default is to use polygon without buffer but can add buffer if desired
      if(facil.geom.type %in% c('POINT','LINESTRING','MULTIPOINT','MULTILINESTRING')){
        if (i > 0){
          facility_buff <- st_buffer(facility_data %>% st_transform("ESRI:102005"), 
                                     dist = units::set_units(i,"mi"))
        } else {
          stop('Buffer around points required.')
        }
      } else if(facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
        if (i > 0){
          facility_buff <- st_buffer(facility_data %>% st_transform("ESRI:102005"), 
                                     dist = units::set_units(i,"mi"))
        } else if (i == 0) {
          facility_buff <- facility_data
        } else {
          stop('Buffer distance(s) must be numeric and non-negative.')
        }
      }

      if(gis_option %in% c("all", "intersect")){
        print('Intersect method...')
        area1_intersect <- facility_buff %>%
          st_join(data.state.uspr, join=st_intersects) %>%
          dplyr::select(-geometry) %>%
          as.data.frame()

        EJ.list.data[[j]] <- area1_intersect
        names(EJ.list.data)[j] = paste0("area1_intersect_radius",i,"mi")

        EJ.index.data[[paste0("Indexes_intersect_radius",i,"mi")]] <-
          EJIndexes(area1_intersect, gis_method="intersect" , buffer=i)
        EJ.demographics.data[[paste0("demographics_intersect_radius",i,"mi")]] <-
          EJdemographics(area1_intersect, gis_method="intersect" , buffer=i)
        EJ.CorrPlots.data[[paste0("CorrPlots_intersect_radius",i,"mi")]] <-
          EJCorrPlots(area1_intersect, gis_method ="intersect" , buffer=i, threshold=Thresh)
        EJ.facil.data[[paste0('facil_intersect_radius',i,'mi')]] <-
          EJFacilLevel(list_data = EJ.list.data[[j]],
                       facil_data = st_transform(facility_data, crs = 4326))
      }

      if(gis_option %in% c("all", "centroid")){
        print('Centroid method...')
        j=j+1
        area2_centroid <- facility_buff %>%
          st_join(st_centroid(data.state.uspr), join=st_contains) %>%
          as.data.frame() %>%
          dplyr::select(-geometry)

        EJ.list.data[[j]] <- area2_centroid
        names(EJ.list.data)[j] = paste0("area2_centroid_radius",i,"mi")

        EJ.index.data[[paste0("Indexes_centroid_radius",i,"mi")]] <-
          EJIndexes(area2_centroid, gis_method="centroid" , buffer=i)
        EJ.demographics.data[[paste0("demographics_centroid_radius",i,"mi")]] <-
          EJdemographics(area2_centroid, gis_method="centroid" , buffer=i, threshold=Thresh)
        EJ.CorrPlots.data[[paste0("CorrPlots_centroid_radius",i,"mi")]] <-
          EJCorrPlots(area2_centroid, gis_method ="centroid" , buffer=i, threshold=Thresh)
        EJ.facil.data[[paste0('facil_centroid_radius',i,'mi')]] <-
          EJFacilLevel(list_data = EJ.list.data[[j]],
                       facil_data = st_transform(facility_data, crs = 4326))
      }

      if(gis_option %in% c("all", "intersection")){
        print('Intersection method...')
        j=j+1
        #THIS STILL NEEDS WORK TO WEIGHT BY PROPORTION
        area3_intersection <- st_intersection(facility_buff, st_buffer(data.state.uspr,0)) %>%
          mutate(area_geo = st_area(geometry)) %>%
          mutate(percent_area = area_geo/area_bg*100) %>%
          dplyr::select(-geometry) %>%
          as.data.frame()

        EJ.list.data[[j]] <- area3_intersection
        names(EJ.list.data)[j] = paste0("area3_intersection_radius",i,"mi")

        EJ.index.data[[paste0("Indexes_intersection_radius",i,"mi")]] <-
          EJIndexes(area3_intersection, gis_method="intersection" , buffer=i)
        EJ.demographics.data[[paste0("demographics_intersection_radius",i,"mi")]] <-
          EJdemographics(area3_intersection, gis_method="intersection" , buffer=i, threshold=Thresh)
        EJ.CorrPlots.data[[paste0("CorrPlots_intersection_radius",i,"mi")]] <-
          EJCorrPlots(area3_intersection, gis_method ="intersection" , buffer=i, threshold=Thresh)

        ### Areal apportionment using circular buffers around facilities
        # Extract the state associated with each facility
        state.shapes <- spData::us_states %>% st_as_sf() %>%
          st_transform(crs="ESRI:102005") %>%
          dplyr::select('NAME') %>%
          rename(facility_state = NAME)
        facility_buff <- st_join(facility_data, state.shapes, join=st_intersects) %>%
          st_buffer(dist = units::set_units(i,"mi"))

        rm(state.shapes)

        EJ.facil.data[[paste0('facil_intersection_radius',i,'mi')]] <-
          areal_apportionment(ejscreen_bgs_data = data.state.uspr,
                              facility_buff = facility_buff,
                              facil_data = facility_data,
                              path_raster_layer = raster_data)
        }
        j=j+1
      }


    EJ.list.data <- Filter(Negate(is.null), EJ.list.data)
    EJ.facil.data <- Filter(Negate(is.null), EJ.facil.data)

    output.list <- sapply(objects(pattern="^EJ", envir = environment()),get, envir = environment(), simplify=F, USE.NAMES=T)
    output.list <- output.list[unlist(lapply(output.list,class))!="function"]

    return(output.list)

    #--------------------------------------------------------------------------#
    #--------------------------------------------------------------------------#
    #--------------------------------------------------------------------------#
    } else if(data_type=="waterbased") {

      ## Can come back and add all option later if demand exists.
      if(gis_option == 'all'){
        stop('Please choose ONLY ONE of (centroid, intersect, intersection) for water-based analysis.')
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
      if(is.null(buff_dist)){
        buffer <- 1  #default value
      } else {
        buffer <-  buff_dist  #user inputted values that overrides default
      }

      #ATTAINS
      if(is.null(attains)){
        attains.check <- F  #default value
      } else {
        attains.check <-  attains  #user inputted values that overrides default
      }

      EJ.demographics.data <- list()
      # EJ.demoOverlap.data <- list()
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
        catchment.polygons <- EJWaterReturnCatchmentBuffers(facility_data, ds.us.mode, ds.us.dist,
                                                            i, in.type, attains.check)

        if (in.type == 'sf') {
          catch.facil.data <- catchment.polygons[[1]] %>%
            as.data.frame() %>%
            inner_join(dplyr::select(as.data.frame(facility_data), -geometry),
                       by = 'shape_ID') %>%
            st_as_sf()
        } else {
          catch.facil.data <- catchment.polygons[[1]] %>%
            st_as_sf()
        }

        #############
        ## This section intersects/contains facility buffered areas and CBGs
        if (gis_option %in% c('intersect', 'intersection')){
          area <- catchment.polygons[[1]] %>%
            st_join(data.state.uspr, join = st_intersects) %>%
            filter(!is.na(shape_ID)) %>%
            st_drop_geometry()
          #            dplyr::select(-starts_with('Shape', ignore.case = F))
        } else if (gis_option %in% c('centroid')){
          area <- catchment.polygons[[1]] %>%
            st_join(st_centroid(data.state.uspr), join=st_contains) %>%
            st_drop_geometry()
        }

        EJ.list.data[[paste0('area1_',gis_option,'_radius',i,'mi')]] <- area

        EJ.index.data[[paste0("Indexes_",gis_option,"_buffer",i,"mi")]] <-
          EJIndexes(area, gis_method = gis_option, buffer=i)

        EJ.demographics.data[[paste0("demographics_",gis_option,"_buffer",i,"mi")]] <-
          EJdemographics(area, gis_method = gis_option, buffer=i, threshold=Thresh)

        # EJ.demoOverlap.data[[paste0("demoOverlap_",gis_option,"_buffer",i,"mi")]] <-
        #   EJdemoOverlap(area, gis_method = gis_option, buffer=i, threshold=Thresh)


        #############
        ## This returns facility level summaries for
        if(gis_option %in% c('intersect','centroid')){
          if (in.type == 'sf'){
            EJ.facil.data[[paste0('facil_',gis_option,'_radius',i,'mi')]] <-
              EJFacilLevel(list_data = area,
                           facil_data = st_transform(facility_data, crs = 4326))
          } else if (in.type == 'catchment'){
            temp.mat <- as.data.frame(catchment.polygons[[4]]) %>%
              mutate(comid = as.numeric(comid)) %>%
              inner_join(facility_data, by = c('comid' = 'V1')) %>%
              st_as_sf()
            EJ.facil.data[[paste0('facil_',gis_option,'_radius',i,'mi')]] <-
              EJFacilLevel(list_data = area,
                           facil_data = st_transform(temp.mat, crs = 4326))
            rm(temp.mat)
          }

          ## AREAL APPORTIONMENT for user-selected buffer around stream from facility
          ## This yields pop-weighted average data for a given facility
        } else if(gis_option == 'intersection'){

          state.shapes <- spData::us_states %>% st_as_sf() %>%
            st_transform(crs="ESRI:102005") %>%
            dplyr::select('NAME') %>%
            rename(facility_state = NAME)
          if(in.type == 'sf'){
            facility_buff <- st_join(facility_data, state.shapes, join=st_intersects) %>%
              dplyr::select(shape_ID, facility_state) %>%
              st_drop_geometry() %>%
              inner_join(catchment.polygons[[1]], by = 'shape_ID') %>%
              st_as_sf()

            EJ.facil.data[[paste0('facil_intersection_radius',i,'mi')]] <-
              areal_apportionment(ejscreen_bgs_data = data.state.uspr,
                                  facility_buff = facility_buff,
                                  facil_data = facility_data,
                                  path_raster_layer = raster_data)
          } else if (in.type == 'catchment') {

            ## Shapefile for downstream (/upstream?) buffer
            facility_buff <- catchment.polygons[[4]] %>%
              mutate(comid = as.numeric(comid)) %>%
              inner_join(facility_data, by = c('comid' = 'V1')) %>%
              dplyr::select(shape_ID, facility_state) %>%
              st_drop_geometry() %>%
              inner_join(catchment.polygons[[1]], by = 'shape_ID') %>%
              st_as_sf()

            ## Shapefile with lat/lon of catchmentID waterbody centroid
            temp.mat <- as.data.frame(catchment.polygons[[4]]) %>%
              mutate(comid = as.numeric(comid)) %>%
              inner_join(facility_data, by = c('comid' = 'V1')) %>%
              st_as_sf() %>%
              st_transform(crs = 4326)

            EJ.facil.data[[paste0('facil_intersection_radius',i,'mi')]] <-
              areal_apportionment(ejscreen_bgs_data = data.state.uspr,
                                  facility_buff = facility_buff,
                                  facil_data = temp.mat,
                                  path_raster_layer = raster_data)
          }
          rm(state.shapes)


        }

        if (attains.check == T){
          EJ.buffer.shapes[[paste0('buffer_shape_radius',i,'mi')]] <-
            inner_join(catchment.polygons[[1]], catchment.polygons[[3]],
                       by = c('shape_ID' = '.id'))
          EJ.attains.data[[paste0('attains_raw_radius', i, 'mi')]] <-
            catchment.polygons[[2]]
        } else {
          EJ.buffer.shapes[[paste0('buffer_shape_radius',i,'mi')]] <-
            catchment.polygons[[1]]
        }
      }

      if(attains.check == F){
        return.me <- list(EJ.demographics.data, # EJ.demoOverlap.data
                          EJ.facil.data, EJ.list.data,
                          EJ.index.data, EJ.buffer.shapes)
        names(return.me) <- c('EJ.demographics.data', #'EJ.demoOverlap.data',
                              'EJ.facil.data', 'EJ.list.data',
                              'EJ.index.data', 'EJ.buffer.summary')
      } else {
        return.me <- list(EJ.demographics.data, EJ.demoOverlap.data,
                          EJ.facil.data, EJ.list.data,
                          EJ.index.data, EJ.buffer.shapes,
                          EJ.attains.data)
        names(return.me) <- c('EJ.demographics.data', #'EJ.demoOverlap.data',
                              'EJ.facil.data', 'EJ.list.data',
                              'EJ.index.data', 'EJ.buffer.summary',
                              'EJ.attainsdata.raw')
      }
      return(return.me)

    }

}
