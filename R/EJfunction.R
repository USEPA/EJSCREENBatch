#' EJ tool
#'
#' Main function that performs screening (land and water-based).
#' Input must be an SF object! User must make this transformation.
#'
#' @param data_type
#' @param facility_data
#' @param gis_option User specified method of creating buffers around areas of interest (intersect, centroid, intersection). Default is intersection.
#' @param buff_dist Distance(s) used to create buffers. Default is 1, 3, and 5 miles.
#' @param threshold User specified threshold to represent potential concern. Default is 80%.
#' @param state User can restrict screening to particular states. Default is to screen for entire contiguous US.
#' @param ds_mode Set Upstream/downstream option for water-based screening. Default is downstream.
#' @param ds_dist Set distance to examine areas upstream/downstream for water-based screening. Default is 50 miles.
#' @param input.type
#' @param attains Option to pull data from the attains database. Default is FALSE.
#' @param raster.data Dasymetric raster data. Default is to use 1kmX1km raster
#'                    data from NASA's Socioeconomic Data and Applications Center (SEDAC)
#'
#'
#' @return
#' @export
#'
#' @examples
#' #read in data
#' facilities <- fread("dmr_mpp_facilities_2019.csv") %>%
#'  unique() %>%
#'  st_as_sf(coords = c("Facility Longitude", "Facility Latitude"), crs = 4326)  %>%
#'  st_transform("ESRI:102005")
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
#' # 2) buff.dist. Radius to use around facilities
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
#'                 input.type = 'sf', attains = F)
#'
EJfunction <- function(data_type, facility_data, gis_option=NULL, buff_dist=NULL,
                       threshold=NULL, state=NULL, ds_mode=NULL, ds_dist=NULL,
                       input.type = NULL, attains=NULL, raster.data = "data/US Census Grid_SF2010_TIFF"){


  `%notin%` = Negate(`%in%`)
  #check to make sure data type is currently supported in tool
  if(data_type %notin% c("landbased", "waterbased")){
    stop("Data type not supported. Please specify one of the following data types:
         landbased OR waterbased.")
  }

  #SOME MORE DATA CHECKS NEEDED HERE
  #does input type need to be specified if water-based analysis?

  # Bring in EJ Screen Data
  if ("data.state.uspr" %in% ls(envir = .GlobalEnv)) {
    get("data.state.uspr", envir = .GlobalEnv)
  } else {
    data.state.uspr <- fetch_data_ej(state)
    assign("data.state.uspr", data.state.uspr, envir=globalenv())
  }



  # Create internal function facility ID (in case user doesn't)
  facility_data <- facility_data %>%
    mutate(shape_ID = 1:n())

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
        #For each buffer radius, calculate intersection several different ways
        if (i > 0){
          facility_buff <- st_buffer(facility_data, dist = units::set_units(i,"mi"))
        } else {
          stop('Buffer around points required.')
        }
      } else if(facil.geom.type %in% c('POLYGON', 'MULTIPOLYGON')){
        #For each buffer radius, calculate intersection several different ways
        if (i > 0){
          facility_buff <- st_buffer(facility_data, dist = units::set_units(i,"mi"))
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
          EJFacilLevel(list.data = EJ.list.data[[j]],
                       facil.data = st_transform(facility_data, crs = 4326))
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
          EJdemographics(area2_centroid, gis_method="centroid" , buffer=i)
        EJ.CorrPlots.data[[paste0("CorrPlots_centroid_radius",i,"mi")]] <-
          EJCorrPlots(area2_centroid, gis_method ="centroid" , buffer=i, threshold=Thresh)
        EJ.facil.data[[paste0('facil_centroid_radius',i,'mi')]] <-
          EJFacilLevel(list.data = EJ.list.data[[j]],
                       facil.data = st_transform(facility_data, crs = 4326))
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
          EJdemographics(area3_intersection, gis_method="intersection" , buffer=i)
        EJ.CorrPlots.data[[paste0("CorrPlots_intersection_radius",i,"mi")]] <-
          EJCorrPlots(area3_intersection, gis_method ="intersection" , buffer=i, threshold=Thresh)

        # areal apportionment using circular buffers around facilities
        EJ.facil.data[[paste0('facil_intersection_radius',i,'mi')]] <-
          areal_apportionment(ejscreen.bgs.data = data.state.uspr,
                              facility_buff = facility_buff,
                              path.raster.layer = SEDAC)
        }
        j=j+1
      }



    # rm(list=ls(pattern= "^area"))
    # rm(list=ls(pattern= "^test"))
    EJ.list.data <- Filter(Negate(is.null), EJ.list.data)
    EJ.facil.data <- Filter(Negate(is.null), EJ.facil.data)

    output.list <- sapply(objects(pattern="^EJ", envir = environment()),get, envir = environment(), simplify=F, USE.NAMES=T)
    output.list <- output.list[unlist(lapply(output.list,class))!="function"]

    return(output.list)

    #--------------------------------------------------------------------------#
    #--------------------------------------------------------------------------#
    #--------------------------------------------------------------------------#
    } else if(data_type=="waterbased") {

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

      # Data type
      if(is.null(input.type)){
        in.type <- 'sf'
      } else {
        in.type <- input.type
      }

      #ATTAINS
      if(is.null(attains)){
        attains.check <- F  #default value
      } else {
        attains.check <-  attains  #user inputted values that overrides default
      }

      for (i in buffer){
        print(paste0('Calculating for buffer distance: ', i))
        catchment.polygons <- EJWaterReturnCatchmentBuffers(facility_data, ds.us.mode, ds.us.dist,
                                                            i, in.type, attains.check)

        catch.facil.data <- catchment.polygons[[1]] %>%
          st_join(facility_data)

        ############
        ## Return list.data objects 1:3
        ## Use intersection to extract **MEDIAN** data.objects
        area_intersect <- data.state.uspr %>%
          st_join(catchment.polygons[[1]], join=st_intersects) %>%
          filter(!is.na(catchment.id))

        EJ.list.data <- list()
        EJ.list.data[[paste0("Indexes_intersect_buffer",i,"mi")]] <-
          EJIndexes(area_intersect, gis_method="intersect" , buffer=i)

        EJ.list.data[[paste0("demographics_intersect_buffer",i,"mi")]] <-
          EJdemographics(area_intersect, gis_method="intersect" , buffer=i)

        EJ.list.data[[paste0("CorrPlots_intersect_buffer",i,"mi")]] <-
          EJCorrPlots(area_intersect, gis_method ="intersect" , buffer=i, threshold=Thresh)
        #############

        #############
        ## Return list.data object 4
        ## AREAL APPORTIONMENT for 1 mi buffer around stream from facility
        ## This yields pop-weighted average data for a given facility
        EJ.list.data[[paste0('catchment_intersect_buffer',i,'mi')]] <-
          areal_apportionment(ejscreen.bgs.data = data.state.uspr,
                              facility_buff = catch.facil.data,
                              raster.pop.data = SEDAC)
      }

      if(attains.check == F){
        return.me <- list(EJ.list.data,  catchment.polygons[[1]])
        names(return.me) <- c('EJ.list.data', 'EJ.buffer.summary')
      } else {
        feature.summary <- inner_join(catchment.polygons[[1]], catchment.polygons[[3]],
                                      by = c('catchment.id' = '.id'))
        return.me <- list(EJ.list.data, feature.summary,
                          catchment.polygons[[2]])
        names(return.me) <- c('EJ.list.data', 'EJ.buffer.summary',
                              'EJ.attainsdata.raw')
      }
      return(return.me)

    }

}
