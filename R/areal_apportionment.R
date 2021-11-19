#' EJ Population Areal Apportionment
#'
#' Function to apportion census block group population using dasymetric raster data
#' when using intersection approach to creating buffers around areas of interest.
#' This function currently uses 1kmX1km raster data from NASA's Socioeconomic
#' Data and Applications Center (SEDAC)
#'
#'
#' @param ejscreen_bgs_data EJSCREEN data
#' @param facility_buff Polygons representing buffered areas of interest
#' @param facil_data Original facility-level data (for non-buffered shapes)
#' @param path_raster_layer
#' @param thrshld
#'
#' @return
#' @export
#'
#' @examples
areal_apportionment <- function(ejscreen_bgs_data, facility_buff, facil_data, path_raster_layer,
                                thrshld){

  print('Importing rasters for spatial weighting...')
  layers <- list.files(path=path_raster_layer, pattern= "((pop)).*\\.tif$", full.names = TRUE )
  raster_extract <- raster::raster(layers) %>%
    raster::projectRaster(crs="ESRI:102005")

  #Decennial Census Data from NASA's SEDAC
  #get pop for block group (intersect) and area covered by buffer (intersection)
  #Used to compute fraction necessary to weight EJ Indices
  print('Computing weights by areal apportionment...')
  methods=c("intersect", "intersection")
  for(method in methods){
    intermediate <- ejscreen_bgs_data %>%
      dplyr::select(ID, Shape) %>%
      {if(method=="intersection"){
        sf::st_intersection(.,facility_buff) %>%
          dplyr::group_by(shape_ID) %>%
          dplyr::mutate(count_bgs_radius = n_distinct(ID)) %>%
          dplyr::group_by(ID) %>%
          dplyr::mutate(count_fac_radius = n_distinct(shape_ID))
      } else if(method=="intersect"){
        sf::st_join(.,facility_buff, join=st_intersects) %>%
          dplyr::filter(!is.na(shape_ID))
      }} %>%
      cbind(exactextractr::exact_extract(raster_extract, .,
                                         c('sum'),
                                         include_xy=F,
                                         stack_apply=T,
                                         full_colnames=T)) %>%
      dplyr::rename(sum.uspop10.tif=starts_with('exact'))
    assign(paste0("bgs.",method), intermediate)
  }

  #Summarizes EJ Indices for buffer, computes state and national averages, and national percentiles
  print('Reshaping data')
  facility_level <- bgs.intersect %>%
    as.data.frame() %>%
    dplyr::select(shape_ID, ID, starts_with("sum")) %>%
    dplyr::left_join(bgs.intersection %>%
                       as.data.frame() %>%
                       dplyr::select(-c(starts_with("Shape", ignore.case = FALSE))) %>%
                       dplyr::select(ID, shape_ID, facility_state, starts_with("sum"),starts_with("count_")) %>%
                       dplyr::rename(sum.uspop10.intersection = sum.uspop10.tif),
                     by = c("ID", "shape_ID")) %>%
    dplyr::mutate(fraction = as.numeric(sum.uspop10.intersection/sum.uspop10.tif*100, options(scipen=999))) %>%
    dplyr::right_join(data.state.uspr, by=c("ID"="ID")) %>%
    dplyr::select(ID, shape_ID, sum.uspop10.tif, sum.uspop10.intersection,
                  fraction, count_bgs_radius, #count_fac_radius,
                  facility_state, STATE_NAME, ACSTOTPOP,
                  PM25, OZONE, DSLPM, CANCER, RESP, PTRAF, PNPL, PRMP, PRE1960PCT,
                  PTSDF, PWDIS, VULEOPCT,
                  MINORPCT, LOWINCPCT, UNDER5PCT,
                  LESSHSPCT, OVER64PCT, LINGISOPCT,
                  med_inc, frac_white, frac_black, frac_amerind,
                  frac_asian, frac_pacisl, frac_hisp,
                  frac_pov50, frac_pov99) %>%
    dplyr::group_by(shape_ID) %>%
    dplyr::mutate(across(c(PM25, OZONE, DSLPM, CANCER, RESP, PTRAF, PNPL, PRMP, PRE1960PCT,
                           PTSDF, PWDIS, VULEOPCT,
                           MINORPCT, LOWINCPCT, UNDER5PCT,
                           LESSHSPCT, OVER64PCT, LINGISOPCT,
                           med_inc, frac_white, frac_black, frac_amerind,
                           frac_asian, frac_pacisl, frac_hisp,
                           frac_pov50, frac_pov99),
                         list(~ifelse(!is.na(ID) & is.na(shape_ID), ., (sum(fraction*ACSTOTPOP*., na.rm=T)/sum(fraction*ACSTOTPOP,na.rm=T)))  )  ,
                         .names="raw_{.col}")) %>%
    dplyr::select(shape_ID, sum.uspop10.intersection, ID, facility_state, STATE_NAME, starts_with("raw_"), ends_with("state"), ends_with("US")) %>%
    dplyr::mutate(ID = ifelse(!is.na(shape_ID),"1",ID),
                  sum.uspop10.intersection = sum(sum.uspop10.intersection)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(c(starts_with("raw_")),
                         list(~round(ecdf(data.state.uspr %>%
                                            as.data.frame() %>%
                                            dplyr::select(as.name(str_replace(cur_column(), c("raw_") ,""))) %>%
                                            unlist() %>%
                                            as.numeric())(.)*100
                                     ,0)),
                         # list(~ntile(., 100)),
                         .names="P_{.col}_US"))  %>%
    #rename_with(~ sub("raw_", "", .x), everything()) %>%
    dplyr::filter(!is.na(shape_ID) & facility_state == STATE_NAME) %>%
    dplyr::rename(`Pop. Count` = sum.uspop10.intersection)

  states <- facility_level %>%
    dplyr::select(STATE_NAME) %>%
    dplyr::filter(!(STATE_NAME=="Puerto Rico")) %>%
    unique() %>%
    unlist() %>%
    as.list()

  #computes state percentiles--looping to make sure distribution is used so that we get percentiles by state
  #Can parallelize for speed
  print('Computing state percentiles...')
  rename_cols <- c("PM25","OZONE","DSLPM","CANCER","RESP","PTRAF","PNPL",
                   "PRMP","PRE1960PCT","PTSDF","PWDIS","VULEOPCT","MINORPCT",
                   "LOWINCPCT","UNDER5PCT","LESSHSPCT","OVER64PCT","LINGISOPCT",
                   "med_inc","frac_white","frac_black","frac_amerind","frac_asian",          
                   "frac_pacisl","frac_hisp","frac_pov50","frac_pov99")
  
  facility_level_estimates <- do.call(rbind,lapply(states, function(x){
    iterm <- facility_level %>%
      dplyr::filter(STATE_NAME==x) %>%
      dplyr::filter(!is.na(shape_ID))  %>%
      dplyr::mutate(across(c(starts_with("raw_")),
                           list(~round(ecdf(na.omit(data.state.uspr %>%
                                                      as.data.frame() %>%
                                                      dplyr::filter(STATE_NAME==x) %>%
                                                      dplyr::select(as.name(str_replace(cur_column(), c("raw_"),""))) %>%
                                                      unlist() %>%
                                                      as.numeric()))(.)*100
                                       ,0)),
                           .names="P_{.col}_state"))  %>%
      dplyr::rename_with(~ sub("raw_", "", .x), everything())
  })) %>%
    rename_with(.fn = ~paste0('P_',paste0(., '_raw')), .cols = all_of(rename_cols)) %>%
    dplyr::select(shape_ID, `Pop. Count`, starts_with("P_"),count_bgs_radius) %>%
    dplyr::rename(shapeID = shape_ID) %>%
    tidyr::pivot_longer(cols=starts_with("P_"),
                        names_to="variable",
                        values_to = "value") %>%
    dplyr::mutate(variable = stringi::stri_replace_last_fixed(variable,'_','.')) %>%
    tidyr::separate(variable, into = c("variable","geography"), sep = "\\.",
                    extra = "merge", fill = "left")   %>%
    tidyr::pivot_wider(names_from = c(variable)) %>%
    dplyr::rename(Lead                = P_PRE1960PCT,
                  'Diesel PM'         = P_DSLPM,
                  'Air, Cancer'       = P_CANCER,
                  'Resp. Hazard'      = P_RESP,
                  'Traffic'           = P_PTRAF,
                  'WW Discharge'      = P_PWDIS,
                  'NPL'               = P_PNPL,
                  'RMP Facility'      = P_PRMP,
                  'TSD Facility'      = P_PTSDF,
                  'Ozone'             = P_OZONE,
                  'PM'                = P_PM25,
                  'Demo. Index'       = P_VULEOPCT,
                  Minority            = P_MINORPCT,
                  'Low Income'        = P_LOWINCPCT,
                  'Less HS Educ'      = P_LESSHSPCT,
                  'Ling. Isol.'       = P_LINGISOPCT,
                  'Age Under 5'       = P_UNDER5PCT,
                  'Age Over 64'       = P_OVER64PCT,
                  'Median Income'     = P_med_inc,
                  'Caucasian (%)'     = P_frac_white,
                  'Black (%)'         = P_frac_black,
                  'Amer. Ind. (%)'    = P_frac_amerind,
                  'Asian (%)'             = P_frac_asian,
                  'Pac. Isl (%)'      = P_frac_pacisl,
                  'Hispanic (%)'      = P_frac_hisp,
                  '<50% P.L. (%)'     = P_frac_pov50,
                  '<100% P.L. (%)'    = P_frac_pov99,
                  shape_ID            = shapeID)

  df.latlon <- facil_data %>%
    dplyr::select(shape_ID, geometry) %>%
    st_transform(crs = 4326)

  # Merge all together
  together.sf <- dplyr::inner_join(facility_level_estimates, df.latlon, by = "shape_ID") %>%
    #inner_join(df.pop.sum, by = 'shape_ID') %>% come back later to add population?
    dplyr::relocate(shape_ID, `Pop. Count`,
                    `Low Income`, `Minority`, `Less HS Educ`, `Ling. Isol.`,
                    `Age Under 5`, `Age Over 64`, `Air, Cancer`, `Diesel PM`,
                    Lead, Ozone, PM, NPL, `RMP Facility`, Traffic, `TSD Facility`,
                    `WW Discharge`, `Resp. Hazard` #,
                    #`Median Income`, `Caucasian (%)`, `Black (%)`, `Amer. Ind. (%)`,
                    #`Asian (%)`, `Pac. Isl (%)`, `Hispanic (%)`,
                    #`<50% P.L. (%)`, `<100% P.L. (%)`
                    )
  
  together.sf <- together.sf %>%
    dplyr::mutate(!!paste0('Env. indicators above ',thrshld,'th %ile') :=
                    as.factor(rowSums(dplyr::select(as.data.frame(together.sf),
                                                    `Air, Cancer`:`Resp. Hazard`) > thrshld))) %>%
    dplyr::mutate(!!paste0('Demo. indicators above ',thrshld,'th %ile') :=
                    as.factor(rowSums(dplyr::select(as.data.frame(together.sf),
                                                    `Low Income`:`Age Over 64`) > thrshld))) 
  together.perc <- together.sf %>%
    dplyr::filter(geography != 'raw') %>%
    dplyr::mutate_if(is.numeric, round)
  
  together.sf <- together.sf %>%
    dplyr::filter(geography == 'raw') %>%
    dplyr::mutate(`Pop. Count` = round(`Pop. Count`),
                  !!paste0('Env. indicators above ',thrshld,'th %ile') := NA,
                  !!paste0('Demo. indicators above ',thrshld,'th %ile') := NA
                  )
  
  together.sf <- rbind(together.sf, together.perc) %>%
    arrange(shape_ID, geography)

  return(together.sf)
}
