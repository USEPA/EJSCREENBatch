#' Method for using raster for buffering in EJ proximity analysis.
#'
#' @param ejscreen_bgs_data EJSCREEN data in sf data.frame format.
#' @param facility_buff An sf data.frame containing the (buffered) LOIs
#' @param ejvarlist Meta-data relevant to running the screening analysis
#' @param year The data vintage year specified by user
#'
#' @return A list containing 2 sublists: (1) a LOI-buffer-level screening summary, and (2) a CBG-level screening summary for all block groups within affected communities.
#' @export
#'
#' @examples
EJSCREENBufferRaster <- function(ejscreen_bgs_data, facility_buff, ejvarlist, year){

  print('Importing rasters for spatial weighting...')
  if ("raster_extract" %in% ls(envir = .GlobalEnv)){
    data <- get('raster_extract', env = .GlobalEnv)
    raster_extract <- data
  } else {
    raster_extract <- fetch_rasters(year = year)
    assign('raster_extract', raster_extract, envir = globalenv())
  }

  print('Computing weights by areal apportionment...')

  #########################################
  ## 1. RETURN FACILITY LEVEL DATA SUMMARY
  #sum pop in buffer by facil + CBG
  #Geometry of BG, linked to LOI, calc population
  bgs.intersect <- ejscreen_bgs_data %>%
    dplyr::select_if(names(.) %in% c('ID', 'Shape')) %>%
    sf::st_join(.,facility_buff, join=sf::st_intersects) %>%
    dplyr::filter(!is.na(shape_ID)) %>%
    #key step: extract raster-based pop estimate for CBG
    cbind(exactextractr::exact_extract(raster_extract, .,
                                       c('sum'),
                                       include_xy=F,
                                       stack_apply=T,
                                       full_colnames=T)) %>%
    dplyr::rename(sum.uspop.tif=dplyr::starts_with('exact'))

  #Geometry of shared portion of BG + LOI's buffer area, calc population
  bgs.intersection <- ejscreen_bgs_data %>%
    dplyr::select_if(names(.) %in% c('ID', 'Shape')) %>%
    dplyr::filter(ID %in% bgs.intersect$ID) %>%
    sf::st_intersection(., facility_buff) %>%
    #key step: extract raster-based pop estimate for CBG+buffer intersection
    cbind(exactextractr::exact_extract(raster_extract, .,
                                       c('sum'),
                                       include_xy=F,
                                       stack_apply=T,
                                       full_colnames=T)) %>%
    dplyr::rename(sum.uspop.robust=starts_with('exact'))

  print('Reshaping data')
  #columns to keep in the facility level table
  colsToKeep <- c(ejvarlist[[1]][-(1:4)],
                  'med_inc','frac_white', 'frac_black', 'frac_amerind',
                  'frac_asian', 'frac_pacisl', 'frac_hisp', 'frac_pov99', 'frac_pov199',
                  'POP_sum')

  facility_level <- bgs.intersect %>%
    as.data.frame() %>%
    dplyr::select(shape_ID, ID, starts_with("sum")) %>%
    dplyr::left_join(bgs.intersection %>%
                       as.data.frame() %>%
                       dplyr::select(-c(dplyr::starts_with("Shape", ignore.case = FALSE))) %>%
                       dplyr::select(ID, shape_ID,dplyr::starts_with("sum")),
                     by = c("ID", "shape_ID")) %>%
    #calculate fraction of CBG population that falls within LOI's buffer
    dplyr::mutate(fraction = as.numeric(sum.uspop.robust/sum.uspop.tif*100, options(scipen=999))) %>%
    dplyr::left_join(ejscreen_bgs_data, by=c("ID"="ID")) %>%
    data.table::as.data.table()

  #function to calculate modal value for LOI's state assignment (for %iles)
  #? for future: how to best treat buffer areas that span multiple states?
  Modal <- function(x) {
    val <- unique(x)
    return(val[which.max(tabulate(match(x, val)))])
  }

  message('Creating LOI-level summary table.')
  #clean tables... weighted average of values at the LOI level.
  loi_clean <- facility_level[is.na(fraction), fraction := 0L
  ][, POP_sum := round(sum((fraction/100)*ACSTOTPOP)),
    by = shape_ID
  ][, ST_ABB := Modal(ST_ABBREV),
    by = shape_ID
  ][, lapply(.SD, weighted.mean, w=fraction*ACSTOTPOP, na.rm = T),
    by = list(shape_ID, ST_ABB),
    .SDcols = colsToKeep
    ][, lapply(.SD, function(x) replace(x, is.nan(x), NA))]

  #calculate percentiles using the raw data distributions
  message('Computing LOI-level percentiles...')

  #national percentiles
  temp_intersect <- loi_clean %>%
    dplyr::select(dplyr::any_of(c('shape_ID','ST_ABB',colsToKeep))) %>%
    dplyr::mutate(dplyr::across(colsToKeep[-length(colsToKeep)],
                                list(~round(ecdf(ejscreen_bgs_data %>%
                                                   sf::st_drop_geometry() %>%
                                                   as.data.frame() %>%
                                                   dplyr::select(cur_column()) %>%
                                                   unlist() %>%
                                                   as.numeric())(.)*100
                                            ,0)),
                                .names="P_{.col}_US"))

  #state percentiles
  states <- na.omit(unique(temp_intersect$ST_ABB))
  LOI_list <- lapply(states, function(x){
    temp_intersect %>%
      dplyr::filter(ST_ABB==x) %>%
      dplyr::filter(!is.na(shape_ID))  %>%
      dplyr::mutate(dplyr::across(colsToKeep[-length(colsToKeep)],
                                  list(~tryCatch(round(ecdf(na.omit(ejscreen_bgs_data %>%
                                                                      sf::st_drop_geometry() %>%
                                                                      as.data.frame() %>%
                                                                      dplyr::filter(ST_ABBREV==x) %>%
                                                                      dplyr::select(cur_column())) %>%
                                                              unlist() %>%
                                                              as.numeric())(.)*100
                                                       ,0),
                                                 error = function(err) return(NA))),
                                  .names="P_{.col}_state"))
  })

  #clear some memory
  rm(states, temp_intersect, loi_clean, bgs.intersection, facility_level)

  ####################################
  ## 2. RETURN CBG LEVEL DATA SUMMARY
  #sum pop in buffer across all facilities.
  message('Calculating within-buffer apportionment % for CBGs...')
  bgs.intersection <- ejscreen_bgs_data %>%
    dplyr::select_if(names(.) %in% c('ID', 'Shape')) %>%
    dplyr::filter(ID %in% bgs.intersect$ID) %>%
    sf::st_intersection(., sf::st_union(facility_buff)) %>%
    #key step: extract raster-based pop estimate for CBG+buffer intersection
    cbind(exactextractr::exact_extract(raster_extract, .,
                                       c('sum'),
                                       include_xy=F,
                                       stack_apply=T,
                                       full_colnames=T)) %>%
    dplyr::rename(sum.uspop.robust=starts_with('exact'))

  allColsToKeep <- c(ejvarlist[[1]],
                     paste0("P_",ejvarlist[[1]][-(1:4)],"_US"),
                     paste0("P_",ejvarlist[[1]][-(1:4)],"_state"),
                     'med_inc','frac_white', 'frac_black','frac_amerind',
                     'frac_asian','frac_pacisl','frac_hisp','frac_pov99','frac_pov199')

  message('Creating CBG-level summary table.')
  all_together <- bgs.intersect %>%
    as.data.frame() %>%
    dplyr::select(ID, starts_with("sum")) %>%
    dplyr::distinct() %>%
    dplyr::left_join(bgs.intersection %>%
                       as.data.frame() %>%
                       dplyr::select(-c(dplyr::starts_with("Shape", ignore.case = FALSE))) %>%
                       dplyr::select(ID,dplyr::starts_with("sum")),
                     by = c("ID")) %>%
    #calculate fraction of CBG population that falls within CBG's buffer
    dplyr::mutate(fraction = as.numeric(sum.uspop.robust/sum.uspop.tif, options(scipen=999))) %>%
    dplyr::left_join(ejscreen_bgs_data, by=c("ID"="ID")) %>%
    dplyr::mutate(POP_sum = ceiling(fraction*ACSTOTPOP)) %>%
    dplyr::select(dplyr::any_of(c(allColsToKeep,'POP_sum'))) %>%
    dplyr::select(-c('ACSTOTPOP')) %>%
    dplyr::filter(POP_sum > 0) %>%
    data.table::as.data.table()

  #calculate percentiles using the raw data distributions
  message('Computing CBG-level percentiles...')

  #national percentiles
  temp_all <- all_together %>%
    dplyr::mutate(dplyr::across(c('med_inc','frac_white', 'frac_black','frac_amerind',
                                  'frac_asian','frac_pacisl','frac_hisp','frac_pov99','frac_pov199'),
                                list(~round(ecdf(ejscreen_bgs_data %>%
                                                   sf::st_drop_geometry() %>%
                                                   as.data.frame() %>%
                                                   dplyr::select(cur_column()) %>%
                                                   unlist() %>%
                                                   as.numeric())(.)*100
                                            ,0)),
                                .names="P_{.col}_US"))

  #state percentiles
  states <- na.omit(unique(temp_all$ST_ABBREV))
  CBG_list <- lapply(states, function(x){
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


  return(list(data.table::rbindlist(LOI_list),
              data.table::rbindlist(CBG_list)))
}
