#' Method for using census blocks for buffering in EJ proximity analysis.
#'
#' @param dta_year The data vintage year specified by user
#' @param buff_dta An sf data.frame containing the (buffered) LOIs
#' @param ejscreen_bgs_data EJSCREEN data in sf data.frame format.
#' @param ejvarlist Meta-data relevant to running the screening analysis
#'
#' @return A list containing 2 sublists: (1) a LOI-buffer-level screening summary, and (2) a CBG-level screening summary for all block groups within affected communities.
#' @export
#'
#' @examples
EJSCREENBufferBlocks <- function(dta_year, buff_dta, ejscreen_bgs_data, ejvarlist){
  # Bring in Census Block centroids
  if ("block.data" %in% ls(envir = .GlobalEnv)){
    data <- get('block.data', env = .GlobalEnv)
    block.data <- data
  } else {
    block.data <- fetch_blockcents(dta_year)
    assign('block.data', block.data, envir = globalenv())
  }

  message('Performing spatial intersections.')
  #intersect buffer area with CBG-level EJSCREEN data
  area.intersection <- buff_dta %>%
    sf::st_join(ejscreen_bgs_data,
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
  cb.intersect <- buff_dta %>%
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
                                  list(~round(ecdf(na.omit(ejscreen_bgs_data %>%
                                                             sf::st_drop_geometry() %>%
                                                             as.data.frame() %>%
                                                             dplyr::filter(ST_ABBREV==x) %>%
                                                             dplyr::select(cur_column())) %>%
                                                     unlist() %>%
                                                     as.numeric())(.)*100
                                              ,0)),
                                  .names="P_{.col}_state"))
  })

  #clear some memory
  rm(states, temp_intersect, loi_clean,area_together, area_cb_intersect)


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
                           list(~round(ecdf(na.omit(ejscreen_bgs_data %>%
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
