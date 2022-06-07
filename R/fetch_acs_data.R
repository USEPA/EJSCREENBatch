#' Fetch data from ACS
#'
#' This function looks for demographic data from ACS. First checks if this data is
#' in working directory. If not, it creates a directory and downloads most recent
#' data.
#' @param working_dir
#' @param state_filter Users may restrict screening to a particular state in the
#' contiguous US. If so, users can specify a state. Default is to conduct
#' screening for the entire contiguous US.
#'
#' @return
#' @export
#'
#' @examples

fetch_acs_data <- function(working_dir, state_filter){

  # Create directory if needed.
  ifelse(!dir.exists(paste0(working_dir,"/ACS_data")),
         dir.create(paste0(working_dir,"/ACS_data")), FALSE)

  # Function to extract relevant CBG dataframe
  parallel.api <- function(st){
    tidycensus::get_acs(
      geography = 'block group',
      variables = c(pop_total = 'B02001_001',
                    pop_white = 'B02001_002',
                    pop_black = 'B02001_003',
                    pop_amerind = 'B02001_004',
                    pop_asian = 'B02001_005',
                    pop_pacisl = 'B02001_006',
                    pop_hisp = 'B03003_003',
                    pov50 = 'C17002_002',
                    pov99 = 'C17002_003',
                    med_inc = 'B19013_001'),
      state = st,
      year = 2020,
      geometry = F
    ) %>%
      dplyr::select(GEOID, variable, estimate) %>%
      tidyr::pivot_wider(id_cols = c(GEOID), names_from = variable, values_from = estimate)
  }

  if(identical(list.files(path=paste0(working_dir,"/ACS_data"), pattern="acs_ejstats.csv"),
               character(0))){

    ## State lists
    state.list <- c(state.abb, 'DC')
    state.list <- state.list[!(state.list %in% c('AK', 'HI'))]

    # Loop (in parallel) thru each county/state pair, calling census API
    future::plan(multisession, workers = (parallel::detectCores()-2))
    cbg.list <- furrr::future_pmap(list(state.list), parallel.api)

    cbg.together <- data.table::rbindlist(cbg.list, use.names = T, idcol = 'statelistid')
    convert.cols <- names(cbg.together)[-2]
    acs.cbg.data <- cbg.together[, (convert.cols) := lapply(.SD, as.integer),
                                 .SDcols = convert.cols
            ][, state := state.list[statelistid]
              ][, .(GEOID, state, med_inc,
                    frac_white = pop_white/pop_total,
                    frac_black = pop_black/pop_total,
                    frac_amerind = pop_amerind/pop_total,
                    frac_asian = pop_asian/pop_total,
                    frac_pacisl = pop_pacisl/pop_total,
                    frac_hisp = pop_hisp/pop_total,
                    frac_pov50 = pov50/pop_total,
                    frac_pov99 = (pov50+pov99)/pop_total)]
    rm(convert.cols, cbg.list, cbg.together)

    # Write file for future use
    fwrite(acs.cbg.data, paste0(working_dir,'/ACS_data/acs_ejstats.csv'))

  } else {
    acs.cbg.data <- fread(paste0(working_dir,'/ACS_data/acs_ejstats.csv'),
                          colClasses = c('GEOID'='character'))
  }

  # Filter down to user-requested state
  if (!is.null(state_filter)){
    acs.cbg.data <- acs.cbg.data %>%
      filter(state %in% state_filter)
  }

  return(acs.cbg.data)
}
