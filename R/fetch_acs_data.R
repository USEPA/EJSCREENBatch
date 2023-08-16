#' Support function to extract data from ACS
#'
#' This function looks for demographic data from ACS. First checks if this data is
#' in working directory. If not, it creates a directory and downloads most recent
#' data.
#'
#' @param year Year of ACS data desired by user.
#' @param state_filter Users may restrict screening to a particular state in the
#' contiguous US. If so, users can specify a state. Default is to conduct
#' screening for the entire contiguous US.
#'
#' @return data.frame of national ACS demographic data at block group resolution.
#' @export
#'
#' @examples
fetch_acs_data <- function(year, state_filter = NULL){

  # Create directory if needed.
  ifelse(!dir.exists(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                        "/ACS_data")),
         dir.create(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                           "/ACS_data")), FALSE)

  yr_input = year

  # Function to extract relevant CBG dataframe
  parallel.api <- function(st){
    tidycensus::get_acs(
      geography = 'block group',
      variables = c(pop_total = 'B03002_001',
                    pop_white = 'B03002_003',
                    pop_black = 'B03002_004',
                    pop_amerind = 'B03002_005',
                    pop_asian = 'B03002_006',
                    pop_pacisl = 'B03002_007',
                    pop_hisp = 'B03002_012',
                    pov_total = 'C17002_001',
                    pov50 = 'C17002_002',
                    pov99 = 'C17002_003',
                    pov124 = 'C17002_004',
                    pov149 = 'C17002_005',
                    pov184 = 'C17002_006',
                    pov199 = 'C17002_007',
                    med_inc = 'B19013_001'),
      state = st,
      year = yr_input,
      geometry = F
    ) %>%
      dplyr::select(GEOID, variable, estimate) %>%
      tidyr::pivot_wider(id_cols = c(GEOID), names_from = variable, values_from = estimate)
  }

  # If EJStats file for year of interest doesn't exist, create it.
  # Else, open the existing file
  if(!(file.exists(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                '/ACS_data/acs_',yr_input,'_ejstats.csv')))){

    ## State lists
    state.list <- c(state.abb, 'DC')

    # Loop (in parallel) thru each county/state pair, calling census API
    future::plan("multisession", workers = (parallel::detectCores()-2))
    cbg.list <- furrr::future_pmap(list(state.list), parallel.api,
                                   .options = furrr::furrr_options(seed = NULL))
    future::plan('sequential')

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
                    frac_pov99 = (pov50+pov99)/pop_total,
                    frac_pov199 = (pov50+pov99+pov124+pov149+pov184+pov199) /
                      pov_total,
                    pop_total)]
    rm(convert.cols, cbg.list, cbg.together)

    # Write file for future use
    data.table::fwrite(acs.cbg.data,
           paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                  '/ACS_data/acs_',yr_input,'_ejstats.csv'),
           scipen = 50)

  } else {
    acs.cbg.data <- data.table::fread(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                 '/ACS_data/acs_',yr_input,'_ejstats.csv'),
                          colClasses = c('GEOID'='character'))
  }

  # Filter down to user-requested state
  if (!is.null(state_filter)){
    acs.cbg.data <- acs.cbg.data %>%
      dplyr::filter(state %in% state_filter)
  }

  return(acs.cbg.data)
}
