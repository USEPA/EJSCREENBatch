#' Title
#'
#' @param list_data
#' @param facil_data
#' @param ejscreen_data
#' @param acs_data
#' @param thrshld
#'
#' @return
#' @export
#'
#' @examples
EJFacilLevel <- function(list_data, facil_data, ejscreen_data, acs_data, thrshld) {

  #Key variables
  rename_cols <- c("PM25","OZONE","DSLPM","CANCER","RESP","PTRAF","PNPL",
                   "PRMP","PRE1960PCT","PTSDF","PWDIS","VULEOPCT","MINORPCT",
                   "LOWINCPCT","UNDER5PCT","LESSHSPCT","OVER64PCT","LINGISOPCT",
                   "med_inc","frac_white","frac_black","frac_amerind","frac_asian",          
                   "frac_pacisl","frac_hisp","frac_pov50","frac_pov99")
  
  #Calculate modal value
  mode <- function(codes){
    names(which.max(table(codes)))
  }  
  
  # Extract key variables, take ***pop-weighted*** average
  df.var.wm <-list_data %>%
    as.data.frame() %>%
    dplyr::select(shape_ID, ACSTOTPOP, PM25, OZONE, DSLPM, CANCER,
                  RESP, PTRAF, PNPL, PRMP, PRE1960PCT, PTSDF, PWDIS,
                  VULEOPCT, MINORPCT, LOWINCPCT, UNDER5PCT,LESSHSPCT,
                  OVER64PCT, LINGISOPCT, med_inc, frac_white, frac_black,
                  frac_amerind, frac_asian, frac_pacisl, frac_hisp, 
                  frac_pov50, frac_pov99) %>%
    dplyr::group_by(shape_ID) %>%
    dplyr::summarize(across(PM25:frac_pov99, ~weighted.mean(., w = ACSTOTPOP, na.rm = T)))
  
  #Modal state abbrev for each facil
  df.var.state <- list_data %>%
    dplyr::select(shape_ID, ST_ABBREV) %>%
    dplyr::mutate(ST_ABBREV = as.factor(ST_ABBREV)) %>%
    dplyr::group_by(shape_ID) %>%
    dplyr::summarize(ST_ABBREV = mode(ST_ABBREV)) %>%
    dplyr::distinct()
  
  #Rejoin, then calculate nat'l percentiles
  df.var.wm <- df.var.wm %>%
    dplyr::left_join(df.var.state, by = 'shape_ID') %>%
    dplyr::mutate(across(PM25:LINGISOPCT,
                         list(~round(ecdf(ejscreen_data %>%
                                            as.data.frame() %>%
                                            dplyr::select(cur_column()) %>%
                                            unlist() %>%
                                            as.numeric())(.)*100
                                     ,0)),
                         # list(~ntile(., 100)),
                         .names="P_{.col}_US")) %>%
    dplyr::mutate(across(med_inc:frac_pov99,
                         list(~round(ecdf(acs_data %>%
                                            as.data.frame() %>%
                                            dplyr::select(cur_column()) %>%
                                            unlist() %>%
                                            as.numeric())(.)*100
                                     ,0)),
                         .names="P_{.col}_US"))
  
  #Calculate state percentiles
  states <- na.omit(unique(df.var.wm$ST_ABBREV))
  temp_state <- lapply(states, function(x){
    ti2 <- df.var.wm %>%
      dplyr::filter(ST_ABBREV==x) %>%
      dplyr::filter(!is.na(shape_ID))  %>%
      dplyr::mutate(across(PM25:LINGISOPCT,
                           list(~round(ecdf(na.omit(ejscreen_data %>%
                                                      as.data.frame() %>%
                                                      dplyr::filter(ST_ABBREV==x) %>%
                                                      dplyr::select(cur_column()) %>%
                                                      unlist() %>%
                                              as.numeric()))(.)*100
                                       ,0)),
                           .names="P_{.col}_state")) %>%
      dplyr::mutate(across(med_inc:frac_pov99,
                           list(~round(ecdf(na.omit(acs_data %>%
                                                      as.data.frame() %>%
                                                      dplyr::filter(state==x) %>%
                                                      dplyr::select(cur_column()) %>%
                                                      unlist() %>%
                                              as.numeric()))(.)*100
                                       ,0)),
                           .names="P_{.col}_state"))
  })
  
  df.var.wm <- data.table::rbindlist(temp_state) %>%
    dplyr::rename_at(vars(rename_cols), ~paste0('P_',all_of(rename_cols),'_raw')) %>%
    tidyr::pivot_longer(cols=starts_with("P_"),
                 names_to="variable",
                 values_to = "value") %>%
    dplyr::mutate(variable=stringi::stri_replace_last_fixed(variable,'_','.')) %>%
    tidyr::separate(variable, into=c("variable","geography"), sep="\\.",extra="merge", fill = "left")   %>%
    tidyr::pivot_wider(names_from = c(variable)) %>%
    dplyr::rename(Lead         = P_PRE1960PCT,
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
           'Asian (%)'         = P_frac_asian,
           'Pac. Isl (%)'      = P_frac_pacisl,
           'Hispanic (%)'      = P_frac_hisp,
           '<50% P.L. (%)'     = P_frac_pov50,
           '<100% P.L. (%)'    = P_frac_pov99) %>%
    dplyr::select(-ST_ABBREV)

  # Sum of population w/in 5miles
  df.pop.sum <- list_data %>%
    dplyr::select(ACSTOTPOP, shape_ID) %>%
    dplyr::rename(`Pop. Count` = ACSTOTPOP) %>%
    dplyr::group_by(shape_ID) %>%
    dplyr::summarize_at(vars(`Pop. Count`),funs(sum))

  # Need lat/lon, (previously: URL to the facility's DFR)
  df.latlon <- facil_data %>%
    dplyr::select(shape_ID, geometry) %>%
    sf::st_transform(crs = 4326)

  # Merge all together
  together.sf <- dplyr::inner_join(df.var.wm, df.pop.sum, by = "shape_ID") %>%
    dplyr::inner_join(df.latlon, by = 'shape_ID') %>%
    dplyr::relocate(shape_ID, `Pop. Count`,
             `Low Income`, `Minority`, `Less HS Educ`, `Ling. Isol.`,
             `Age Under 5`, `Age Over 64`, `Air, Cancer`, `Diesel PM`,
             Lead, Ozone, PM, NPL, `RMP Facility`, Traffic, `TSD Facility`,
             `WW Discharge`, `Resp. Hazard` )

  together.sf <- together.sf %>% 
    dplyr::mutate(!!paste0('Env. indicators above ',thrshld,'th %ile') :=
                    as.factor(rowSums(dplyr::select(as.data.frame(together.sf),
                                                    `Air, Cancer`:`Resp. Hazard`) > thrshld))) %>%
    dplyr::mutate(!!paste0('Demo. indicators above ',thrshld,'th %ile') :=
                    as.factor(rowSums(dplyr::select(as.data.frame(together.sf),
                                                    `Low Income`:`Age Over 64`) > thrshld)))

  return(together.sf)
}
