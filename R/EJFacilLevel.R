#' Title
#'
#' @param list.data
#' @param facil.data
#'
#' @return
#' @export
#'
#' @examples
EJFacilLevel <- function(list.data, facil.data) {

  # Extract key variables, take ***pop-weighted*** average
  df.var.wm <- list.data %>%
    as.data.frame() %>%
    dplyr::select(P_MINORPCT_US, P_LWINCPCT_US, P_LESHSPCT_US, P_LNGISPCT_US,
           P_UNDR5PCT_US, P_OVR64PCT_US, P_LDPNT_US, P_VULEOPCT_US,
           P_DSLPM_US, P_CANCR_US, P_RESP_US, P_PTRAF_US, P_PWDIS_US,
           P_PNPL_US, P_PRMP_US, P_PTSDF_US, P_OZONE_US,
           P_PM25_US, P_MINORPCT_state, P_LWINCPCT_state, P_LESHSPCT_state, P_LNGISPCT_state,
           P_UNDR5PCT_state, P_OVR64PCT_state, P_LDPNT_state, P_VULEOPCT_state,
           P_DSLPM_state, P_CANCR_state, P_RESP_state, P_PTRAF_state, P_PWDIS_state,
           P_PNPL_state, P_PRMP_state, P_PTSDF_state, P_OZONE_state,
           P_PM25_state, ACSTOTPOP, shape_ID
           # Below are EJ "indices" rather than environmental indicators
           #P_LDPNT_D2_US, P_DSLPM_D2_US, P_CANCR_D2_US,
           #P_RESP_D2_US, P_PTRAF_D2_US, P_PWDIS_D2_US, P_PNPL_D2_US,
           #P_PRMP_D2_US, P_PTSDF_D2_US, P_OZONE_D2_US, P_PM25_D2_US,
           #P_LDPNT_D2_state, P_DSLPM_D2_state, P_CANCR_D2_state,
           #P_RESP_D2_state, P_PTRAF_D2_state, P_PWDIS_D2_state, P_PNPL_D2_state,
           #P_PRMP_D2_state, P_PTSDF_D2_state, P_OZONE_D2_state, P_PM25_D2_state,
    ) %>%
    group_by(shape_ID) %>%
    summarize_at(vars(-ACSTOTPOP), funs(median(., na.rm = T))) %>%
    as.data.table()


  df.var.wm <- data.table::melt(df.var.wm, id = 'shape_ID'
    )[, variable := stringi::stri_replace_last_fixed(variable,'_','|')
      ][, c('variable','geography') := data.table::tstrsplit(variable, '|', fixed = T)]

  df.var.wm <- dcast(df.var.wm, shape_ID + geography ~ variable, value.var = "value") %>%
    rename(Lead                = P_LDPNT,
           'Diesel PM'         = P_DSLPM,
           'Air, Cancer'       = P_CANCR,
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
           'Low Income'        = P_LWINCPCT,
           'Less HS Educ'      = P_LESHSPCT,
           'Ling. Isol.'       = P_LNGISPCT,
           'Age Under 5'       = P_UNDR5PCT,
           'Age Over 64'       = P_OVR64PCT)

  # Sum of population w/in 5miles
  df.pop.sum <- list.data %>%
    dplyr::select(ACSTOTPOP, shape_ID) %>%
    rename(`Pop. Count` = ACSTOTPOP) %>%
    group_by(shape_ID) %>%
    summarize_at(vars(`Pop. Count`),funs(sum))

  # Need lat/lon, (previously: URL to the facility's DFR)
  df.latlon <- facil.data %>%
    dplyr::select(shape_ID, geometry)

  # Merge all together
  together.sf <- inner_join(df.var.wm, df.pop.sum, by = "shape_ID") %>%
    inner_join(df.latlon, by = 'shape_ID') %>%
    relocate(shape_ID, `Pop. Count`,
             `Low Income`, `Minority`, `Less HS Educ`, `Ling. Isol.`,
             `Age Under 5`, `Age Over 64`, `Air, Cancer`, `Diesel PM`,
             Lead, Ozone, PM, NPL, `RMP Facility`, Traffic, `TSD Facility`,
             `WW Discharge`, `Resp. Hazard` )

  together.sf <- together.sf %>% mutate(`Env. indicators above 80th %ile` = as.factor(rowSums(dplyr::select(as.data.frame(together.sf),
                                                                                                     `Air, Cancer`:`Resp. Hazard`) > 80))) %>%
    mutate(`Demo. indicators above 80th %ile` = as.factor(rowSums(dplyr::select(as.data.frame(together.sf),
                                                                         `Low Income`:`Age Over 64`) > 80))) %>%
    mutate_if(is.numeric, round)

  return(together.sf)
}
