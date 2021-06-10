#' EJ Ranking Function
#'
#' Returns information for facilities or census block groups ranked by the number
#' of indicators that exceed the 80th percentile.
#'
#' @param input_data
#' @param rank_type
#' @param geography_type
#' @param rank_count
#' @param save.option
#'
#' @return
#' @export
#'
#' @examples
#' # Facility and CBG rankings
#' facil.ranking <- EJRanking(input_data = a2, rank_count = 10, rank_type = 'location',
#'                             geography_type = 'US', save.option = F)
#'
#' cbg.ranking <- EJRanking(input_data = a2, rank_type = 'cbg')
EJRanking <- function(input_data, rank_type = 'location', geography_type = 'US',
                      rank_count = 10, save.option = F){

  if (!(geography_type %in% c('US','state'))){
    stop('Geography type must be either -US- or -state-.')
  }

  if (rank_type == 'location'){

    # Create an empty list for rankings (one for each dist and buffer method)
    data_transf <- list()

    for (i in 1:length(input_data$EJ.facil.data)){
      if (rank_count > (dim(input_data$EJ.facil.data[[i]])[1]/2)){
        stop('Ranking list length can be no longer than location list.')
      } else {
        locay <- input_data$EJ.facil.data[[i]] %>%
          filter(geography == geography_type) %>%
          mutate(`Total indicators above 80th %ile` =
                   as.numeric(as.character(`Env. indicators above 80th %ile`)) +
                   as.numeric(as.character(`Demo. indicators above 80th %ile`))) %>%
          arrange(desc(`Total indicators above 80th %ile`),
                  desc(`Env. indicators above 80th %ile`)) %>%
          dplyr::select(shape_ID,
                 `Total indicators above 80th %ile`,
                 `Env. indicators above 80th %ile`,
                 `Demo. indicators above 80th %ile`) %>%
          mutate(`Env. indicators above 80th %ile` =
                   as.numeric(as.character(`Env. indicators above 80th %ile`))) %>%
          mutate(`Demo. indicators above 80th %ile` =
                   as.numeric(as.character(`Demo. indicators above 80th %ile`))) %>%
          rename(`location ID` = shape_ID) %>%
          slice_head(n = rank_count)

        data_transf[[names(input_data$EJ.facil.data)[i]]] <- flextable(locay) %>%
          theme_zebra() %>%
          set_table_properties(layout='autofit', width = .3)

        if (save.option == T){
          ifelse(!dir.exists(file.path(getwd(),"ranktables/")),
                 dir.create(file.path(getwd(),"ranktables/")), FALSE)
          save_as_image(x = data_transf[[names(input_data$EJ.facil.data)[i]]],
                        path = paste0('ranktables/loca_',names(input_data$EJ.facil.data)[i], ".png"))
        }
      }
    }
  } else if (rank_type == 'cbg'){

    # Create an empty list for rankings (one for each dist and buffer method)
    data_transf <- list()

    for (i in 1:length(input_data$EJ.list.data)){
      if (rank_count > (dim(input_data$EJ.list.data[[i]])[1]/2)){
        stop('Ranking list length can be no longer than location list.')
      } else {
        cbg <- input_data$EJ.list.data[[i]] %>%
          as.data.frame() %>%
          dplyr::select(P_MINORPCT_US, P_LWINCPCT_US, P_LESHSPCT_US, P_LNGISPCT_US,
                 P_UNDR5PCT_US, P_OVR64PCT_US, P_LDPNT_US, P_VULEOPCT_US,
                 P_DSLPM_US, P_CANCR_US, P_RESP_US, P_PTRAF_US, P_PWDIS_US,
                 P_PNPL_US, P_PRMP_US, P_PTSDF_US, P_OZONE_US,
                 P_PM25_US, P_MINORPCT_state, P_LWINCPCT_state, P_LESHSPCT_state, P_LNGISPCT_state,
                 P_UNDR5PCT_state, P_OVR64PCT_state, P_LDPNT_state, P_VULEOPCT_state,
                 P_DSLPM_state, P_CANCR_state, P_RESP_state, P_PTRAF_state, P_PWDIS_state,
                 P_PNPL_state, P_PRMP_state, P_PTSDF_state, P_OZONE_state,
                 P_PM25_state, ID) %>%
          as.data.table()

        cbg <- melt(unique(cbg), id = 'ID'
        )[, variable := stri_replace_last_fixed(variable,'_','|')
        ][, c('variable','geography') := tstrsplit(variable, '|', fixed = T)
        ][!is.na(ID)]

        cbg <- dcast(cbg, ID + geography ~ variable, value.var = "value") %>%
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
                 'Age Over 64'       = P_OVR64PCT) %>%
          relocate(ID, `Low Income`, `Minority`, `Less HS Educ`, `Ling. Isol.`,
                   `Age Under 5`, `Age Over 64`, `Air, Cancer`, `Diesel PM`,
                   Lead, Ozone, PM, NPL, `RMP Facility`, Traffic, `TSD Facility`,
                   `WW Discharge`, `Resp. Hazard` )

         cbg <- cbg %>%
          mutate(`Env. indicators above 80th %ile` = rowSums(dplyr::select(as.data.frame(cbg),
                                                                    `Air, Cancer`:`Resp. Hazard`) > 80)) %>%
          mutate(`Demo. indicators above 80th %ile` = rowSums(dplyr::select(as.data.frame(cbg),
                                                                     `Low Income`:`Age Over 64`) > 80)) %>%
          mutate_if(is.numeric, round) %>%
          mutate(`Total indicators above 80th %ile` =
                   `Env. indicators above 80th %ile` +
                   `Demo. indicators above 80th %ile`) %>%
          arrange(desc(`Total indicators above 80th %ile`)) %>%
          filter(geography == geography_type) %>%
          arrange(desc(`Total indicators above 80th %ile`),
                  desc(`Env. indicators above 80th %ile`)) %>%
          dplyr::select(ID,
                 `Total indicators above 80th %ile`,
                 `Env. indicators above 80th %ile`,
                 `Demo. indicators above 80th %ile`) %>%
          rename(`CBG code` = ID) %>%
          slice_head(n = rank_count)

        data_transf[[names(input_data$EJ.list.data)[i]]] <- flextable(cbg) %>%
          theme_zebra() %>%
          set_table_properties(layout='autofit', width = .3)

        if (save.option == T){
          ifelse(!dir.exists(file.path(getwd(),"ranktables/")),
                 dir.create(file.path(getwd(),"ranktables/")), FALSE)
          save_as_image(x = data_transf[[names(input_data$EJ.list.data)[i]]],
                        path = paste0('ranktables/cbg_',names(input_data$EJ.list.data)[i], ".png"))
        }
      }
    }
  } else {
    stop('Rank type must be -location- OR -cbg-.')
  }

  # Return the list
  return(data_transf)
}



