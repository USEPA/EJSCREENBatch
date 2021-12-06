#' EJ Ranking Function
#'
#' Returns information for facilities or census block groups ranked by the number
#' of indicators that exceed the 80th percentile.
#'
#' @param input_data
#' @param rank_type 'location' or 'CBG'. Default is 'location'.
#' @param rank_count Number of locations or CBGs to return. Default is 10.
#' @param rank_geography_type State or US. Default is US.
#' @param save_option Option to save rank table to a folder in working directory. Default is FALSE.
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
#' # Facility and CBG rankings
#' facil.ranking <- EJRanking(input_data = a2, rank_count = 10, rank_type = 'location',
#'                             rank_geography_type = 'US', save_option = F)
#'
#' cbg.ranking <- EJRanking(input_data = a2, rank_type = 'cbg')
EJRanking <- function(input_data, rank_type = 'location', rank_geography_type = 'US',
                      rank_count = 10, save_option = F, directory){

  `%notin%` = Negate(`%in%`)
  if (!(rank_geography_type %in% c('US','state'))){
    stop('Geography type must be either -US- or -state-.')
  }



  # Searching the variable name string by character index to extract threshold
  thrshld <- as.numeric(
    substr(
      names(input_data$EJ.facil.data[[1]])[length(names(input_data$EJ.facil.data[[1]]))],
      24,
      str_length(names(input_data$EJ.facil.data[[1]])[length(names(input_data$EJ.facil.data[[1]]))])-7
    )
  )

  if (rank_type == 'location'){

    # Create an empty list for rankings (one for each dist and buffer method)
    data_transf <- list()

    for (i in 1:length(input_data$EJ.facil.data)){

      if (rank_count > (dim(input_data$EJ.facil.data[[i]])[1]/2)){
        stop('Ranking list length can be no longer than location list.')
      }

      # Use name or shape_ID?
      keep.id <- names(input_data$EJ.facil.data[[i]])[1]

      locay <- batch.datalist$EJ.facil.data$facil_intersect_radius3mi %>%
        as.data.frame() %>%
        dplyr::filter(geography == rank_geography_type) %>%
        dplyr::mutate_at(vars(dplyr::ends_with('%ile')), funs(as.integer(as.character(.)))) %>%
        dplyr::mutate(!!paste0('Total indicators above ',thrshld,'th %ile') :=
                        rowSums(select(., dplyr::ends_with('%ile')))) %>%
        dplyr::arrange_at(vars(dplyr::starts_with('Total indicators'),
                               dplyr::starts_with('Env. indicators')),
                          desc) %>%
        dplyr::select_if(names(.) %in% c(keep.id,
                                         paste0('Total indicators above ',thrshld,'th %ile') ,
                                         paste0('Env. indicators above ',thrshld,'th %ile') ,
                                         paste0('Demo. indicators above ',thrshld,'th %ile') )) %>%
        dplyr::mutate(Rank = row_number()) %>%
        dplyr::relocate(Rank) %>%
        dplyr::slice_head(n = rank_count)

      data_transf[[stringr::str_sub(names(input_data$EJ.facil.data),
                                    start = 7)[i]]] <- flextable::flextable(locay) %>%
        flextable::theme_zebra() %>%
        flextable::set_table_properties(layout='autofit', width = .3) %>%
        flextable::colformat_num(big.mark = '')

      if (save_option == T){
        ifelse(!dir.exists(file.path(directory,"ranktables")),
               dir.create(file.path(directory,"ranktables")), FALSE)
        flextable::save_as_image(x = data_transf[[stringr::str_sub(names(input_data$EJ.facil.data),
                                                                   start = 7)[i]]],
                                 path = paste0(directory,'/ranktables/location_',
                                               rank_geography_type, '_',
                                               stringr::str_sub(names(input_data$EJ.facil.data),
                                                                start = 7)[i],".png"))
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
          data.table::as.data.table()

        cbg <- data.table::melt(unique(cbg), id = 'ID'
        )[, variable := stringi::stri_replace_last_fixed(variable,'_','|')
        ][, c('variable','geography') := data.table::tstrsplit(variable, '|', fixed = T)
        ][!is.na(ID)]

        cbg <- data.table::dcast(cbg, ID + geography ~ variable, value.var = "value") %>%
          dplyr::rename(Lead                = P_LDPNT,
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
          dplyr::relocate(ID, `Low Income`, `Minority`, `Less HS Educ`, `Ling. Isol.`,
                   `Age Under 5`, `Age Over 64`, `Air, Cancer`, `Diesel PM`,
                   Lead, Ozone, PM, NPL, `RMP Facility`, Traffic, `TSD Facility`,
                   `WW Discharge`, `Resp. Hazard` )

         cbg <- cbg %>%
          dplyr::mutate(!!paste0('Env. indicators above ',thrshld,'th %ile')  := rowSums(dplyr::select(as.data.frame(cbg),
                                                                    `Air, Cancer`:`Resp. Hazard`) > thrshld)) %>%
          dplyr::mutate(!!paste0('Demo. indicators above ',thrshld,'th %ile')  := rowSums(dplyr::select(as.data.frame(cbg),
                                                                     `Low Income`:`Age Over 64`) > thrshld)) %>%
          dplyr::mutate_if(is.numeric, round) %>%
          dplyr::mutate(!!paste0('Total indicators above ',thrshld,'th %ile') :=
                           rowSums(select(., ends_with('%ile'))))%>%
          dplyr::filter(geography == rank_geography_type) %>%
          dplyr::arrange_at(vars(dplyr::starts_with('Total indicators'),
                                 dplyr::starts_with('Env. indicators')),
                             dplyr::desc) %>%
          dplyr::select(ID,
                        paste0('Total indicators above ',thrshld,'th %ile') ,
                        paste0('Env. indicators above ',thrshld,'th %ile') ,
                        paste0('Demo. indicators above ',thrshld,'th %ile') ) %>%
          dplyr::rename(`CBG code` = ID) %>%
          dplyr::slice_head(n = rank_count)

        data_transf[[stringr::str_sub(names(input_data$EJ.facil.data),
                                      start = 7)[i]]] <- flextable::flextable(cbg) %>%
          flextable::theme_zebra() %>%
          flextable::set_table_properties(layout='autofit', width = .3) %>%
          flextable::colformat_num(big.mark = '')

        if (save_option == T){
          ifelse(!dir.exists(file.path(directory,"ranktables")),
                 dir.create(file.path(directory,"ranktables")), FALSE)
          flextable::save_as_image(x = data_transf[[stringr::str_sub(names(input_data$EJ.facil.data),
                                                                     start = 7)[i]]],
                        path = paste0(directory,'/ranktables/cbg_',
                                      rank_geography_type, '_',
                                      stringr::str_sub(names(input_data$EJ.facil.data),
                                                       start = 7)[i],".png"))
        }
      }
    }
  } else {
    stop('Rank type must be -location- OR -cbg-.')
  }

  # Return the list
  return(data_transf)
}



