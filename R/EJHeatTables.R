#' EJ Heat Tables
#'
#' Produces one of three possible heat tables.
#' 1) Summary of median CBG value across all facilities ('all')
#' 2) Summary of median CBG value for a single facility ('single')
#' 3) Summary of median CBG values for "top N" facilities ('topn')
#'
#' heat_table_geog_lvl denotes whether to use nat'l/state percentiles (nat'l default): options 'US'/'state'
#' when type = 'single, heat_table_input_name must be set by user, denotes the (integer) rowid of the facility
#' topN is an integer set by user when using ranking table. (Default is 5 for fit, min is 0, max is 10.)

#'
#' @param input_data
#' @param heat_table_type  All facilities, single facility, top N facilities. If top N facilities, must specify topN.
#' @param heat_table_geog_lvl state or US
#' @param heat_table_input_name Option to keep row ID number of location
#' @param heat_table_topN Number of locations with highest median CBG values to return in Heat table.
#' @param save_option Option to save heat table to a folder in working directory. Default is FALSE.
#' @param directory
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
#' y1 <- EJHeatTables(input_data = y, heat_table_type = 'topn', heat_table_topN = 5)
#' y2 <- EJHeatTables(input_data = y, heat_table_type = 'all', heat_table_geog_lvl = 'state')
EJHeatTables <- function(input_data, heat_table_type, heat_table_geog_lvl= NULL,
                         heat_table_input_name = NULL, heat_table_topN = NULL,
                         save_option = F, directory, threshold = NULL){



  #set heat table thresholds
  if(!is.null(threshold)){
    if(threshold > 0 & threshold < 100){
      thrshld <- threshold
    } else {
      stop('Set threshold to numeric value between 0 and 100.')
    }
  } else if (is.null(threshold)){
    thrshld <- 80
  }

  # Set default geography level @ nat'l scale
  if(is.null(heat_table_geog_lvl)){
    geog <- 'US'  #default values
  } else if(!(heat_table_geog_lvl %in% c('US', 'state'))){
    stop('Geographies available are "US" and "state".')
  } else {
    geog <- heat_table_geog_lvl  #user inputted values that override default
  }

  if (heat_table_type == 'all'){ # This returns a HeatTable for median CBG value across ALL facilities

    # Preallocate list
    dt <- vector(mode = 'list', length = length(input_data$EJ.facil.data))

    ## This takes the median.
    for (i in 1:length(input_data$EJ.facil.data)){

      if (geog == 'US'){
        df.var.wm <- input_data$EJ.list.data[[i]] %>%
          dplyr::select(P_MINORPCT_US, P_LWINCPCT_US, P_LESHSPCT_US, P_LNGISPCT_US,
                        P_UNDR5PCT_US, P_OVR64PCT_US, P_LDPNT_US, P_VULEOPCT_US,
                        P_DSLPM_US, P_CANCR_US, P_RESP_US, P_PTRAF_US, P_PWDIS_US,
                        P_PNPL_US, P_PRMP_US, P_PTSDF_US, P_OZONE_US,
                        P_PM25_US, ACSTOTPOP, shape_ID) %>% as.data.table()

        for (col in 1:ncol(df.var.wm)){
          colnames(df.var.wm)[col] <-  sub("_US", "", colnames(df.var.wm)[col])
        }
      } else if (geog == 'state') {
        df.var.wm <- input_data$EJ.list.data[[i]] %>%
          dplyr::select(P_MINORPCT_state, P_LWINCPCT_state, P_LESHSPCT_state, P_LNGISPCT_state,
                        P_UNDR5PCT_state, P_OVR64PCT_state, P_LDPNT_state, P_VULEOPCT_state,
                        P_DSLPM_state, P_CANCR_state, P_RESP_state, P_PTRAF_state, P_PWDIS_state,
                        P_PNPL_state, P_PRMP_state, P_PTSDF_state, P_OZONE_state,
                        P_PM25_state,
                        ACSTOTPOP, shape_ID) %>% as.data.table()

        for (col in 1:ncol(df.var.wm)){
          colnames(df.var.wm)[col] <-  sub("_state", "", colnames(df.var.wm)[col])
        }
      } else {
        stop('Geography must be "US" or "state".')
      }

      df.var.wm <- df.var.wm %>%
        dplyr::rename(Lead         = P_LDPNT,
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
        dplyr::select(-c(ACSTOTPOP, shape_ID, 'Demo. Index')) %>%
        as.data.table()

      # Keep list of relevant varnames
      keepnames <- names(df.var.wm)

      ### This takes the median of all CBGs
      df.var.wm <- df.var.wm[, lapply(.SD, median, na.rm = T)
      ][, lapply(.SD, round)]

      # Write to dt.list
      dt[[i]] <- data.table::melt(df.var.wm)[,2]
      names(dt[[i]]) <- paste0(gsub(".*radius(.+)mi.*", "\\1",
                                    names(input_data$EJ.facil.data)[i]),
                               ' mile radius')
    }

    ## Shape into data.table
    dt <- as.data.table(cbind(keepnames,rlist::list.cbind(dt))
    )[1:6, ind.type := 'Demographic'
    ][7:17, ind.type := 'Environmental']

    ## Create the heat table
    heat.table <- flextable::as_grouped_data(dt, groups = 'ind.type') %>%
      flextable::flextable() %>%
      flextable::compose(i = 1, j = 1, value = flextable::as_paragraph(""), part = "header") %>%
      flextable::autofit() %>%
      flextable::align_nottext_col(align = 'center') %>%
      flextable::align_text_col(align = 'left') %>%
      flextable::bg(bg = function(x){
        out <- rep("transparent", length(x))
        out[is.numeric(x) & (x >= (thrshld + .75*(100-thrshld)))] <- "red1"
        out[is.numeric(x) & (x >= (thrshld + (100-thrshld)/2)) &
              (x < (thrshld + .75*(100-thrshld)))] <- "orange1"
        out[is.numeric(x) & x >= thrshld & (x < (thrshld + (100-thrshld)/2))] <- 'yellow1'
        out
      }) %>%
      flextable::compose(j = 2, value = flextable::as_paragraph(''), part = 'head') %>%
      flextable::bold(bold = T, part = 'header') %>%
      flextable::bold(i = 1, j = 1, bold = T, part = "body") %>%
      flextable::bold(i = 8, j = 1, bold = T, part = 'body') %>%
      flextable::colformat_num(big.mark = '') %>%
      flextable::footnote(i = 1, j = 1,
                          value = flextable::as_paragraph(paste0('Color code: Yellow (',
                                                                 thrshld,
                                                                 '-',
                                                                 (thrshld + 0.5*(100-thrshld)),
                                                                 '). Orange (',
                                                                 (thrshld + 0.5*(100-thrshld)),
                                                                 '-',
                                                                 (thrshld + 0.75*(100-thrshld)),
                                                                 '). Red (',
                                                                 (thrshld + 0.75*(100-thrshld)),
                                                                 '-100).'
                                                                 )),
                          part = 'header',
                          ref_symbols = '')
    ## Save if option selected.
    if (save_option == T){
      ifelse(!dir.exists(file.path(directory,"heattabs")),
             dir.create(file.path(directory,"heattabs")), FALSE)
      flextable::save_as_image(x = heat.table, path = paste0(directory,"/heattabs/ht_all.png"))
    }

  } else if (heat_table_type == 'single') { #This returns HeatTable for user-specified facil

    if (is.null(heat_table_input_name) | !is.numeric(heat_table_input_name)){
      stop('Must include a numeric row ID (heat_table_input_name) of single facility for this table type.')
    } else {
      shape.keep <- heat_table_input_name
    }

    # Preallocate list
    dt <- vector(mode = 'list', length = length(input_data$EJ.facil.data))

    # Keep list of relevant varnames
    keepnames <- as.data.table(input_data$EJ.facil.data[[1]]
    )[, dplyr::select(.SD, `Low Income`:`Resp. Hazard`)] %>% names()

    ## This draws from facility level data (median CBG value for that facil)
    for (i in 1:length(input_data$EJ.facil.data)){

      dt[[i]] <- data.table::as.data.table(input_data$EJ.facil.data[[i]]
        )[geography == geog & shape_ID == shape.keep,
          dplyr::select(.SD, `Low Income`:`Resp. Hazard`)
          ][, lapply(.SD, round)]
      dt[[i]] <- data.table::melt(dt[[i]])[,2]
      names(dt[[i]]) <- paste0(gsub(".*radius(.+)mi.*", "\\1",
                                    names(input_data$EJ.facil.data)[i]),
                               ' mile radius')
    }

    ## Shape into data.table
    dt <- as.data.table(cbind(keepnames,rlist::list.cbind(dt))
    )[1:6, ind.type := 'Demographic'
    ][7:17, ind.type := 'Environmental']

    ## Create the heat table
    heat.table <- flextable::as_grouped_data(dt, groups = 'ind.type') %>%
      flextable::flextable() %>%
      flextable::compose(i = 1, j = 1, value = flextable::as_paragraph(""), part = "header") %>%
      flextable::autofit() %>%
      flextable::align_nottext_col(align = 'center') %>%
      flextable::align_text_col(align = 'left') %>%
      flextable::bg(bg = function(x){
        out <- rep("transparent", length(x))
        out[is.numeric(x) & x >= 95] <- "red1"
        out[is.numeric(x) & x >= 90 & x < 95] <- "orange1"
        out[is.numeric(x) & x >= 80 & x < 90] <- 'yellow1'
        out
      }) %>%
      flextable::compose(j = 2, value = flextable::as_paragraph(''), part = 'head') %>%
      flextable::bold(bold = T, part = 'header') %>%
      flextable::bold(i = 1, j = 1, bold = T, part = "body") %>%
      flextable::bold(i = 8, j = 1, bold = T, part = 'body') %>%
      flextable::colformat_num(big.mark = '')

    ## Save if option selected.
    if (save_option == T){
      ifelse(!dir.exists(file.path(directory,"heattabs")),
             dir.create(file.path(directory,"heattabs")), FALSE)
      flextable::save_as_image(x = heat.table, path = paste0(directory,'/heattabs/ht_single_',
                                                             heat_table_input_name,".png"))
    }

  } else if (heat_table_type == 'topn') { #Return HeatTable summary for Top10 facilities

    # How many facilities included in table?
    if(is.null(heat_table_topN)){
      n_rank <-  5 #default values
    } else if (heat_table_topN > 10) {
      print('Table is too large. *heat_table_topN* set to its max value (10).')
      n_rank <- 10   # Nope, 10 is max.
    } else if (heat_table_topN <= 10 & heat_table_topN > 0) {
      n_rank <- round(heat_table_topN) # user inputted values that override default
    } else {
      stop('User-designated value for heat_table_topN must be an integer between 1 and 10')
    }

    heat.table <- list()

    for (k in 1:length(input_data$EJ.facil.data)) {
      # Facility names for merging
      facil.name <- names(input_data$EJ.facil.data[[k]])[1]

      ##
      # Extract nat'l level data, keep only top N
      dt <- as.data.table(input_data$EJ.facil.data[[k]]
      )[geography == geog
      ][, `Total indicators above 80th %ile` :=
          as.numeric(as.character(`Env. indicators above 80th %ile`)) +
          as.numeric(as.character(`Demo. indicators above 80th %ile`))
      ][order(-`Total indicators above 80th %ile`)
      ][1:n_rank,
      ][, dplyr::select(.SD, c(tidyselect::all_of(facil.name),
                               `Low Income`:`Resp. Hazard`))]
      setcolorder(dt, neworder = facil.name)

      # Reshape/transpose data
      new.dt <- data.table(cn = names(dt), data.table::transpose(dt))
      setnames(new.dt, as.character(new.dt[1,]))
      cols <- names(new.dt)[2:(dim(new.dt)[2])]
      new.dt <- new.dt[-1,][, (cols) := lapply(.SD, as.numeric),
                            .SDcols = cols][1:6, ind.type := 'Demographic'
                            ][7:17, ind.type := 'Environmental'
                            ]

      # Create the final table
      ht <- flextable::as_grouped_data(new.dt, groups = 'ind.type') %>%
        flextable::flextable() %>%
        flextable::compose(i = 1, j = 1, value = flextable::as_paragraph(""), part = "header") %>%
        flextable::compose(i = 1, j = 2, value = flextable::as_paragraph(""), part = "header") %>%
        flextable::autofit() %>%
        flextable::align_nottext_col(align = 'center') %>%
        flextable::align_text_col(align = 'left') %>%
        flextable::bg(bg = function(x){
          out <- rep("transparent", length(x))
          out[is.numeric(x) & x >= 95] <- "red1"
          out[is.numeric(x) & x >= 90 & x < 95] <- "orange1"
          out[is.numeric(x) & x >= 80 & x < 90] <- 'yellow1'
          out
        }) %>%
        flextable::compose(j = 2, value = flextable::as_paragraph(''), part = 'head') %>%
        flextable::bold(bold = T, part = 'header') %>%
        flextable::bold(i = 1, j = 1, bold = T, part = "body") %>%
        flextable::bold(i = 8, j = 1, bold = T, part = 'body') %>%
        flextable::colformat_num(big.mark = '')

      heat.table[[stringr::str_sub(names(input_data$EJ.facil.data),
                                   start = 7)[k]]] <- ht

      ## Save if option selected.
      if (save_option == T){
        ifelse(!dir.exists(file.path(directory,"heattabs")),
               dir.create(file.path(directory,"heattabs")), FALSE)
        flextable::save_as_image(x = ht,
                                 path = paste0(directory,"/heattabs/ht_topN_",
                                               stringr::str_sub(names(input_data$EJ.facil.data),
                                                                start = 7)[k],
                                               ".png"))
      }
    }
  } else {
    stop('Table type not valid. Please specify one of "all", "single" OR "topn"')
  }

  return(heat.table)
}
