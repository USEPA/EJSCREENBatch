#' EJ Heat Tables
#'
#' Produces one of three possible heat tables.
#' 1) Summary of median CBG value across all facilities ('all')
#' 2) Summary of median CBG value for a single facility ('single')
#' 3) Summary of median CBG values for "top N" facilities ('topn')
#'
#' geog_lvl denotes whether to use nat'l/state percentiles (nat'l default): options 'US'/'state'
#' when type = 'single, keepid must be set by user, denotes the (integer) rowid of the facility
#' topN is an integer set by user when using ranking table. (Default is 10, min is 0.)
#' topN_dta_idx denotes which EJfunction() output data to use (e.g. when there are mult. buff. distances)
#' -> it is an integer. Default setting is 1
#' input.names is user-provided list of facility names. it must be same length as input_data
#'
#' @param input_data
#' @param type
#' @param geog_lvl
#' @param keepid
#' @param topN
#' @param topN_dta_idx
#' @param save_option
#'
#' @return
#' @export
#'
#' @examples
#' y1 <- EJHeatTables(input_data = y, type = 'topn', topN = 5, topN_dta_idx = 3)
#' y2 <- EJHeatTables(input_data = y, type = 'all', geog_lvl = 'state')
EJHeatTables <- function(input_data, type, geog_lvl= NULL, keepid = NULL, topN = NULL,
                         topN_dta_idx = NULL, input.names = NULL, save_option = F){

  # Set default geography level @ nat'l scale
  if(is.null(geog_lvl)){
    geog <- 'US'  #default values
  } else if(!(geog_lvl %in% c('US', 'state'))){
    stop('Geographies available are "US" and "state".')
  } else {
    geog <- geog_lvl  #user inputted values that override default
  }

  if (type == 'all'){ # This returns a HeatTable for median of ALL facilities

    # Preallocate list
    dt <- vector(mode = 'list', length = length(input_data$EJ.facil.data))

    ## This takes the MEAN
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
      names(dt[[i]]) <- paste0(str_sub(labels(input_data$EJ.facil.data)[[i]],-3,-3),
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
      flextable::bold(i = 8, j = 1, bold = T, part = 'body')

    ## Save if option selected.
    if (save_option == T){
      flextable::save_as_image(x = heat.table, path = "heat_table_all.png")
    }
    
  } else if (type == 'single') { #This returns HeatTable for user-specified facil
    
    if (is.null(keepid) | !is.numeric(keepid)){
      stop('Must include a numeric row ID (keepid) of single facility for this table type.')
    } else {
      shape.keep <- keepid
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
      names(dt[[i]]) <- paste0(str_sub(labels(input_data$EJ.facil.data)[[i]],-3,-3),
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
      flextable::bold(i = 8, j = 1, bold = T, part = 'body')
    
    ## Save if option selected.
    if (save_option == T){
      flextable::save_as_image(x = heat.table, path = "heat_table_single.png")
    }
    
  # } else if (type == 'topn') { #Return HeatTable summary for Top10 facilities
  # 
  #   # How many facilities included in table?
  #   if(is.null(topN)){
  #     n_rank <-  10 #default values
  #   } else if (topN > 10) {
  #     n_rank <- 10   #user inputted values that override default
  #   } else if (topN <= 10 & topN > 0) {
  #     n_rank <- round(topN)
  #   } else {
  #     stop('User-designated value for topN must be an integer between 1 and 10')
  #   }
  # 
  #   # When EJfunction data has more than 1 buffer dist, which DF to use?
  #   if(is.null(topN_dta_idx)){
  #     dta.idx <- 1 # Default is the first list element in ...$EJ.facil.list
  #   } else if((topN_dta_idx > length(input_data$EJ.facil.data)) |
  #             !is.numeric(topN_dta_idx)){
  #     stop('Error: index provided is non-numeric or exceeds length of list.')
  #   } else {
  #     dta.idx <- topN_dta_idx
  #   }
  # 
  #   ##
  #   # Extract nat'l level data, keep only top N
  #   if (!is.null(input.names)){
  #     if(length(input.names) != dim(input_data$EJ.facil.data[[dta.idx]])[1]/2){
  #       stop('List of names but be same length as list of input geometries.')
  #     } else {
  #       dt <- as.data.table(input_data$EJ.facil.data[[dta.idx]]
  #       )[geography == geog
  #       ][, `Total indicators above 80th %ile` :=
  #           as.numeric(as.character(`Env. indicators above 80th %ile`)) +
  #           as.numeric(as.character(`Demo. indicators above 80th %ile`))
  #       ][, `NPDES Permit Number` :=
  #           as.vector(facilities$`NPDES Permit Number`)
  #       ][order(-`Total indicators above 80th %ile`)
  #       ][1:n_rank,
  #       ][, dplyr::select(.SD, c(`Low Income`:`Resp. Hazard`, `NPDES Permit Number`))]
  #       setcolorder(dt, neworder = 'NPDES Permit Number')
  #     }
  #   } else {
  #     dt <- as.data.table(input_data$EJ.facil.data[[dta.idx]]
  #     )[geography == geog
  #     ][, `Total indicators above 80th %ile` :=
  #         as.numeric(as.character(`Env. indicators above 80th %ile`)) +
  #         as.numeric(as.character(`Demo. indicators above 80th %ile`))
  #     ][order(-`Total indicators above 80th %ile`)
  #     ][1:n_rank,
  #     ][, dplyr::select(.SD, c(shape_ID, `Low Income`:`Resp. Hazard`))]
  #     setcolorder(dt, neworder = 'shape_ID')
  #   }
  #   # Reshape/transpose data
  #   new.dt <- data.table(cn = names(dt), data.table::transpose(dt))
  #   setnames(new.dt, as.character(new.dt[1,]))
  #   cols <- names(new.dt)[2:(dim(new.dt)[2])]
  #   new.dt <- new.dt[-1,][, (cols) := lapply(.SD, as.numeric),
  #                         .SDcols = cols][1:6, ind.type := 'Demographic'
  #                         ][7:17, ind.type := 'Environmental'
  #                         ]
  # 
  #   # Create the final table
  #   heat.table <- flextable::as_grouped_data(new.dt, groups = 'ind.type') %>%
  #     flextable::flextable() %>%
  #     flextable::compose(i = 1, j = 1, value = flextable::as_paragraph(""), part = "header") %>%
  #     flextable::compose(i = 1, j = 2, value = flextable::as_paragraph(""), part = "header") %>%
  #     flextable::autofit() %>%
  #     flextable::align_nottext_col(align = 'center') %>%
  #     flextable::align_text_col(align = 'left') %>%
  #     bg(bg = function(x){
  #       out <- rep("transparent", length(x))
  #       out[is.numeric(x) & x >= 95] <- "red1"
  #       out[is.numeric(x) & x >= 90 & x < 95] <- "orange1"
  #       out[is.numeric(x) & x >= 80 & x < 90] <- 'yellow1'
  #       out
  #     }) %>%
  #     flextable::compose(j = 2, value = as_paragraph(''), part = 'head') %>%
  #     flextable::bold(bold = T, part = 'header') %>%
  #     flextable::bold(i = 1, j = 1, bold = T, part = "body") %>%
  #     flextable::bold(i = 8, j = 1, bold = T, part = 'body')
  # 
  #   ## Save if option selected.
  #   if (save_option == T){
  #     flextable::save_as_image(x = heat.table, path = "heat_table_top10.png")
  #   }

  } else {
    stop('Table type not valid. Please specify one of "all", "single" OR "topn"')
  }

  return(heat.table)
}
