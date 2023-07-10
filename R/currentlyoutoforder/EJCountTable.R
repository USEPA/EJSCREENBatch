#' EJ Count Table
#'
#' Creates a two-way table that summarizes the number and percentage of locations
#' to examine overlap in demographic and environmental indicators of concern.
#'
#' @param input_data Required.
#' @param geography_type State or US. Default is US.
#' @param save_option Option to save rank table to a folder in working directory. Default is FALSE.
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
#' count.table <- EJCountTable(input_data = z, save_option = F)
EJCountTable <- function(input_data, geography_type = 'US',
                         directory, save_option = F){

  # #check whether user-requested working directory exists
  # if(!is.null(directory)){
  #   if(dir.exists(directory) == FALSE){
  #     stop("Working directory requested by user does not exist. Check directory name.")
  #   }
  # } else {
  #   directory <- getwd()
  # }

  EJ.count.tables <- list()
  for (i in 1:length(input_data$EJ.facil.data)){
    varname1 <- input_data$EJ.facil.data[[i]] %>%
      dplyr::select(dplyr::starts_with('Env. indicators')) %>%
      names()
    varname2 <- input_data$EJ.facil.data[[i]] %>%
      dplyr::select(dplyr::starts_with('Demo. indicators')) %>%
      names()
    EJ.count.tables[[stringr::str_sub(names(input_data$EJ.facil.data),
                                      start = 7)[i]]] <-
      flextable::proc_freq(filter(input_data$EJ.facil.data[[i]], geography == geography_type),
                           varname1, varname2,
                include.table_percent = T, include.row_percent = F,
                include.column_percent = F, include.row_total = F,
                include.column_total = F) %>%
      flextable::compose(j = 2, value = flextable::as_paragraph(''), part = 'head') %>%
      flextable::footnote(i = 1, j = 1,
                          value = flextable::as_paragraph(paste0('Values generated using a buffer distance of ',
                                         gsub(".*radius(.+)mi.*", "\\1",
                                              names(input_data$EJ.facil.data)[i]),
                                         ' miles.')),
                          part = 'header',
                          ref_symbols = '')

    if (save_option == T){
      ifelse(!dir.exists(file.path(directory,"counttables")),
             dir.create(file.path(directory,"counttables")), FALSE)
      flextable::save_as_image(x = EJ.count.tables[[stringr::str_sub(names(input_data$EJ.facil.data),
                                                                     start = 7)[i]]],
                    path = paste0(directory,'/counttables/ct_',
                                  geography_type, '_',
                                  stringr::str_sub(names(input_data$EJ.facil.data),
                                                   start = 7)[i], ".png"))
    }
  }

  return(EJ.count.tables)
}
