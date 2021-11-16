#' EJ Count Table
#'
#' Creates a two-way table that summarizes the number and percentage of locations
#' to examine overlap in demographic and environmental indicators of concern.
#'
#' @param input_data
#' @param save_option
#'
#' @return
#' @export
#'
#' @examples
#' count.table <- EJCountTable(input_data = z, save_option = F)
EJCountTable <- function(input_data, save_option = F){
  
  EJ.count.tables <- list()
  for (i in 1:length(input_data$EJ.facil.data)){
    EJ.count.tables[[stringr::str_sub(names(input_data$EJ.facil.data), 
                                      start = 7)[i]]] <-
      flextable::proc_freq(filter(input_data$EJ.facil.data[[i]], geography == 'US'), 'Env. indicators above 80th %ile', 'Demo. indicators above 80th %ile',
                include.table_percent = T, include.row_percent = F,
                include.column_percent = F, include.row_total = F,
                include.column_total = F) %>%
      flextable::compose(j = 2, value = flextable::as_paragraph(''), part = 'head')

    if (save_option == T){
      ifelse(!dir.exists(file.path(getwd(),"counttables/")),
             dir.create(file.path(getwd(),"counttables/")), FALSE)
      flextable::save_as_image(x = EJ.count.tables[[stringr::str_sub(names(input_data$EJ.facil.data), 
                                                                     start = 7)[i]]],
                    path = paste0('counttables/ct_',
                                  stringr::str_sub(names(input_data$EJ.facil.data), 
                                                   start = 7)[i], ".png"))
    }
  }

  return(EJ.count.tables)
}
