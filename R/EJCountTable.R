#' EJ Count Table
#'
#' Creates a two-way table that summaries the number and percentage of locations
#' to examine overlap in demographic and environmental indicators of concern.
#'
#' @param input.data
#' @param save.option
#'
#' @return
#' @export
#'
#' @examples
#' count.table <- EJCountTable(input.data = z, save.option = F)
EJCountTable <- function(input.data, save.option = F){
  EJ.count.tables <- list()

  for (i in 1:length(input.data$EJ.facil.data)){
    EJ.count.tables[[names(input.data$EJ.facil.data)[i]]] <-
      flextable::proc_freq(filter(input.data$EJ.facil.data[[i]], geography == 'US'), 'Env. indicators above 80th %ile', 'Demo. indicators above 80th %ile',
                include.table_percent = T, include.row_percent = F,
                include.column_percent = F, include.row_total = F,
                include.column_total = F) %>%
      flextable::compose(j = 2, value = as_paragraph(''), part = 'head')

    if (save.option == T){
      ifelse(!dir.exists(file.path(getwd(),"counttables/")),
             dir.create(file.path(getwd(),"counttables/")), FALSE)
      flextable::save_as_image(x = EJ.count.tables[[names(input.data$EJ.facil.data)[i]]],
                    path = paste0('counttables/',names(input.data$EJ.facil.data)[i], ".png"))
    }
  }

  return(EJ.count.tables)
}
