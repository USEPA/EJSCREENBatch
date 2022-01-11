#' EJ Correlation Plots
#'
#' Creates a correlation plot that quantifies two-way correlations between
#' EJ and demographic indicators.
#'
#' @param data
#' @param gis_method User specified method of creating buffers around areas of interest ("fast", "robust").
#' @param buffer Distance(s) used to create buffers.
#' @param threshold User specified threshold to represent potential concern. Default is 80%.
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
EJCorrPlots <- function(data, gis_method, buffer, threshold, directory){
  step2 <- EJCorrPlotDataProcessing(data, gis_method, buffer, threshold, directory)

      tryCatch(
        {
          step3 <- step2 %>%
            dplyr::select(ID, overlap) %>%
            distinct() %>%
            filter(!is.na(ID)) %>%
            separate_rows(overlap,sep=", ") %>%
            mutate(value=1)  %>%
            filter(overlap!="")  %>%
            pivot_wider(values_from=value, names_from=overlap,  values_fill=0)  %>%
            dplyr::select(-ID) %>%
            dplyr::select(order(colnames(.))) %>%
            cor()

          if(dataset=="demo_indexes"){
             txt.size=480
           } else {
             txt.size=900
           }

          jpeg(file=paste0(directory,"/plots/correlations_",dataset,"_gis_",gis_method,"_radius",buffer,"_",geo_level,".jpeg"), width = txt.size, height = txt.size)
          col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
          plot <- corrplot::corrplot(step3, method="color",
                             order = 'original',
                             type="upper",
                             addCoef.col = "black", # Add coefficient of correlation
                             tl.col="black", tl.srt=45, #Text label color and rotation
                             diag=FALSE, # hide correlation coefficient on the principal diagonal,
                             tl.cex=1.25
          )
          dev.off()

        },
        warning=function(cond){
          message(paste0("Only ",length(unique(step2$ID))," block groups for ",length(unique(step2$shape_ID))," locations.
                         No correlation plots will be outputted"))
        })

    }
  }
  step2 <- suppressMessages(step2_demo_indexes %>%
    rename(potential_issues_count_demo = potential_issues_count,
           overlap_demo = overlap) %>%
    left_join(step2_ej_indexes) %>%
    rename(potential_issues_count_ej = potential_issues_count,
           overlap_ej = overlap))


  return(step2)
}
