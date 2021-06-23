#' EJ Correlation Plots
#'
#' Creates a correlation plot that quantifies two-way correlations between
#' EJ and demographic indicators.
#'
#' @param data
#' @param gis_method User specified method of creating buffers around areas of interest (intersect, centroid, intersection).
#' @param buffer Distance(s) used to create buffers.
#' @param threshold User specified threshold to represent potential concern. Default is 80\%.
#'
#' @return
#' @export
#'
#' @examples
EJCorrPlots <- function(data, gis_method, buffer, threshold){
  ifelse(!dir.exists(file.path(getwd(),"plots")), dir.create(file.path(getwd(),"plots")), FALSE)

  exceed.threshold <- function(x) {
    ifelse(as.numeric(x)>threshold, 1, 0)
  }

  replace.zeros <- function(x) {
    ifelse(x==0, NA, x)
  }

  demo_indexes <- data %>%
    as.data.frame() %>%
    dplyr::select(ID, STATE_NAME, ST_ABBREV, P_MINORPCT_state, P_LWINCPCT_state,
                  P_LESHSPCT_state, P_LNGISPCT_state, P_UNDR5PCT_state, P_OVR64PCT_state) %>%
    rename(
      Minority=P_MINORPCT_state,
      Low_Income=P_LWINCPCT_state,
      Less_than_HS_Edu=P_LESHSPCT_state,
      Linguistic_Isolation=P_LNGISPCT_state,
      Age_Under_5=P_UNDR5PCT_state,
      Age_Over_64=P_OVR64PCT_state) %>%
    # mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),as.numeric) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),exceed.threshold) %>%
    mutate(potential_issues_count = rowSums(dplyr::select(., -c("ID","STATE_NAME","ST_ABBREV")))) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),replace.zeros)


  ej_indexes <- data %>%
    as.data.frame() %>%
    dplyr::select(ID, STATE_NAME, ST_ABBREV, P_LDPNT_state, P_DSLPM_state, P_CANCR_state,
                  P_RESP_state, P_PTRAF_state, P_PWDIS_state, P_PNPL_state,
                  P_PRMP_state, P_PTSDF_state, P_OZONE_state, P_PM25_state, P_VULEOPCT_state) %>%
    rename(
      Lead_Paint=P_LDPNT_state,
      Diesel_PM=P_DSLPM_state,
      Air_Toxics_Cancer_Risk=P_CANCR_state,
      Air_Toxics_Respiratory_Hazard=P_RESP_state,
      Traffic_Proximity=P_PTRAF_state,
      Major_WW_Dischargers=P_PWDIS_state,
      Nation_Priorities_List=P_PNPL_state,
      Risk_Mgmt_Plan_Facilities=P_PRMP_state,
      Treatment_Storage_Disposal_Facilities=P_PTSDF_state,
      Ozone_Level=P_OZONE_state,
      PM=P_PM25_state,
      Demographic_Index=P_VULEOPCT_state) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),exceed.threshold) %>%
    mutate(potential_issues_count = rowSums(dplyr::select(., -c("ID","STATE_NAME","ST_ABBREV")))) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),replace.zeros)

  datasets <- c("demo_indexes", "ej_indexes")
  for(dataset in datasets){
    step1 <- get(dataset)


    w <- which(step1==1,arr.ind=TRUE)
    step1[w] <- names(step1)[w[,"col"]]

    `%notin%` = Negate(`%in%`)
    colsNOT2paste <- c("ID","STATE_NAME","ST_ABBREV", "potential_issues_count", "overlap")
    step1$overlap <- do.call(paste, c(step1[, which(names(step1) %notin% colsNOT2paste)], sep=","))

    step1.1 <- step1[, which(names(step1) %notin% colsNOT2paste)]
    step1$overlap <- apply(step1.1, 1, function(x) toString(na.omit(x)))

    step2 <- step1 %>%
      dplyr::select(ID,STATE_NAME,ST_ABBREV,potential_issues_count, overlap) %>%
      mutate(potential_issues_count = ifelse(is.na(potential_issues_count),0, potential_issues_count),
             potential_issues_count = ifelse(potential_issues_count=="potential_issues_count",1,potential_issues_count)) %>%
      filter(!is.na(ID))

    assign(paste0("step2_",dataset),step2)

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
          cor()

        jpeg(file=paste0("plots/correlations_",dataset,"_gis_",gis_method,"_radius",buffer,".jpeg"))
        col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
        corrplot::corrplot(step3, method="color",
                 type="upper", order="hclust",
                 addCoef.col = "black", # Add coefficient of correlation
                 tl.col="black", tl.srt=45, #Text label color and rotation
                 diag=FALSE # hide correlation coefficient on the principal diagonal
        )
        dev.off()

      },
      warning=function(cond){
        message(paste0("Only ",length(unique(step2$ID))," block groups for ",length(unique(step2$shape_ID))," locations.
                       No correlation plots will be outputted"))
      })

  }

  step2 <- step2_demo_indexes %>%
    rename(potential_issues_count_demo = potential_issues_count,
           overlap_demo = overlap) %>%
    left_join(step2_ej_indexes) %>%
    rename(potential_issues_count_ej = potential_issues_count,
           overlap_ej = overlap)


  return(step2)
}
