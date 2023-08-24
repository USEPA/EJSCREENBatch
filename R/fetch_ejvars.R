#' Support function to compile EJSCREEN variable names for data vintage
#'
#' @param year Vintage of EJSCREEN data
#'
#' @return List of variable names and other metadata for use by EJfunction()
#' @export
#'
#' @examples
fetch_ejvars <- function(year = NULL){
  if(is.null(year)){
    latestavailableyear <- function(mypath){
      calendaryear <- as.numeric(format(Sys.time(), "%Y"))
      yrschecked <- 2015:calendaryear
      temp1 <-  lapply(paste0("https://gaftp.epa.gov/EJSCREEN/",
                              yrschecked, "/", sep = ""), httr::GET,
                       config = httr::config(connecttimeout = 20))
      temp2 <- sapply(temp1, "[[", 2)
      exists.fun <- function(x){
        ifelse(x>200, FALSE, TRUE)
      }
      return(yrschecked[max(which(sapply(temp2, exists.fun)))])
    }
    year <- latestavailableyear(ftpurlbase)
  }

  if (year == 2023){
    ejvars <- c('ID', 'STATE_NAME', 'ST_ABBREV', 'ACSTOTPOP',
                'PM25', 'OZONE', 'DSLPM', 'CANCER', 'RESP', 'RSEI_AIR', 'PTRAF',
                'PNPL', 'PRMP', 'PRE1960PCT', 'PTSDF', 'PWDIS', 'UST',
                'PEOPCOLORPCT', 'LOWINCPCT', 'LINGISOPCT', 'UNEMPPCT',
                'UNDER5PCT', 'LESSHSPCT', 'OVER64PCT', 'LIFEEXPPCT'
                )
    acs.year <- 2021
  } else if (year <= 2022 & year >= 2021) {
    ejvars <- c('ID', 'STATE_NAME', 'ST_ABBREV', 'ACSTOTPOP',
                'PM25', 'OZONE', 'DSLPM', 'CANCER', 'RESP', 'PTRAF',
                'PNPL', 'PRMP', 'PRE1960PCT', 'PTSDF', 'PWDIS', 'UST',
                'MINORPCT', 'LOWINCPCT', 'LINGISOPCT', 'UNEMPPCT',
                'UNDER5PCT', 'LESSHSPCT', 'OVER64PCT'
    )
    if (year == 2022) {
      acs.year <- 2020
    } else {
      acs.year <- 2019
    }
  } else if (year == 2020) {
    ejvars <- c('ID', 'STATE_NAME', 'ST_ABBREV', 'ACSTOTPOP',
                'PM25', 'OZONE', 'DSLPM', 'CANCER', 'RESP', 'PTRAF',
                'PNPL', 'PRMP', 'PRE1960PCT', 'PTSDF', 'PWDIS',
                'MINORPCT', 'LOWINCPCT', 'LINGISOPCT',
                'UNDER5PCT', 'LESSHSPCT', 'OVER64PCT'
    )
    acs.year <- 2018
  } else if (year < 2020){
    stop('EJSCREENbatch currently supports only use of data from 2020 onwards.')
  }

  return(list(ejvars,acs.year,year))
}
