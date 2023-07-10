library(data.table)
library(tidyverse)

my_wd <- 'C:/Users/atheisin/Environmental Protection Agency (EPA)/Economics and Policy Analysis Branch (EPAB) - Documents/EJ_analyses/Chemicals/NMP EJ/'
setwd(my_wd)

chem_path <- 'NMP CSV Files/' #this is where the chemical specific input datasets are located

functions_path <- 'C:/Users/atheisin/Environmental Protection Agency (EPA)/Economics and Policy Analysis Branch (EPAB) - Documents/EJ_analyses/EPAB-ejanalysis/ej_functions/'

data_path <- 'C:/Users/atheisin/Environmental Protection Agency (EPA)/Economics and Policy Analysis Branch (EPAB) - Documents/EJ_analyses/ej_data/'

buffers <- c(1) 

########################################################
#### MAYBE DON'T CHANGE THIS SECTION :) ####
###############################################
## chemical datafiles
chemical_list <- paste0(my_wd,chem_path,
                        list.files(paste0(my_wd,chem_path), 
                                   pattern = '.csv')) #pull out names of files that start with "Final_list_"

# Read in the naics6-indnaics crosswalk
naics6.cross <- fread(paste0(data_path, 'naics_crosswalks/',
                             'naics22_indnaics18_cross.csv')
); setkey(naics6.cross, naics)

# Read in .csv files as a list, homogenize column names
facil <- lapply(lapply(chemical_list, data.table::fread), 
                function(x) {colnames(x) <- tolower(colnames(x));x})

# Merge .csv files, convert to a spatial object
# Note: Create unique facil.id (names aren't unique here)
facil <- data.table::rbindlist(facil, fill = T
)[, facil.id := .I
]; setkey(facil, naics)

facil <- naics6.cross[facil] %>% 
  dplyr::mutate(long = as.numeric(longitude),
                lat = as.numeric(latitude)) %>%
  sf::st_as_sf(coords = c('long', 'lat'), crs = 4326)

#### FLAG TO CATCH ISSUES WITH NMP DATA ####
facil <- facil %>% dplyr::filter(naics < 1000000) %>%
  dplyr::slice(1:25)
