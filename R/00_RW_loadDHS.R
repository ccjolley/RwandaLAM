#' Import Rwanda DHS 2014/2015 data
#'
#' Imports data from the 2014/2015 Demographic and Health Surveys from Rwanda
#'
#' DHS modules are as follows:
#' (from http://dhsprogram.com/data/Dataset-Types.cfm)
#' * HR: household data (household level description)
#' * PR: household list (inidividual roster)
#' * IR: women's module (one woman/hh)
#' * MR: men's module (one man/hh)
#' * CR: couple's module
#' * KR: children's module (ea. child < 5 y old)
#' * BR: birth data (birth history of all children) --> fertility / mortality rates
#' * WI: wealth index data (for data before 1990 to calcualte wealth index)
#'
#' @author Laura Hughes, lhughes@usaid.gov
#'
#' @import dplyr
#' @import haven
#' @import llamar
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#'

# Import/Load packages ----------------------------------------------------
pkgs = c('dplyr', 'haven', 'tidyr', 'ggplot2', 'readxl', 'foreign', 'data.table', 'stringr')

# Check if packages are installed
alreadyInstalled = installed.packages()[, "Package"]

toInstall = pkgs[!pkgs %in% alreadyInstalled]

# Install anything that isn't already installed.
if (length(toInstall > 0)) {
  print(paste0("Installing these packages: ", toInstall))
  
  install.packages(toInstall)
}

# Load packages
for (i in seq_along(pkgs)) {
  library(pkgs[i], character.only = TRUE, quietly = quiet)
}


# Main function to load data ----------------------------------------------

setRwWD = function(user = 'Nada',
                   path = NA) {
  
  # Set up the working directory where the DHS data are locally stored.
  
  # Manually override the options
  if (!user %in% c('Laura', 'Tim', 'Nada', 'Patrick','Craig')){ # User name isn't recognized
    if(is.na(path)) {
      stop("Path containing data isn't specified.")
      
    } else {
      path = path # Manually set path
    }
    
    # Laura
  } else if(user == 'Laura') {
    path = '~/GitHub/RwandaLAM/'
    
    # Tim
  } else if(user == 'Tim') {
    path = '~/GitHub/RwandaLAM/'
    
  } else if(user == 'Tim_home') {
    path = '~/Users/Tim/Rwanda/'
    
    # Nada
  } else if(user == 'Nada') {
    path = '~/Documents/GitHub/RwandaLAM/'
    
    # Patrick
  } else if(user == 'Patrick') {
    path = '~/Documents/USAID/Rwanda/'

    # Craig
  } else if(user == 'Craig') {
    path = 'C:/Users/Craig/Desktop/Live Projects/Rwanda/'
  }
  
  # Helper functions to deal with labels ------------------------------------
  source(paste0(path, 'R/attributes.R'))
  
  return(path)
}  


path = setRwWD(user = 'Nada')

# Import raw data ---------------------------------------------------------

hh = read_dta(paste0(path, 'Datain/RW_2014-15_DHS/rwhr70dt/RWHR70FL.DTA'))
# birth = read_dta(paste0(path, 'Datain/RW_2014-15_DHS/rwbr70dt/RWBR70FL.DTA')) # Duplicate info from other mods
# couples = read_dta(paste0(path, 'Datain/RW_2014-15_DHS/rwcr70dt/RWCR70FL.DTA')) # Duplicate info from womens/mens mods
women = read_dta(paste0(path, 'Datain/RW_2014-15_DHS/rwir70dt/RWIR70FL.DTA'))
kids = read_dta(paste0(path, 'Datain/RW_2014-15_DHS/rwkr70dt/RWKR70FL.DTA'))
men = read_dta(paste0(path, 'Datain/RW_2014-15_DHS/rwmr70dt/RWMR70FL.DTA'))
roster = read_dta(paste0(path, 'Datain/RW_2014-15_DHS/rwpr70dt/RWPR70FL.DTA'))
geo = read.dbf(paste0(path,'Datain/RW_2014-15_DHS/rwge71fl/RWGE71FL.dbf'))


# pull out value labels ---------------------------------------------------
hh_labels = pullAttributes(hh) %>% 
  mutate(module = 'hh', rowNum = row_number()) # Add tags for the data frame name and the row number for each variable
  
# birth_labels = pullAttributes(birth) %>% 
  # mutate(module = 'birth', rowNum = row_number())

# couples_labels = pullAttributes(couples) %>% 
  # mutate(module = 'couples', rowNum = row_number())

women_labels = pullAttributes(women) %>% 
  mutate(module = 'women', rowNum = row_number())

kids_labels = pullAttributes(kids) %>% 
  mutate(module = 'kids', rowNum = row_number())

men_labels = pullAttributes(men) %>% 
  mutate(module = 'men', rowNum = row_number())

roster_labels = pullAttributes(roster) %>% 
  mutate(module = 'roster', rowNum = row_number())

# combine all labels together in one master list, note couples_labels is currently not included
#labels = bind_rows(hh_labels, birth_labels, women_labels, kids_labels, men_labels, roster_labels)
labels = bind_rows(hh_labels, women_labels, kids_labels, men_labels, roster_labels)

# remove value labels -----------------------------------------------------
hh_all = removeAttributes(hh)
# birth_all = removeAttributes(birth)
# couples_clean = removeAttributes(couples)
women_all = removeAttributes(women)
kids_all = removeAttributes(kids)
men_all = removeAttributes(men)
roster_all = removeAttributes(roster)


