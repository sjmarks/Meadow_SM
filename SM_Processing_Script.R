#################################################################################################################################

## Install required packages- uncomment the below lines if you have never downloaded these packages to your computer

# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("plyr")

#################################################################################################################################

## Load libraries (required packages), this only needs to be ran once after opening the script in R Studio

library(plyr)
library(tidyverse)
library(lubridate)

#################################################################################################################################

################### Functions

########## Helper function to `process_SM_data` - performs coalescing join on added data

coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  
  # names of desired output
  cols <- dplyr::union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

########## Helper fun

readSoilvueVWC_CR1000 <- function(data) {
  
  data <- dplyr::select(data, 1:2, 149:184) %>%
    # Drop record no., electrical conductivity, permittivity, temperature
    dplyr::select(TIMESTAMP, dplyr::starts_with("VWC")) %>% 
    # Date info
    dplyr::mutate(TIMESTAMP = lubridate::ymd_hms(TIMESTAMP)) %>% 
    dplyr::rename(Date = TIMESTAMP) %>% 
    # add site name variable (ID)
    dplyr::mutate(ID = "RCSM2")
  
  return(data)
}

########## Helper fun

readSoilvueVWC_CR300 <- function(path) {
  
  data <- read_soilsapflow(path = path) %>% 
    # Drop record no., electrical conductivity, permittivity, temperature
    dplyr::select(TIMESTAMP, dplyr::starts_with("VWC")) %>%
    # Date info
    dplyr::mutate(TIMESTAMP = lubridate::ymd_hms(TIMESTAMP)) %>% 
    dplyr::rename(Date = TIMESTAMP) %>% 
    # add site name variable (ID)
    dplyr::mutate(ID = "RCSM3")
  
  names <- colnames(data) %>% 
    stringr::str_remove_all(pattern = "_Avg")
  
  colnames(data) <- names
  
  return(data)
}

########## Helper fun

clean_SM_data <- function(path, site){
  
  # Deduce site ID- can be site name or logger S/N
  ID <- read.csv(path, nrows = 1, header = FALSE) %>% 
    dplyr::pull(var = V1) %>% 
    as.character() %>% 
    # extracts everything after a colon
    stringr::str_extract(pattern = "[^:]*$") %>%
    stringr::str_trim()
  
  if(site == "rc"){
    
    ID <- dplyr::case_when(
      ID == "20541053" ~ "RC1SM",
      ID == "20541050" ~ "RCSM5",
      TRUE ~ ID
    )
    
  }
  
  ID <- stringr::str_to_upper(ID)
  
  ### RC, Control, Marian Procedure
  if(site %in% c("rc", "control", "marian")){
    
    ## Data cleaning procedure for HOBO SM files (.csv)
    if(stringr::str_detect(stringr::str_to_lower(path), pattern = ".csv")){
      
      # Read in data from .csv excluding top row
      data <- read_csv(path, skip = 1, 
                       col_types = cols(`#` = col_skip()))
      
      # Name first column Date
      names(data)[1] <- "Date"
      
      # Parse Date as POSIXct object, rounds down to half hour
      data <- data %>% 
        dplyr::mutate(Date = lubridate::mdy_hms(Date)) %>% 
        dplyr::mutate(Date = lubridate::floor_date(Date, unit = "minute"))
      
      # Extract columns for sensor depths, perform regex search to deduce the sensor depth
      # Lower case all column names and strip spaces to ease regex matching
      data_colnames <- colnames(data) %>% 
        stringr::str_to_lower() %>% 
        stringr::str_replace_all(pattern = " ", replacement = "")
      
      colnames(data) <- data_colnames
      
      # 10 cm, 30 cm, and 1 meter configuration
      if(TRUE %in% stringr::str_detect(data_colnames, pattern = "1m|100cm|1meter")){
        
        ten_cm <- select(data, date, matches("10cm|0.1m"))
        colnames(ten_cm) <- c("Date", "VWC_10cm")
        
        thirty_cm <- select(data, date, matches("30cm|0.3m"))
        colnames(thirty_cm) <- c("Date", "VWC_30cm")
        
        one_meter <- select(data, date, matches("1m|100cm|1meter"))
        colnames(one_meter) <- c("Date", "VWC_1m")
        
        # Create clean/standardized data with sensor depths in proper order
        ordered_data <- plyr::join_all(list(ten_cm, thirty_cm , one_meter), by = 'Date', 
                                       type = 'inner') %>%
          dplyr::mutate(ID = ID)
        
      }
      
      # 10 cm, 30 cm, and 75 cm configuration
      if(TRUE %in% stringr::str_detect(data_colnames, pattern = "0.75m|75cm")){
        
        ten_cm <- select(data, date, matches("10cm|0.1m"))
        colnames(ten_cm) <- c("Date", "VWC_10cm")
        
        thirty_cm <- select(data, date, matches("30cm|0.3m"))
        colnames(thirty_cm) <- c("Date", "VWC_30cm")
        
        seventyfive_cm <- select(data, date, matches("0.75m|75cm"))
        colnames(seventyfive_cm) <- c("Date", "VWC_75cm")
        
        # Create clean/standardized data with sensor depths in proper order (i.e.columns)
        ordered_data <- plyr::join_all(list(ten_cm, thirty_cm , seventyfive_cm), by = 'Date', 
                                       type = 'inner') %>%
          dplyr::mutate(ID = ID)
        
      }
      
      return(ordered_data)
      
    }
    
    ## Data cleaning procedure for .dat files (Campbell SoilVUE) 
    if(stringr::str_detect(stringr::str_to_lower(path), pattern = ".dat")){
      
      if(stringr::str_detect(stringr::str_to_lower(path), "sap")){
        
        ordered_data <- read_soilsapflow(path = path) %>% 
          readSoilvueVWC_CR1000()
        
      } else {
        
        ordered_data <- readSoilvueVWC_CR300(path)
        
      }
      
      return(ordered_data)
      
    }
    
  }
  
  if(site == "childs"){
    
    # Read in data from .csv excluding top row
    data <- read_csv(path, skip = 1, 
                     col_types = cols(`#` = col_skip()))
    
    # Name first column Date
    names(data)[1] <- "Date"
    
    # Parse Date as POSIXct object, rounds down to half hour
    data <- data %>% 
      dplyr::mutate(Date = lubridate::mdy_hms(Date)) %>% 
      dplyr::mutate(Date = lubridate::floor_date(Date, unit = "minute"))
    
    # Extract columns for sensor depths, perform regex search to deduce the sensor depth
    # Lower case all column names and strip spaces to ease regex matching
    data_colnames <- colnames(data) %>% 
      stringr::str_to_lower() %>% 
      stringr::str_replace_all(pattern = " ", replacement = "")
    
    colnames(data) <- data_colnames
    
    # 30 cm, 60 cm configuration
    thirty_cm <- select(data, date, matches("30cm|0.6m"))
    colnames(thirty_cm) <- c("Date", "VWC_30cm")
    
    sixty_cm <- select(data, date, matches("60cm|0.6m"))
    colnames(sixty_cm) <- c("Date", "VWC_60cm")
    
    # Create clean/standardized data with sensor depths in proper order
    ordered_data <- plyr::join_all(list(thirty_cm, sixty_cm), by = 'Date', 
                                   type = 'inner') %>%
      dplyr::mutate(ID = ID)
    
    return(ordered_data)
    
  }
  
}

########## Main function

process_SM_data <- function(path, site, prev_compile = NULL, written_file_name = NULL){
  
  ## Control sequence checks before proceeding with function eval
  
  site <- stringr::str_to_lower(site)
  
  # Generic name for the outputted file based on site if a file name is not provided
  if(is.null(written_file_name)){
    written_file_name <- paste0("SMCompilation_", site, ".csv")
  }
  
  if(!stringr::str_detect(written_file_name, pattern = ".csv")){
    stop("provided written file name must end with .csv")
  }
  
  if(!site %in% c("marian", "rc", "control", "childs")){
    stop("site must be inputted as one of Marian, RC, Control, or Childs in quotes (not case sensitive).")
  }
  
  ## Create char. vector of file names w/ directory path prepended contained in provided path
  
  files <- list.files(path, full.names = TRUE)
  
  ## Creates list of clean/standardized data (data frames) for all those provided in the path using `clean_SM_data` function
  
  listed_SM <- purrr::map(files, clean_SM_data, site = site)
  
  ## Compiles soil moisture data using only those files provided in the path
  
  # Keeps files grouped by instrument and orders data chronologically. Duplicate observations are removed.
  compiled <- dplyr::bind_rows(listed_SM) %>% 
    dplyr::distinct() %>%
    dplyr::group_by(ID) %>% 
    # `dplyr::arrange` would not function properly here
    do(data.frame(with(data = .,.[order(Date),]))) %>% 
    ungroup()
  
  # Extract colnames for the compiled data
  compiled_colnames <- colnames(compiled) %>% 
    stringr::str_remove(pattern = "Date|ID")
  
  compiled_colnames <- compiled_colnames[compiled_colnames != ""]
  
  # Wide pivot the data with `names_from` ID, `values_from` compiled_colnames (excluding date and ID)
  compiled_pivoted <- compiled %>% 
    tidyr::pivot_wider(names_from = ID, values_from = all_of(compiled_colnames), values_fn = list) %>% 
    tidyr::unnest(starts_with("VWC"), keep_empty = FALSE)
  
  if(is.null(prev_compile)){
    
    compiled_pivoted <- compiled_pivoted %>% 
      do(data.frame(with(data = .,.[order(Date),]))) %>%
      dplyr::distinct() %>%
      purrr::discard(~all(is.na(.))) %>% 
      dplyr::mutate(Date = as.character(Date))
    
    # Write out compiled file from provided files only 
    readr::write_csv(compiled_pivoted, path = paste0(path, "/", written_file_name))
    
    ## When prev_compile file is provided, the files provided in the path are appended to the previously compiled file
    
    # Performs coalescing join- user does not have to worry about feeding in data that has been previously compiled
  } else {
    
    appended_compile <- prev_compile %>%
      coalesce_join(compiled_pivoted, by = "Date") %>% 
      do(data.frame(with(data = .,.[order(Date),]))) %>%
      dplyr::distinct() %>% 
      purrr::discard(~all(is.na(.))) %>% 
      dplyr::mutate(Date = as.character(Date))
    
    # Write out appended compile file
    readr::write_csv(appended_compile, path = paste0(path, "/", written_file_name))
    
  }
  
}

###################

#################################################################################################################################

