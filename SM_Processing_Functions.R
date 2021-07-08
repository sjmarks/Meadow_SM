library(plyr)
library(tidyverse)
library(lubridate)

#### Helper- `coalesce_join`

# **Description**
#   
#   `coalesce_join` performs a majority of the data aggregation work within the `process_SM_data`. The function combines two datasets containing identical non-key variables in varying states of completeness. This allows for the soil moisture data compilation to be updated/appended to without worry of overlap with the existing time series in the existing compilation.
# 
# **Arguments:**
#   
#   * `x`, `y` tbls to join
# * `by` a character vector of variables to join by. If NULL, the default, *_join() defined by the `join` argument will do a natural join, using all variables with common names across the two tables. A message lists the variables so that you can check they're right (to suppress the message, simply explicitly list the variables that you want to join).
# * `suffix` If there are non-joined duplicate variables in x and y, these suffixes will be added to the output to disambiguate them. Should be a character vector of length 2
# * `join` type of mutating join supported by `dplyr`
# * ... other parameters passed onto methods, for instance, `na_matches` to control how NA values are matched. This is mostly included for robustness.

# **Value**
#   
#   A data frame `tbl_df`

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


###########################################################################################

#### Helper- `read_campbell_dat`

# **Description**
#   
#   `read_campbell_dat` reads in the `.dat` files produced by the Campbell Scientific CR1000 and CR300 logger programs currently collecting SoilVUE10 soil moisture data in Rock Creek Meadow. The function adds an additional column/variable to the data file called `ID`. **Pleasure ensure the first line of the `.dat` file contains the site ID for the logger/soil moisture instrument (i.e. RCSM2).** You can edit the `.dat` file in Notepad or something else. 
# 
# **Arguments:**
#   
#   * `path` path to the `.dat` file. Must be quoted and end with `.dat`.
# 
# **Value**
#   
#   A data frame `tbl_df`


read_campbell_dat <- function(path) {
  
  # get column names from .dat file
  column_names <- readr::read_lines(path, skip = 1, n_max = 1) %>% 
    stringr::str_remove_all(pattern = '\"') %>% 
    stringr::str_split(pattern = ",") %>% 
    purrr::pluck(1)
  
  # gather metadata from first line of .dat file, collapse into single string, extract site name (ID)
  meta_data <- readr::read_lines(path, n_max = 1) %>% 
    stringr::str_remove_all(pattern = '\"') %>% 
    stringr::str_split(pattern = ",") %>% 
    purrr::pluck(1) %>% 
    paste(collapse = " ") %>% 
    # because these instruments are sites RCSM2, RCSM2b, or RCSM3 
    str_extract("RCSM2b_1|RCSM2b_2|RCSM2|RCSM3")
  
  data <- readr::read_csv(path, col_names = column_names, skip = 4, na = "NAN", 
                          col_types = cols(.default = col_double(), 
                                           TIMESTAMP = col_datetime(format = "")))
  data <- data %>%
    dplyr::mutate(ID = meta_data)
  
  return(data)
  
}

###########################################################################################

#### Helper `readSoilvueVWC_CR1000`

# **Description**
#   
#   `readSoilvue_CR1000` manipulates the data frame `tbl_df` produced by `read_campbell_dat` to include only timestamp and VWC variables, navigating electrical conductivity, permitivity, temperature, and sap flow data currently produced by the CR1000 soil and sap flow program.
# 
# **Arguments:**
#   
#   * `data` A data frame `tbl_df` produced by `read_campbell_dat`
# 
# **Value**
#   
#   A data frame `tbl_df`

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

###########################################################################################

#### Helper `readSoilvueVWC_CR300`

# **Description**
#   
#   `readSoilvue_CR300` reads in the .dat file currently produced by the CR300 soilVUE program to produce a data frame `tbl_df` including only timestamp and VWC variables, navigating electrical conductivity, permitivity, and temperature data.
# 
# **Arguments:**
#   
#   * `path` path to the .dat soilVUE file produced by the CR300. Must be quoted and end with .dat
# 
# **Value**
#   
#   A data frame `tbl_df`

readSoilvueVWC_CR300 <- function(path) {
  
  data <- read_campbell_dat(path = path) %>% 
    # Drop record no., electrical conductivity, permittivity, temperature
    dplyr::select(TIMESTAMP, dplyr::starts_with("VWC"), ID) %>%
    # Date info
    dplyr::mutate(TIMESTAMP = lubridate::ymd_hms(TIMESTAMP)) %>% 
    dplyr::rename(Date = TIMESTAMP)
  
  names <- colnames(data) %>% 
    # if logger program was recording an avg, instead of sample
    stringr::str_remove_all(pattern = "_Avg")
  
  colnames(data) <- names
  
  return(data)
  
}

###########################################################################################

#### Helper `clean_SM_data`

# **Description**
#   
#   `clean_SM_data` reads in .csv data file produced by HOBO Onset logger for soil moisture (VWC) or .dat file produced by CR300 or CR1000 for SoilVUE10 (VWC). Produces a data frame `tbl_df` in long format with variables corresponding to date/time, VWC at site respective sensor depths, and site ID.
# 
# **Arguments:**
#   
#   * `path` path to the .csv or .dat file, must end in .csv or .dat
# * `site` meadow site where the raw file was collected- one of "Marian", "RC", "Control", or "Childs" in quotes
# 
# **Value**
#   
#   A data frame `tbl_df`

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
      
      # retieve number of cols from data
      data_cols <- ncol(data)
      
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
      if(TRUE %in% stringr::str_detect(data_colnames, pattern = "1m|100cm|1meter") & data_cols == 4){
        
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
      if(TRUE %in% stringr::str_detect(data_colnames, pattern = "0.75m|75cm") & data_cols == 4){
        
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
      
      # 30 cm only configuration
      if(TRUE %in% stringr::str_detect(data_colnames, pattern = "0.3m|30cm") & data_cols == 2){
        
        thirty_cm <- select(data, date, matches("30cm|0.3m"))
        colnames(thirty_cm) <- c("Date", "VWC_30cm")
        
        # Create clean/standardized data with sensor depths in proper order (i.e.columns)
        ordered_data <- thirty_cm %>%
          dplyr::mutate(ID = ID)
        
      }
      
      # 30 cm and 70 cm configuration (RCSM3_temp following the May 2021 trip)
      if(TRUE %in% stringr::str_detect(data_colnames, pattern = "0.7m|70cm") & data_cols == 3){
        
        thirty_cm <- select(data, date, matches("30cm|0.3m"))
        colnames(thirty_cm) <- c("Date", "VWC_30cm")
        
        seventy_cm <- select(data, date, matches("70cm|0.7m"))
        colnames(seventy_cm) <- c("Date", "VWC_70cm")
        
        # Create clean/standardized data with sensor depths in proper order (i.e.columns)
        ordered_data <- plyr::join_all(list(thirty_cm , seventy_cm), by = 'Date', 
                                       type = 'inner') %>%
          dplyr::mutate(ID = ID)
        
      }
      
      # 30 cm and 60 cm configuration (new sap flow site following the May 2021 trip)
      if(TRUE %in% stringr::str_detect(data_colnames, pattern = "0.6m|60cm") & data_cols == 3){
        
        thirty_cm <- select(data, date, matches("30cm|0.3m"))
        colnames(thirty_cm) <- c("Date", "VWC_30cm")
        
        sixty_cm <- select(data, date, matches("60cm|0.6m"))
        colnames(sixty_cm) <- c("Date", "VWC_60cm")
        
        # Create clean/standardized data with sensor depths in proper order (i.e.columns)
        ordered_data <- plyr::join_all(list(thirty_cm , sixty_cm), by = 'Date', 
                                       type = 'inner') %>%
          dplyr::mutate(ID = ID)
        
      }
      
      
      return(ordered_data)
      
    }
    
    ## Data cleaning procedure for .dat files (Campbell SoilVUE) 
    if(stringr::str_detect(stringr::str_to_lower(path), pattern = ".dat")){
      
      if(stringr::str_detect(stringr::str_to_lower(path), "sap")){
        
        ordered_data <- read_campbell_dat(path = path) %>% 
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

###########################################################################################

#### Main Function `process_SM_data`

# **Description**
#   
#   `process_SM_data` takes a folder containing raw .csv and/or .dat soil moisture data files from one meadow site and performs data compilation, appending to an existing compilation for that meadow site if prompted. Outputs a written .csv file.
# 
# **Arguments**
#   
#   * `path` path to folder containing raw .csv and/or .dat soil moisture data files for a meadow site. This provided path is where the compilation .csv will be written to.
# * `site` meadow site where the raw files were collected- one of "Marian", "RC", "Control", or "Childs" in quotes
# * `prev_compile` previous data compilation. By default this is set to `NULL` 
# * `written_file` name of produced compilation file, must be quoted and end with .csv
# 
# **Value**
#   
#   A .csv file written to `path`
# 
# **Other notes**
#   
#   Make sure that the first line of the .csv file contains a site ID that is consistent with the column naming convention in the file that is being appended to. From my experience, editing .csv in EXCEL and then saving them can be problematic. Suggestion is to edit the .csv in NotePad.
# 

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

## Temporal Aggregation Function

# **Description**
#   
#   `temp_agg_meadow_dat` takes a data.frame of a meadow data compilation (e.g. soil moisture, well, climate, sap flow data) and performs a temporal aggregation. 
# 
# **Arguments**
#   
#   * `data` dataframe with data to aggregate, **must** contain a variable called "Date" of class `POSIXct` 
# * `start_day_of_week` an integer specifying the start day of the week for the temporal aggregation. 1 corresponds to Sunday and so on.
# * `interval` the function tries to determine the interval of the original time series (e.g. hourly) by calculating the most common interval between time steps. The interval is needed for calculations where the `data.thresh > 0` as is defaulted. For example, a time step of 30 minutes would be specified as `interval = "30 min"`
# * `avg.time` This defines the time period to average to. Can be “sec”, “min”, “hour”, “day”, “DSTday”, “week”, “month”, “quarter” or “year”. For much increased flexibility a number can precede these options followed by a space. For example, a 7 day aggregation would be `avg.time = "7 day"`. In addition, avg.time can equal “season”, in which case 3-month seasonal values are calculated with spring defined as March, April, May and so on.
# * `data.thresh` The data capture threshold to use (%). A value of zero means that all available data will be used in a particular period regardless if of the number of values available. Conversely, a value of 100 will mean that all data will need to be present for the average to be calculated, else it is recorded as NA. 
# * `statistic` The statistic to apply when aggregating the data; default is the mean. Can be one of “mean”, “max”, “min”, “median”, “sd”. "sd" is standard deviation.
# 
# **Value**
#   
#   A data.frame with Date in class `POSIXct` and WY in class `numeric`. WY indicates the water year that the aggregation belongs to.
# 
# **Other notes**
#   
#   Make sure there is a variable in the supplied data.frame called "Date"

temp_agg_meadow_dat <- function(data, start_day_of_week, interval = "30 min", avg.time = "7 day", data.thresh = 50, statistic = "mean"){
  
  # data <- readr::read_csv(path, col_types = cols(Date = col_character(), .default = col_double())) %>% 
  #   dplyr::mutate(Date = as.POSIXct(Date, tz = "UTC"))
  
  # path_write_out <- paste0(stringr::str_replace(path, "[^/]+$", replacement = ""), file_name_write)
  
  # Determine the dates corresponding to the first and last of chosen weekday (start date for averaging) present in data
  start_date <- data %>% 
    dplyr::mutate(is.chosen_wday = ifelse(lubridate::wday(Date) == start_day_of_week, T, F)) %>%
    dplyr::filter(is.chosen_wday == TRUE) %>% 
    dplyr::summarise(min(Date)) %>% 
    dplyr::pull()
  
  end_date <- data %>% 
    dplyr::mutate(is.chosen_wday = ifelse(lubridate::wday(Date) == start_day_of_week - 1, T, F)) %>%
    dplyr::filter(is.chosen_wday == TRUE) %>% 
    dplyr::summarise(max(Date)) %>% 
    dplyr::pull()
  
  data <- data %>% 
    # rename Date variable to "date" to play nicely w/ openair::timeAverage
    dplyr::rename(date = Date) %>% 
    dplyr::filter(date >= start_date & date <= end_date)
  
  aggregation <- openair::timeAverage(data, avg.time = avg.time, 
                                      data.thresh = data.thresh, statistic = statistic,
                                      start.date = start_date, end.date = end_date, interval = interval) %>% 
    # determine water year membership of the time avg- this might need to be tweaked
    dplyr::mutate(WY = dplyr::case_when(
      date %within% lubridate::interval(ymd("2017-10-01", tz = "UTC"), 
                                        ymd("2018-09-30", tz = "UTC")) ~ 2018,
      date %within% lubridate::interval(ymd("2018-10-01", tz = "UTC"), 
                                        ymd("2019-09-30", tz = "UTC")) ~ 2019,
      date %within% lubridate::interval(ymd("2019-10-01", tz = "UTC"), 
                                        ymd("2020-09-30", tz = "UTC")) ~ 2020,
      date %within% lubridate::interval(ymd("2020-10-01", tz = "UTC"), 
                                        ymd("2021-09-30", tz = "UTC")) ~ 2021,
    )) 
  # %>% 
  #   mutate(date = as.character(date)) %>% 
  #   rename(Date = date) %>%  
  #   # Write to .csv
  #   readr::write_csv(path = path_write_out)
  
}
