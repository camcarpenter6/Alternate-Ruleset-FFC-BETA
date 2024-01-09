###These scripts download data from either CDEC or USGS 
library(glue) # to paste things together (similar to python f())
library(dplyr) # data wrangling
library(lubridate) # datetime wrangling
library(readr) # read/write csv
library(fs) # platform independent handling of file systems
library(janitor) # data cleaning


Gage_information <- read.csv(here("Preprocess","Site Data.csv"), header = T)

#This function downloads from the USGS gages and manipulates the data to match user input
USGS_gage_flow <- function(gage_id) {
  
  pCode <- "00060"
  #First get the data from, We want all the flow data available so we just read in the users gage data
  flow <- readNWISdv(
    gage_id,
    "00060",
    startDate = "",
    endDate = ""
  )
  
  #Apply proper names from the USGS site
  flow <- renameNWISColumns(flow)
  
  #Take just the data and flow data
  flow <- flow[,c("Date","Flow")]

  #Rename the data so it matches the user input files
  flow <- flow %>%
    rename("date" = "Date", "flow" = "Flow")
  
  #Return the flow data
  return(flow)
}


#Define the function to get the cdec flow data
get_cdec <- function(
    station,
    sensor,
    duration ="D"
) {
  # Make today the end date
  end <- Sys.Date()
  start <- "1900-01-01"
  
  # Set up the URL for CDEC data
  linkCDEC <- paste(
    "http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", station,
    "&SensorNums=", sensor,
    "&dur_code=", duration,
    "&Start=", start,
    "&End=", end,
    sep = ""
  )
  
  # Read in and format the data
  df <- read_csv(linkCDEC) %>%
    clean_names() %>%
    dplyr::select(-c(obs_date, data_flag)) %>%
    dplyr::rename(datetime = date_time)
  
  # Coerce to numeric for the value column, create NAs for missing values, sometimes listed as "---"
  df$value <- suppressWarnings(as.numeric(df$value))
  
  cdec <- paste0("cdec_", duration, "_", station) # Make a station name
  
  df$datetime <- as.Date(as.character(df$datetime))
  
  df <- as.data.frame(df)
  
  if (dim(df)[1] > 0) {
    cat(paste0("Downloaded Station ", cdec, " data successfully, in current workspace!\n\n"))
    return(df)
  } else {
    cat(paste0("No data available for Station ", cdec, " for this date range or interval!\n\n"))
  }
}



# Gets the site name and  a USGS or CDEC ID and assign title and class
get_gage_data <- function(gage_id) {
  result <- Gage_information %>%
    dplyr::filter(siteid == gage_id)
  
  if (nrow(result) == 0) {
    cat(paste0("Gage ID not found. Please enter a site name for gage ", gage_id,": "))
    site_name <- readline()
    cat(paste0("Please enter a stream class (Defult 3) for gage ", gage_id,": "))
    class <- as.numeric(readline())
    cat(paste0("Please enter the stream comid for gage ", gage_id,": "))
    comid <- readline()
  } else if(is.nan(result$Class) | is.na(result$Class)) {
    site_name <- result$sitename
    cat(paste0("Stream class not found for gage ",gage_id,". Please enter a stream class (Defult 3): "))
    class <- as.numeric(readline())
    site_name <- result$sitename
    comid <- result$COMID
  }
  else{
    site_name <- result$sitename
    class <- result$Class
    comid <- result$COMID
  }
  
  return(list(site_name = site_name, class = class, comid = comid))
}
