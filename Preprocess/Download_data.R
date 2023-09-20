###These scripts download data from either CDEC or USGS 
library(glue) # to paste things together (similar to python f())
library(dplyr) # data wrangling
library(lubridate) # datetime wrangling
library(readr) # read/write csv
library(fs) # platform independent handling of file systems
library(janitor) # data cleaning
library(wateRshedTools)  # for get_cdec function

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


#Define the function tp get the cdec flow data
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
    select(-c(obs_date, data_flag)) %>%
    rename(datetime = date_time)
  
  # Coerce to numeric for the value column, create NAs for missing values, sometimes listed as "---"
  df$value <- suppressWarnings(as.numeric(df$value))
  
  cdec <- paste0("cdec_", duration, "_", station) # Make a station name
  
  if (dim(df)[1] > 0) {
    cat(paste0("Downloaded Station ", cdec, " data successfully, in current workspace!\n\n"))
    return(df)
  } else {
    cat(paste0("No data available for Station ", cdec, " for this date range or interval!\n\n"))
  }
}
