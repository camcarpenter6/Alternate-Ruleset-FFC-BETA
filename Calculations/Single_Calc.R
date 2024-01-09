#This scrip runs a single gage/data

##source the required code downloading the data, completing the calculation and post-processing

source(here("Preprocess","Download_data.R"))

source(here("Preprocess","Preprocess_data.R"))

source(here("Calculations","Get_metrics.R"))

source(here("Post_Processing_Files","Metrics_percentiles.R"))

source(here("Post_Processing_Files","HTML_plotting.R"))

source(here("Post_Processing_Files","Box_plot_creation.R"))



gage_check <- readline("Are you using your time series data or a USGS gage site (TS for time series or G for USGS gage data): ")

#Check to see if which term was entered and get the important information
if (gage_check == "TS"){
  #Get the file location of time series
  cat("Please enter the full file location of the csv file with the flow data.\n The file needs to have a column called 'date' with a MM/DD/YYYY, and a column called 'flow' with the flow data.")
  input_time_series_loc <- readline("File Path Here: ")
  
  flow <- read.csv(input_time_series_loc, header = T) %>% #Enter first flow data
    mutate(date = as.Date(date, "%m/%d/%Y"))
  
  #Remove rows that contains all NA's
  flow <- flow[rowSums(is.na(flow)) != ncol(flow), ]
  
  site_name <- readline("Please enter the name that you want to use to describe this flow data: ")
  
  gage_id <- site_name
  
  cat("Please enter the stream class: ")
  
  class <- as.numeric(readline())
  
  #Get comid for the gauge site
  cat("Please enter the comid of the site that is going to be evaluated: ")
  comid <- readline()
}else if(gage_check == "G"){
  gage_id <- readline("Please enter USGS site number or a 3-letter CDEC station ID, the site must be in the California: ")
  
  if(nchar(gage_id) == 3){
    cat("Please enter the sensor number you want to get the data from.  \nCommon sensors are 8 for full natural flow data and 41 for daily mean flow: ")
    sensor_ID <- readline("Sensor ID: ")
    
    cat("\n Data for the calculator must be daily")
    
    raw_flow <- get_cdec(gage_id,sensor_ID)
    
    #Take just the date and flow data
    raw_flow <-  raw_flow[,c("datetime","value")]
    
    #Rename the data so it matches the user input files
    flow <- raw_flow %>%
      rename("date" = "datetime", "flow" = "value")
    
    gage_info <- get_gage_data(gage_id = gage_id)
    
    site_name <- gage_info$site_name
    comid <- gage_info$comid
    class <- gage_info$class
  }else {
    flow <- USGS_gage_flow(gage_id)
    
    gage_info <- get_gage_data(gage_id = gage_id)
    
    site_name <- gage_info$site_name
    comid <- gage_info$comid
    class <- gage_info$class
  }
}

  
#Since the data frame will take the names of the list we need to rename the columns to match the original Calculator
Results_df  <- flow_metrics_calculations(flow)
  
#Make a new file path for this output directory
new_dir <- paste("Outputs",gage_id,sep = "/")
  
#Now the new directory 
dir.create(new_dir)
  
# Clean up the gage name to remove leading/trailing spaces
  gage_name_cleaned <- trimws(gage_id)
  
  # Construct the file path
  file_path <- file.path(here(), "Outputs", gage_name_cleaned, paste0(gage_name_cleaned, "_Metrics.csv"))
  
  # Write the CSV file
  write_csv(Results_df, file = file_path)
  
  flow_file_path <- file.path(here(), "Outputs", gage_name_cleaned, paste0(gage_name_cleaned, "_flow.csv"))
  
  # Write the CSV file
  write_csv(flow, file = flow_file_path)
  
  #Get the metrics percentiles following the same method as the original calculator
  metrics_percentiles <- get_percentiles(Results_df,comid)
  
  # Construct the file path
  file_path_2 <- file.path(here(), "Outputs", gage_name_cleaned, paste0(gage_name_cleaned, "_Metric_Percentiles.csv"))
  
  # Write the CSV file
  write_csv(metrics_percentiles, file = file_path_2)

  
  #Make the html figure for the
  single_series_HTML(flow,Results_df,gage_name_cleaned,site_name)
  
  #Make a new directory for the the boxplots
  new_dir_box_plots <- paste("Outputs",gage_name_cleaned,"boxplots",sep = "/")
  
  #Now the new directory 
  dir.create(new_dir_box_plots)
  
  #Now call the box plot function
  single_box_plots(Results_df,site_name,new_dir_box_plots)
  
  
  
