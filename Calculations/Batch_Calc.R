#This package batch calculates metrics given USGS and CDEC data

###load the required packages for the all the calculations
library(here); library(tidyverse); library(ffcAPIClient);library(dataRetrieval)

###source the required code downloading the data, completing the calculation and post-processing

source(here("Preprocess","Download_data.R"))

source(here("Preprocess","Preprocess_data.R"))

source(here("Post_Processing_Files","Metrics_percentiles.R"))

source(here("Post_Processing_Files","HTML_plotting.R"))

source(here("Post_Processing_Files","Box_plot_creation.R"))


#get all the gage
cat("Make sure a csv with all of the gages that need to be run are located in the 'Input_data' folder \n 
        all the files in the folder will be anlalyzed and they need to have matching headers. \n
        The headers must be labled in the flowing order: column 1: 'Gage_ID', column 2: sensor_ID,  column 3: Duration_code,column 4: COMID \n
        column 2 and 3 are only nessisary for data from CDEC")

readline("press any 'enter' to continue")

#Old Code - Not working
#input_gages <-
#  list.files(path = "./Input_data/", pattern = "*.csv") %>% 
#  map_df(~read_csv(.))

files <- dir(here("Input_data"), pattern = '\\.csv', full.names = TRUE)
tables <- lapply(files, read.csv)
condesed_table <- do.call(rbind, tables)

for(i in 1:nrow(condesed_table)){
  
  if(nchar(condesed_table$Gage_ID[i]) == 3){
    raw_flow <- get_cdec(condesed_table$Gage_ID[i],condesed_table$sensor_ID[i],condesed_table$Duration_code[i])
    
    #Take just the date and flow data
    raw_flow <-  raw_flow[,c("datetime","value")]

    #Rename the data so it matches the user input files
    flow <- raw_flow %>%
      rename("date" = "datetime", "flow" = "value")
    
    site_name <- paste("CDEC Gage:",condesed_table$Gage_ID[i])
  }
  else {
    flow <- USGS_gage_flow(condesed_table$Gage_ID[i])
    
    gage_data <- readNWISuv(siteNumbers = condesed_table$Gage_ID[i],
                                     parameterCd = "00060")
    
    siteInfo <- attr(gage_data, "siteInfo")
    
    site_name <- siteInfo$station_nm
  }

  #Since the data frame will take the names of the list we need to rename the columns to match the original Calculator
  Results_df  <- flow_metrics_calculatiosn(flow)
    
  #Make a new file path for this output directory
  new_dir <- paste("Outputs",condesed_table$Gage_ID[i],sep = "/")
  
  #Now the new directory 
  dir.create(new_dir)
  
  # Clean up the gage name to remove leading/trailing spaces
  gage_name_cleaned <- trimws(condesed_table$Gage_ID[i])
  
  # Construct the file path
  file_path <- file.path(here(), "Outputs", gage_name_cleaned, paste0(gage_name_cleaned, "_Metrics.csv"))
  
  # Write the CSV file
  write_csv(Results_df, file = file_path)
  
  #Get the metrics percentiles following the same method as the original calculator
  metrics_percentiles <- get_percentiles(Results_df,condesed_table$COMID[i])
  
  # Construct the file path for the percentiles
  file_path_percentiles <- file.path(here(), "Outputs", gage_name_cleaned, paste0(gage_name_cleaned, "_Metric_Percentiles.csv"))
  
  # Write the CSV file
  write_csv(metrics_percentiles, file = file_path_percentiles)
  
  #Make the html figure for the
  single_series_HTML(flow,Results_df,gage_name_cleaned,site_name)
  
  #Make a new directory for the the boxplots
  new_dir_box_plots <- paste("Outputs",condesed_table$Gage_ID[i],"boxplots",sep = "/")
  
  #Now the new directory 
  dir.create(new_dir_box_plots)
  
  #Now call the box plot function
  single_box_plots(Results_df,site_name,new_dir_box_plots)
  
  
  
}
