#This script allows the user to compare multiple gages (up to three) the


#Required libraries
library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);
library(ggplot2);library(dplyr)

#Source the function that are in other scripts 
source(here("Preprocess","Preprocess_data.R"))
source(here("Calculations","Get_metrics.R"))
source(here("Post_Processing_Files","Box_plot_creation.R")) 
source(here("Post_Processing_Files","HTML_plotting.R"))  
source(here("Post_Processing_Files","Metrics_percentiles.R"))



compare_gages <- function(){
  
  cat("Enter the number of hydrographs to be evaluated,  between 2 and 3: ")
  
  evaluation_num <- as.integer(readline(""))
  
  if (evaluation_num<2 | evaluation_num >3) {
    #Tell the user why the program is ending
    cat("the entered number if outside of the limit allowed")
    # Terminate the program execution
    q("no")
  }
  

  if (evaluation_num == 2){
    #Get comid for the gauge site
    comid_1 <- readline("Enter the comid of the first gage that is going to be evaluated: ")
    
    gage_check_1 <- readline("Are you using your time series data or a USGS gage site (TS for time series or G for USGS gage data): ")
    
    #Check to see if which term was entered and get the important information
    if (gage_check_1 == "TS"){
      #Get the file location of time series
      cat("Please enter the full file location of the csv file with the flow data.\n The file needs to have a column called 'date' with a MM/DD/YYYY, and a column called 'flow' with the flow data.")
      input_time_series_loc_1 <- readline("File Path Here: ")
      
      #read in the data
      flow_1 <- read.csv(input_time_series_loc_1, header = T) %>% #Enter first flow data
        mutate(date = as.Date(date, "%m/%d/%Y"))
      
      #Remove rows that contains all NA's
      flow_1 <- flow_1[rowSums(is.na(flow_1)) != ncol(flow_1), ]
      
      site_name_1 <- readline("Please enter the name that you want to use to describe this flow data: ")
      
      gage_id_1 <- site_name
    }
    else if(gage_check_1 == "G"){
      gage_id_1 <- readline("Please enter USGS site number or a 3-letter CDEC station ID, the site must be in the California: ")
      
      if(nchar(gage_id_1) == 3){
        cat("Please enter the sensor number you want to get the data from.  \nCommon sensors are 8 for full natural flow data and 41 for daily mean flow: ")
        sensor_ID_1 <- readline("Sensor ID: ")
        
        cat("\n Data for the calculator must be daily")
        
        raw_flow <- get_cdec(gage_id_1,sensor_ID_1,"D")
        
        #Take just the date and flow data
        raw_flow <-  raw_flow[,c("datetime","value")]
        
        #Rename the data so it matches the user input files
        flow_1 <- raw_flow %>%
          rename("date" = "datetime", "flow" = "value")
        
        site_name_1 <- paste("CDEC Gage:",gage_id_1)
      }else {
        flow_1 <- USGS_gage_flow(gage_id_1)
        
        gage_data_1 <- readNWISuv(siteNumbers = gage_id_1,
                                  parameterCd = "00060")
        
        siteInfo_1 <- attr(gage_data_1, "siteInfo")
        
        site_name_1 <- siteInfo_1$station_nm
      }
    }
    else if (gage_check_1 != "TS" | gage_check_1 != "G"){
      #Tell the user why the program is ending
      cat("Invalid input entered")
      # Terminate the program execution
      q("no")
    }
    
    #Get comid for the gauge site
    comid_2 <- readline("Enter the comid of the second gage that is going to be evaluated: ")
    
    gage_check_2 <- readline("Are you using your time series data or a USGS gage site (TS for time series or G for USGS gage data): ")
    
    #Check to see if which term was entered and get the important information
    if (gage_check_2 == "TS"){
      #Get the file location of time series
      cat("Please enter the full file location of the csv file with the flow data.\n The file needs to have a column called 'date' with a MM/DD/YYYY, and a column called 'flow' with the flow data.")
      input_time_series_loc_2 <- readline("File Path Here: ")
      
      #read in the data
      flow_2 <- read.csv(input_time_series_loc_2, header = T) %>% #Enter first flow data
        mutate(date = as.Date(date, "%m/%d/%Y"))
      
      #Remove rows that contains all NA's
      flow_2 <- flow_2[rowSums(is.na(flow_2)) != ncol(flow_2), ]
      
      site_name_2 <- readline("Please enter the name that you want to use to describe this flow data: ")
      
    
      gage_id_2 <- site_name_2
    }
    else if(gage_check_2 == "G"){
      gage_id_2 <- readline("Please enter USGS site number or a 3-letter CDEC station ID, the site must be in the California: ")
      
      if(nchar(gage_id_2) == 3){
        cat("Please enter the sensor number you want to get the data from.  \nCommon sensors are 8 for full natural flow data and 41 for daily mean flow: ")
        sensor_ID_2 <- readline("Sensor ID: ")
        
        cat("\n Data for the calculator must be daily")
        
        raw_flow <- get_cdec(gage_id_2,sensor_ID_2,"D")
        
        #Take just the date and flow data
        raw_flow <-  raw_flow[,c("datetime","value")]
        
        #Rename the data so it matches the user input files
        flow_2 <- raw_flow %>%
          rename("date" = "datetime", "flow" = "value")
        
        site_name_2 <- paste("CDEC Gage:",gage_id_2)
      }else {
        flow_2 <- USGS_gage_flow(gage_id_2)
        
        gage_data_2 <- readNWISuv(siteNumbers = gage_id_2,
                                  parameterCd = "00060")
        
        siteInfo_2 <- attr(gage_data_2, "siteInfo")
        
        site_name_2 <- siteInfo_2$station_nm
      }
    }
    else if (gage_check_2 != "TS" | gage_check_2 != "G"){
      #Tell the user why the program is ending
      cat("Invalid input entered")
      # Terminate the program execution
      q("no")
    }
    
    #Get the results for each of the results
    Results_df_1  <- flow_metrics_calculations(flow_1)
    Results_df_2  <- flow_metrics_calculations(flow_2)
    
    #Make a name combining the two gages' names 
    output_file_name <- paste0(gage_id_1," and ", gage_id_2," comparison")
    
    #make a new file path for this output directory
    new_dir <- paste("Outputs",output_file_name,sep = "/")
    
    #Now the new folder with the file path 
    dir.create(new_dir)
    
    # Clean up the gage name to remove leading/trailing spaces
    gage_name_cleaned_1 <- trimws(gage_id_1)
    gage_name_cleaned_2 <- trimws(gage_id_2)
    
    # Construct the file path
    file_path_1 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_1, "_Metrics.csv"))
    file_path_2 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_2, "_Metrics.csv"))
    
    # Write the CSV file
    write_csv(Results_df_1, file = file_path_1)
    write_csv(Results_df_2, file = file_path_2)
    
    #Get the metrics percentiles following the same method as the original calculator
    metrics_percentiles_1 <- get_percentiles(Results_df_1,comid_1)
    metrics_percentiles_2 <- get_percentiles(Results_df_2,comid_2)
    
    # Construct the file path for the percentiles
    file_path_percentiles_1 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_1, "_Metric_Percentiles.csv"))
    file_path_percentiles_2 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_2, "_Metric_Percentiles.csv"))
    
    # Write the CSV file
    write_csv(metrics_percentiles_1, file = file_path_percentiles_1)
    write_csv(metrics_percentiles_2, file = file_path_percentiles_2)
    
    #Make the html figure for the
    HTML_comparison(flow_1 =  flow_1,flow_2 = flow_2,metrics_1 = Results_df_1,metrics_2  =Results_df_2, output_file_name = output_file_name)
    
    #Make a new directory for the the boxplots
    new_dir_box_plots <- paste("Outputs",output_file_name,"boxplots",sep = "/")
    
    #Now the new directory 
    dir.create(new_dir_box_plots)
    
    #Now call the box plot function
    comparison_boxplots(Metric_dataframe_1 = Results_df_1,name_1 = gage_name_cleaned_1,Metric_dataframe_2 =  Results_df_2,name_2 = gage_name_cleaned_2, save_loc =  new_dir_box_plots)
    
    
    
  }
  
  if (evaluation_num == 3){
    #Get comid for the gauge site
    comid_1 <- readline("Enter the comid of the first gage that is going to be evaluated: ")
    
    gage_check_1 <- readline("Are you using your time series data or a USGS gage site (TS for time series or G for USGS gage data): ")
    
    #Check to see if which term was entered and get the important information
    if (gage_check_1 == "TS"){
      #Get the file location of time series
      cat("Please enter the full file location of the csv file with the flow data.\n The file needs to have a column called 'date' with a MM/DD/YYYY, and a column called 'flow' with the flow data.")
      input_time_series_loc_1 <- readline("File Path Here: ")
      
      #read in the data
      flow_1 <- read.csv(input_time_series_loc_1, header = T) %>% #Enter first flow data
        mutate(date = as.Date(date, "%m/%d/%Y"))
      
      #Remove rows that contains all NA's
      flow_1 <- flow_1[rowSums(is.na(flow_1)) != ncol(flow_1), ]
      
      site_name_1 <- readline("Please enter the name that you want to use to describe this flow data: ")
      
      gage_id_1 <- site_name_1
    }
    else if(gage_check_1 == "G"){
      gage_id_1 <- readline("Please enter USGS site number or a 3-letter CDEC station ID, the site must be in the California: ")
      
      if(nchar(gage_id_1) == 3){
        cat("Please enter the sensor number you want to get the data from.  \nCommon sensors are 8 for full natural flow data and 41 for daily mean flow: ")
        sensor_ID_1 <- readline("Sensor ID: ")
        
        cat("\n Data for the calculator must be daily")
        
        raw_flow <- get_cdec(gage_id_1,sensor_ID_1,"D")
        
        #Take just the date and flow data
        raw_flow <-  raw_flow[,c("datetime","value")]
        
        #Rename the data so it matches the user input files
        flow_1 <- raw_flow %>%
          rename("date" = "datetime", "flow" = "value")
        
        site_name_1 <- paste("CDEC Gage:",gage_id_1)
      }else {
        flow_1 <- USGS_gage_flow(gage_id_1)
        
        gage_data_1 <- readNWISuv(siteNumbers = gage_id_1,
                                  parameterCd = "00060")
        
        siteInfo_1 <- attr(gage_data_1, "siteInfo")
        
        site_name_1 <- siteInfo_1$station_nm
      }
    }
    else if (gage_check_1 != "TS" | gage_check_1 != "G"){
      #Tell the user why the program is ending
      cat("Invalid input entered")
      # Terminate the program execution
      q("no")
    }
    
    #Get comid for the gauge site
    comid_2 <- readline("Enter the comid of the second gage that is going to be evaluated: ")
    
    gage_check_2 <- readline("Are you using your time series data or a USGS gage site (TS for time series or G for USGS gage data): ")
    
    #Check to see if which term was entered and get the important information
    if (gage_check_2 == "TS"){
      #Get the file location of time series
      cat("Please enter the full file location of the csv file with the flow data.\n The file needs to have a column called 'date' with a MM/DD/YYYY, and a column called 'flow' with the flow data.")
      input_time_series_loc_2 <- readline("File Path Here: ")
      
      #read in the data
      flow_2 <- read.csv(input_time_series_loc_2, header = T) %>% #Enter first flow data
        mutate(date = as.Date(date, "%m/%d/%Y"))
      
      #Remove rows that contains all NA's
      flow_2 <- flow_2[rowSums(is.na(flow_2)) != ncol(flow_2), ]
      
      site_name_2 <- readline("Please enter the name that you want to use to describe this flow data: ")
      
      gage_id_2 <- site_name_2
    }
    else if(gage_check_2 == "G"){
      gage_id_2 <- readline("Please enter USGS site number or a 3-letter CDEC station ID, the site must be in the California: ")
      
      if(nchar(gage_id_2) == 3){
        cat("Please enter the sensor number you want to get the data from.  \nCommon sensors are 8 for full natural flow data and 41 for daily mean flow: ")
        sensor_ID_2 <- readline("Sensor ID: ")
        
        cat("\n Data for the calculator must be daily")
        
        raw_flow <- get_cdec(gage_id_2,sensor_ID_2,"D")
        
        #Take just the date and flow data
        raw_flow <-  raw_flow[,c("datetime","value")]
        
        #Rename the data so it matches the user input files
        flow_2 <- raw_flow %>%
          rename("date" = "datetime", "flow" = "value")
        
        site_name_2 <- paste("CDEC Gage:",gage_id_2)
      }else {
        flow_2 <- USGS_gage_flow(gage_id_2)
        
        gage_data_2 <- readNWISuv(siteNumbers = gage_id_2,
                                  parameterCd = "00060")
        
        siteInfo_2 <- attr(gage_data_2, "siteInfo")
        
        site_name_2 <- siteInfo_2$station_nm
      }
    }
    else if (gage_check_2 != "TS" | gage_check_2 != "G"){
      #Tell the user why the program is ending
      cat("Invalid input entered")
      # Terminate the program execution
      q("no")
    }
    
    #Get comid for the gauge site
    comid_3 <- readline("Enter the comid of the second gage that is going to be evaluated: ")
    
    gage_check_3 <- readline("Are you using your time series data or a USGS gage site (TS for time series or G for USGS gage data): ")
    
    #Check to see if which term was entered and get the important information
    if (gage_check_3 == "TS"){
      #Get the file location of time series
      cat("Please enter the full file location of the csv file with the flow data.\n The file needs to have a column called 'date' with a MM/DD/YYYY, and a column called 'flow' with the flow data.")
      input_time_series_loc_3 <- readline("File Path Here: ")
      
      #read in the data
      flow_3 <- read.csv(input_time_series_loc_3, header = T) %>% #Enter first flow data
        mutate(date = as.Date(date, "%m/%d/%Y"))
      
      #Remove rows that contains all NA's
      flow_3 <- flow_3[rowSums(is.na(flow_3)) != ncol(flow_3), ]
      
      site_name_3 <- readline("Please enter the name that you want to use to describe this flow data: ")
      
      gage_id_3 <- site_name_3
    }
    else if(gage_check_3 == "G"){
      gage_id_3 <- readline("Please enter USGS site number or a 3-letter CDEC station ID, the site must be in the California: ")
      
      if(nchar(gage_id_3) == 3){
        cat("Please enter the sensor number you want to get the data from.  \nCommon sensors are 8 for full natural flow data and 41 for daily mean flow: ")
        sensor_ID_3 <- readline("Sensor ID: ")
        
        cat("\n Data for the calculator must be daily")
        
        raw_flow <- get_cdec(gage_id_3,sensor_ID_3,"D")
        
        #Take just the date and flow data
        raw_flow <-  raw_flow[,c("datetime","value")]
        
        #Rename the data so it matches the user input files
        flow_3 <- raw_flow %>%
          rename("date" = "datetime", "flow" = "value")
        
        site_name_3 <- paste("CDEC Gage:",gage_id_3)
      }else {
        flow_3 <- USGS_gage_flow(gage_id_3)
        
        gage_data_3 <- readNWISuv(siteNumbers = gage_id_3,
                                  parameterCd = "00060")
        
        siteInfo_3 <- attr(gage_data_3, "siteInfo")
        
        site_name_3 <- siteInfo_3$station_nm
      }
    }
    else if (gage_check_3 != "TS" | gage_check_3 != "G"){
      #Tell the user why the program is ending
      cat("Invalid input entered")
      # Terminate the program execution
      q("no")
    }
  
    
    
    #Get the results for each of the results
    Results_df_1  <- flow_metrics_calculations(flow_1)
    Results_df_2  <- flow_metrics_calculations(flow_2)
    Results_df_3  <- flow_metrics_calculations(flow_3)
    
    #Make a name combining the two gages' names 
    output_file_name <- paste0(gage_id_1,",", gage_id_2," and ",gage_id_3," comparison")
    
    #make a new file path for this output directory
    new_dir <- paste("Outputs",output_file_name,sep = "/")
    
    #Now the new folder with the file path 
    dir.create(new_dir)
    
    # Clean up the gage name to remove leading/trailing spaces
    gage_name_cleaned_1 <- trimws(gage_id_1)
    gage_name_cleaned_2 <- trimws(gage_id_2)
    gage_name_cleaned_3 <- trimws(gage_id_3)
    
    # Construct the file path
    file_path_1 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_1, "_Metrics.csv"))
    file_path_2 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_2, "_Metrics.csv"))
    file_path_3  <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_3, "_Metrics.csv"))
    
    # Write the CSV file
    write_csv(Results_df_1, file = file_path_1)
    write_csv(Results_df_2, file = file_path_2)
    write_csv(Results_df_3, file = file_path_3)
    
    #Get the metrics percentiles following the same method as the original calculator
    metrics_percentiles_1 <- get_percentiles(Results_df_1,comid_1)
    metrics_percentiles_2 <- get_percentiles(Results_df_2,comid_2)
    metrics_percentiles_3 <- get_percentiles(Results_df_3,comid_3)
    
    # Construct the file path for the percentiles
    file_path_percentiles_1 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_1, "_Metric_Percentiles.csv"))
    file_path_percentiles_2 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_2, "_Metric_Percentiles.csv"))
    file_path_percentiles_3 <- file.path(here(), "Outputs", output_file_name, paste0(gage_name_cleaned_3, "_Metric_Percentiles.csv"))
    
    # Write the CSV file
    write_csv(metrics_percentiles_1, file = file_path_percentiles_1)
    write_csv(metrics_percentiles_2, file = file_path_percentiles_2)
    write_csv(metrics_percentiles_3, file = file_path_percentiles_3)
    
    #Make the html figure for the
    HTML_comparison(flow_1,flow_2,flow_3,Results_df_1,Results_df_2,Results_df_3,output_file_name)
    
    #Make a new directory for the the boxplots
    new_dir_box_plots <- paste("Outputs",output_file_name,"boxplots",sep = "/")
    
    #Now the new directory 
    dir.create(new_dir_box_plots)
    
    #Now call the box plot function
    comparison_boxplots(Metric_dataframe_1 = Results_df_1,name_1 = gage_name_cleaned_1,Metric_dataframe_2 =  Results_df_2,name_2 = gage_name_cleaned_2,Metric_dataframe_3 = Results_df_3,name_3 = name_3, save_loc =  new_dir_box_plots)
    
    
    
  }
}