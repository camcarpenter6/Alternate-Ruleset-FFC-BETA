#This is the secondary script for running the altered function flow metrics calculator calculator 
#It will allow you to calculate metrics for a single site or for up-to 3 sets of data and compare them
#This code can not batch calculate metrics like the "Main.R" script can

#First check to see if all the required packages are installed

source("./required_packages.R")

#Now the packages are referenced
library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);
library(ggplot2);library(dplyr); library(pracma);library(here);library(dataRetrieval)

## Load all the functions required for the calculator
#Source the function that are in other scripts 
source(here("Preprocess","Preprocess_data.R"))

source(here("Utils","Spring_Metrics_and_Dry_Season_Timing.R"))

source(here("Utils","Wet_and_Dry_Magnitudes.R"))

source(here("Utils","Fall_and_Wet_Timing.R"))

source(here("Utils","Wet_Season_Peak_Flows.R"))

source(here("Utils","Annual_Metrics.R"))

source(here("Preprocess","Download_data.R"))

source(here("Post_Processing_Files","Metrics_percentiles.R"))

source(here("Post_Processing_Files","HTML_plotting.R"))

source(here("Post_Processing_Files","Box_plot_creation.R"))

#Make a function that downloads the gage data and reports the site name
download_data <- function(gage_id){
  
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
    
    #site_name <- paste("CDEC Gage:",gage_id)
  }else {
    flow <- USGS_gage_flow(gage_id)
    
    #gage_data <- readNWISuv(siteNumbers = gage_id,
                            #parameterCd = "00060")
    
    #siteInfo <- attr(gage_data, "siteInfo")
    
    #site_name <- siteInfo$station_nm
  }
  return(flow)
  
}

#This function calculates all the metrics for the comparison functions it is originally
#Set to use the orgiinal calculators method to calculate the peak flows

flow_metrics_calculation_comparison <- function(flow, Original_peaks_Method = TRUE) {
  
  
  #Format the data for the calculator 
  FlowYear <- attach_water_year_data(flow, date_field = "date") %>% 
    mutate(date = format(date, "%m/%d/%Y")) #reformat date 
  
  #First run the spring and timing function
  SP_met_and_DS_Tim <- Altered_Spring_Recession(FlowYear)
  
  #Now Calculate the Fall and Wet season timing
  FA_Mets_and_Wet_Tim <- Altered_Fall_Wet_Timing(FlowYear,SP_met_and_DS_Tim$DS_Tim)
  
  #Now we will get the magnitudes for the seasons
  Wet_DS_Mags_and_Dur <- Wet_Dry_Season_Non_Tim_Metrics(FlowYear,SP_met_and_DS_Tim$SP_Tim,SP_met_and_DS_Tim$DS_Tim, FA_Mets_and_Wet_Tim$Wet_Tim)
  
  #Now get the Annual metrics 
  Ann_Metrics <- Annual_Metrics(FlowYear)
  
  #Finally get the peak flow metrics
  Peak_metrics <- calc_winter_highflow_annual_combined(FlowYear = FlowYear,Original_method = Original_peaks_Method)
  
  #Make a data frame of the results
  Year <- unique(FlowYear$water_year)
  
  #you can change the name of the data frame to make more easily identifiable 
  #Update the name of the data frame below to represent the site you're working to examine
  Results_df <- data.frame(Year,Wet_DS_Mags_and_Dur$DS_Dur_WS,SP_met_and_DS_Tim$DS_Tim,Wet_DS_Mags_and_Dur$DS_Mag_50,Wet_DS_Mags_and_Dur$DS_Mag_90,
                           FA_Mets_and_Wet_Tim$FA_Dur,FA_Mets_and_Wet_Tim$FA_Mag,FA_Mets_and_Wet_Tim$FA_Tim,FA_Mets_and_Wet_Tim$FA_Dif_num,
                           SP_met_and_DS_Tim$SP_ROC,SP_met_and_DS_Tim$SP_ROC_Max,SP_met_and_DS_Tim$SP_Dur,SP_met_and_DS_Tim$SP_Mag,
                           SP_met_and_DS_Tim$SP_Tim,Wet_DS_Mags_and_Dur$Wet_BFL_Dur,Wet_DS_Mags_and_Dur$Wet_BFL_Mag_10,
                           Wet_DS_Mags_and_Dur$Wet_BFL_Mag_50,FA_Mets_and_Wet_Tim$Wet_Tim,Peak_metrics$Peak_Tim_10,Peak_metrics$Peak_Tim_2,
                           Peak_metrics$Peak_Tim_5,Peak_metrics$Peak_Dur_10,Peak_metrics$Peak_Dur_2,Peak_metrics$Peak_Dur_5,Peak_metrics$Peak_10,
                           Peak_metrics$Peak_2,Peak_metrics$Peak_5,Peak_metrics$Peak_Fre_10,Peak_metrics$Peak_Fre_2,Peak_metrics$Peak_Fre_5,
                           Ann_Metrics$Mean_Ann_Flow,Ann_Metrics$WY_Cat)
  
  #Since the data frame will take the names of the list we need to rename the columns to match the original Calculator
  Results_df  <- Results_df  %>%
    rename("DS_Dur_WS" = "Wet_DS_Mags_and_Dur.DS_Dur_WS",
           "DS_Tim" = "SP_met_and_DS_Tim.DS_Tim",
           "DS_Mag_50" = "Wet_DS_Mags_and_Dur.DS_Mag_50",
           "DS_Mag_90" ="Wet_DS_Mags_and_Dur.DS_Mag_90",
           "FA_Dur" = "FA_Mets_and_Wet_Tim.FA_Dur",
           "FA_Mag" = "FA_Mets_and_Wet_Tim.FA_Mag",
           "FA_Tim" = "FA_Mets_and_Wet_Tim.FA_Tim",
           "FA_Dif_num" = "FA_Mets_and_Wet_Tim.FA_Dif_num",
           "SP_ROC" = "SP_met_and_DS_Tim.SP_ROC",
           "SP_ROC_Max" = "SP_met_and_DS_Tim.SP_ROC_Max",
           "SP_Dur" = "SP_met_and_DS_Tim.SP_Dur",
           "SP_Mag" = "SP_met_and_DS_Tim.SP_Mag",
           "SP_Tim" ="SP_met_and_DS_Tim.SP_Tim",
           "Wet_BFL_Dur" = "Wet_DS_Mags_and_Dur.Wet_BFL_Dur",
           "Wet_BFL_Mag_10" = "Wet_DS_Mags_and_Dur.Wet_BFL_Mag_10",
           "Wet_BFL_Mag_50" = "Wet_DS_Mags_and_Dur.Wet_BFL_Mag_50",
           "Wet_Tim" = "FA_Mets_and_Wet_Tim.Wet_Tim",
           "Peak_Tim_10" ="Peak_metrics.Peak_Tim_10",
           "Peak_Tim_2" = "Peak_metrics.Peak_Tim_2",
           "Peak_Tim_5" = "Peak_metrics.Peak_Tim_5",
           "Peak_Dur_10" = "Peak_metrics.Peak_Dur_10",
           "Peak_Dur_2" = "Peak_metrics.Peak_Dur_2",
           "Peak_Dur_5" = "Peak_metrics.Peak_Dur_5",
           "Peak_10" = "Peak_metrics.Peak_10",
           "Peak_2" = "Peak_metrics.Peak_2",
           "Peak_5" = "Peak_metrics.Peak_5",
           "Peak_Fre_10" = "Peak_metrics.Peak_Fre_10",
           "Peak_Fre_2" = "Peak_metrics.Peak_Fre_2",
           "Peak_Fre_5" = "Peak_metrics.Peak_Fre_5",
           "Mean_Ann_Flow" ="Ann_Metrics.Mean_Ann_Flow",
           "WY_Cat" = "Ann_Metrics.WY_Cat")
  
  return(Results_df)
}

# Single site calculation  ------------------------------------------------

#This section will produce metrics and figures for a single site

#You can either use our own timeseries 
#First the code for your own time series
#give the next line of code your flow data location the data needs to have a "date" and "flow" column
flow <- read.csv("~user/someplace/flowdata.csv", header = T) %>% #Enter first flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))

#Remove rows that contains all NA's
flow <- flow[rowSums(is.na(flow)) != ncol(flow), ]

#Or you can download data from USGS or CDEC

#Enter your gage id,the three character id for CDEC or the USGS gage id, set as SF Kern River NR Onyx
gage_id <- 11189500

flow <- download_data(gage_id)

#Now give the flow data a name
site_name <- "Your data's name here"

#Enter the corresponding comid for data
comid <- 14961121

#Format the data for the calculator 
FlowYear <- attach_water_year_data(flow, date_field = "date") %>% 
  mutate(date = format(date, "%m/%d/%Y")) #reformat date 

#First run the spring and timing function
SP_met_and_DS_Tim <- Altered_Spring_Recession(FlowYear)

#Now Calculate the Fall and Wet season timing
FA_Mets_and_Wet_Tim <- Altered_Fall_Wet_Timing(FlowYear,SP_met_and_DS_Tim$DS_Tim)

#Now we will get the magnitudes for the seasons
Wet_DS_Mags_and_Dur <- Wet_Dry_Season_Non_Tim_Metrics(FlowYear,SP_met_and_DS_Tim$SP_Tim,SP_met_and_DS_Tim$DS_Tim, FA_Mets_and_Wet_Tim$Wet_Tim)

#Now get the Annual metrics 
Ann_Metrics <- Annual_Metrics(FlowYear)

#Finally get the peak flow metrics
Peak_metrics <- calc_winter_highflow_annual_combined(FlowYear = FlowYear,Original_method = TRUE)

#Make a data frame of the results
Year <- unique(FlowYear$water_year)

#you can change the name of the data frame to make more easily identifiable 
#Update the name of the data frame below to represent the site you're working to examine
Results_df <- data.frame(Year,Wet_DS_Mags_and_Dur$DS_Dur_WS,SP_met_and_DS_Tim$DS_Tim,Wet_DS_Mags_and_Dur$DS_Mag_50,Wet_DS_Mags_and_Dur$DS_Mag_90,
                         FA_Mets_and_Wet_Tim$FA_Dur,FA_Mets_and_Wet_Tim$FA_Mag,FA_Mets_and_Wet_Tim$FA_Tim,FA_Mets_and_Wet_Tim$FA_Dif_num,
                         SP_met_and_DS_Tim$SP_ROC,SP_met_and_DS_Tim$SP_ROC_Max,SP_met_and_DS_Tim$SP_Dur,SP_met_and_DS_Tim$SP_Mag,
                         SP_met_and_DS_Tim$SP_Tim,Wet_DS_Mags_and_Dur$Wet_BFL_Dur,Wet_DS_Mags_and_Dur$Wet_BFL_Mag_10,
                         Wet_DS_Mags_and_Dur$Wet_BFL_Mag_50,FA_Mets_and_Wet_Tim$Wet_Tim,Peak_metrics$Peak_Tim_10,Peak_metrics$Peak_Tim_2,
                         Peak_metrics$Peak_Tim_5,Peak_metrics$Peak_Dur_10,Peak_metrics$Peak_Dur_2,Peak_metrics$Peak_Dur_5,Peak_metrics$Peak_10,
                         Peak_metrics$Peak_2,Peak_metrics$Peak_5,Peak_metrics$Peak_Fre_10,Peak_metrics$Peak_Fre_2,Peak_metrics$Peak_Fre_5,
                         Ann_Metrics$Mean_Ann_Flow,Ann_Metrics$WY_Cat)

#Since the data frame will take the names of the list we need to rename the columns to match the original Calculator
Results_df  <- Results_df  %>%
  rename("DS_Dur_WS" = "Wet_DS_Mags_and_Dur.DS_Dur_WS",
         "DS_Tim" = "SP_met_and_DS_Tim.DS_Tim",
         "DS_Mag_50" = "Wet_DS_Mags_and_Dur.DS_Mag_50",
         "DS_Mag_90" ="Wet_DS_Mags_and_Dur.DS_Mag_90",
         "FA_Dur" = "FA_Mets_and_Wet_Tim.FA_Dur",
         "FA_Mag" = "FA_Mets_and_Wet_Tim.FA_Mag",
         "FA_Tim" = "FA_Mets_and_Wet_Tim.FA_Tim",
         "FA_Dif_num" = "FA_Mets_and_Wet_Tim.FA_Dif_num",
         "SP_ROC" = "SP_met_and_DS_Tim.SP_ROC",
         "SP_ROC_Max" = "SP_met_and_DS_Tim.SP_ROC_Max",
         "SP_Dur" = "SP_met_and_DS_Tim.SP_Dur",
         "SP_Mag" = "SP_met_and_DS_Tim.SP_Mag",
         "SP_Tim" ="SP_met_and_DS_Tim.SP_Tim",
         "Wet_BFL_Dur" = "Wet_DS_Mags_and_Dur.Wet_BFL_Dur",
         "Wet_BFL_Mag_10" = "Wet_DS_Mags_and_Dur.Wet_BFL_Mag_10",
         "Wet_BFL_Mag_50" = "Wet_DS_Mags_and_Dur.Wet_BFL_Mag_50",
         "Wet_Tim" = "FA_Mets_and_Wet_Tim.Wet_Tim",
         "Peak_Tim_10" ="Peak_metrics.Peak_Tim_10",
         "Peak_Tim_2" = "Peak_metrics.Peak_Tim_2",
         "Peak_Tim_5" = "Peak_metrics.Peak_Tim_5",
         "Peak_Dur_10" = "Peak_metrics.Peak_Dur_10",
         "Peak_Dur_2" = "Peak_metrics.Peak_Dur_2",
         "Peak_Dur_5" = "Peak_metrics.Peak_Dur_5",
         "Peak_10" = "Peak_metrics.Peak_10",
         "Peak_2" = "Peak_metrics.Peak_2",
         "Peak_5" = "Peak_metrics.Peak_5",
         "Peak_Fre_10" = "Peak_metrics.Peak_Fre_10",
         "Peak_Fre_2" = "Peak_metrics.Peak_Fre_2",
         "Peak_Fre_5" = "Peak_metrics.Peak_Fre_5",
         "Mean_Ann_Flow" ="Ann_Metrics.Mean_Ann_Flow",
         "WY_Cat" = "Ann_Metrics.WY_Cat")


#Make a new file path for this output directory
new_dir <- paste("Outputs",gage_id,sep = "/")

#Now make a new directory for outputs
dir.create(new_dir)

# Clean up the gage name to remove leading/trailing spaces
gage_name_cleaned <- trimws(gage_id)

# Construct the file path
file_path <- file.path(here(), "Outputs", gage_name_cleaned, paste0(gage_name_cleaned, "_Metrics.csv"))

# Write the CSV file
write_csv(Results_df, file = file_path)

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


# Flow Comparison Code ----------------------------------------------------

#This code allows the user to compare two or three gages 

#first we need to define all of the flow data
#you can the flow data location the data needs to have a "date" and "flow" column
flow_1 <- read.csv("~user/someplace/flowdata.csv", header = T) %>% #Enter first flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))
flow_2 <- read.csv("~user/someplace/flowdata.csv", header = T) %>% #Enter first flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))
flow_3 <- read.csv("~user/someplace/flowdata.csv", header = T) %>% #Enter first flow data
  mutate(date = as.Date(date, "%m/%d/%Y"))

#Remove rows that contains all NA's
flow_1 <- flow_1[rowSums(is.na(flow_1)) != ncol(flow_1), ]
flow_2 <- flow_2[rowSums(is.na(flow_2)) != ncol(flow_2), ]
flow_3 <- flow_3[rowSums(is.na(flow_3)) != ncol(flow_3), ]

#Or you can download data from USGS or CDEC

#Enter your gage id,the three character id for CDEC or the USGS gage id, set as SF Kern River NR Onyx
gage_id_1 <- 11189500
gage_id_2 <- 11289650
gage_id_3 <- 11189500

flow_1 <- download_data(gage_id_1)
flow_2 <- download_data(gage_id_2)
flow_3 <- download_data(gage_id_3)

#Now give the flow data a name
site_name <- "Your data's name here"

#Enter the corresponding comids for data
comid_1 <- 14961121
comid_2 <- 903423
comid_3 <- 14961121

#Calcuate the metrics using function defined above for each set of flow data
#The function is set to run using the same method as the original calcuator
#Change to false to to use the LP3 method
Results_df_1 <- flow_metrics_calculation_comparison(flow = flow_1, Original_peaks_Method = TRUE)
Results_df_2 <- flow_metrics_calculation_comparison(flow = flow_2, Original_peaks_Method = TRUE)
Results_df_3 <- flow_metrics_calculation_comparison(flow = flow_3, Original_peaks_Method = TRUE)


#Make a name combining the gages' ids, only run ONE of the two lines of code 
output_file_name <- paste0(gage_id_1," and ", gage_id_2," comparison")
#OR
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
HTML_comparison(flow_1 =  flow_1,flow_2 = flow_2,metrics_1 = Results_df_1,metrics_2 =  Results_df_2,output_file_name =  output_file_name)
#OR
HTML_comparison(flow_1 =  flow_1,flow_2 =  flow_2,flow_3 =  flow_3,metrics_1 =Results_df_1,metrics_2 =Results_df_2,metrics_3 = Results_df_3,output_file_name =output_file_name)

#Make a new directory for the the boxplots
new_dir_box_plots <- paste("Outputs",output_file_name,"boxplots",sep = "/")

#Now the new directory 
dir.create(new_dir_box_plots)

#Now call the box plot function
comparison_boxplots(Metric_dataframe_1 = Results_df_1,name_1 = gage_name_cleaned_1,Metric_dataframe_2 =  Results_df_2,name_2 = gage_name_cleaned_2, save_loc =  new_dir_box_plots)
#OR
comparison_boxplots(Metric_dataframe_1 = Results_df_1,name_1 = gage_name_cleaned_1,Metric_dataframe_2 =  Results_df_2,name_2 = gage_name_cleaned_2,Metric_dataframe_3 = Results_df_3,name_3 = name_3, save_loc =  new_dir_box_plots)

