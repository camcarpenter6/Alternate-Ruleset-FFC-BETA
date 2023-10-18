#This script gets the flow metrics given flow data

#Required libraries
library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);
library(ggplot2);library(dplyr)

#Source the function that are in other scripts 
source(here("Preprocess","Preprocess_data.R"))

source(here("Utils","Spring_Metrics_and_Dry_Season_Timing.R"))

source(here("Utils","Wet_and_Dry_Magnitudes.R"))

source(here("Utils","Fall_and_Wet_Timing.R"))

source(here("Utils","Wet_Season_Peak_Flows.R"))

source(here("Utils","Annual_Metrics.R"))


flow_metrics_calculations <- function(flow) {
  
  
  #Format the data for the calculator 
  FlowYear <- attach_water_year_data(flow, date_field = "date")  
    #mutate(date = format(date, "%m/%d/%Y")) #reformat date 
  
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
  
  return(Results_df)
}
