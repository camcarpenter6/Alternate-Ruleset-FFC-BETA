### This function gets the calculation data to be used in the calculator


library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);
library(ggplot2);library(dplyr)


###Bring in the functions needed for this function
source(here("Preprocess","Download_data.R")) 
source(here("Calculations","Flow_Comparison.R"))




###First we need to get the users data

analysis_type<- c()
batch_or_single <- c()

# User Input --------------------------------------------------------------

#We need to see if the user wants to compare or data for just get results
analysis_type<- readline("Do you want to compare results (maximum of 3 gauges) or look at individual results no maximum \n Enter (compare) to compare or (indv) for idividual results ")

if(analysis_type == "compare"){
  
  compare_gages()

  
}else if(analysis_type == "indv"){
  
  batch_or_single<- readline("Do you want to batch all the results ('batch') or run a singular gage 'singular'")
   
  if(batch_or_single == "singular"){
   
    source(here("Calculations","Single_Calc.R"))
    
  }
  else if(batch_or_single == "batch"){
    cat("Make sure a csv with all of the gages that need to be run are located in the 'Input_data' folder \n 
        all the files in the folder will be anlalyzed")
    
    source(here("Calculations","Batch_Calc.R"))

    
    #input_gages <-
     # list.files(path = "./Input_data/", pattern = "*.csv") %>% 
    #  map_df(~read_csv(.))
    #df
  }
}else{
   #Tell the user why the program is ending
   cat("the entered an unallowed type of analysis, please try again")
   # Terminate the program execution
   q("no")
 }

