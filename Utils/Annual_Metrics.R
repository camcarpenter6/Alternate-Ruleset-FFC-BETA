
library(lubridate)

#This function calculates the the mean annual flow and their water year category 
#The input for this function is the post processed flow data with the dates, flow
#and water year at a minimum
Annual_Metrics <- function(FlowYear) {
  #get the number of water years and which years they are
  WYs <- unique(FlowYear$water_year)
  
  #Set up the output vectors
  Mean_Ann_Flow <-c()
  WY_Rank_WtoD <- c()
  WY_Cat <- c()
  count_0 <- c()
  
  
  #Cycle through all the water years
  for (i in 1:length(WYs)) {
    #Filter the flow to the water year in quation
    flow <- filter(FlowYear, water_year == WYs[i])
    
    #Then we need to find the October 1st start date of the water year of interest
    #WY_start <- which(month(date_data ) == 10 & day(date_data) == 1 & year(date_data) == (Water_Years[i]-1))
    
    
    if(sum(is.na(flow$flow) | is.nan(flow$flow[1:length(flow$flow)]))>= 100 || sum(flow$flow[flow$flow==0 & !is.na(flow$flow)])>= 365 | length(flow$date) < 358){
      Mean_Ann_Flow[i] <- NA
      
    }
    
    #Now that the data has past the checks then we need to replace the NA data
    flow$flow <- replace_na(flow$flow)
    
    
    #Calculate the mean annual flow for that water year and put it in the output vector
    Mean_Ann_Flow[i] <- mean(flow$flow)
    
    
    
    #Assign NA values to everything
    count_0[i] <- sum(flow$flow==0)
    
  }
  
  #Rank the water year mean annual flows from low to high
  WY_Rank_WtoD <- rank(-Mean_Ann_Flow)
  
  #calculate the total number of water years we have to rank
  num_years <- length(WYs)
  
  #the top tertile of years are wet years
  cutoff_1 <- round(num_years * (1/3))  # Upper cutoff for wet years
  
  #the middel tertile of years are wet years
  cutoff_2 <- round(num_years * (2/3))  # Upper cutoff for mod years
  
  #Set the water year catagory based on the tertiles above
  WY_Cat <- cut(WY_Rank_WtoD, breaks = c(0, cutoff_1, cutoff_2, Inf), labels = c("wet year", "mod year", "dry year"))
  
  Ann_metric_out <- list("Years" = WYs,"Mean_Ann_Flow"=Mean_Ann_Flow,"WY_Cat"=WY_Cat)
  
  #Return the mean annual flow and water year catagory
  return(Ann_metric_out)
}


#This Function takes flow data and determines the total annual flow
Annual_Flow_Volume <- function(FlowYear) {
  #Set up the results 
  Ann_Vol <- c()
  
  #Determine how many water years there are 
  Water_Years <- unique(FlowYear$water_year)
  
  #Loop through all of the water years
  
  for (i in 1:length(Water_Years)) {
    #Make a data frame of the flow year to check to see if it qualifies to run
    flow <- filter(FlowYear, water_year == Water_Years[i])
    
    #check to see if there are too many "0" values or NA/NaN values in the water year
    if (sum(is.na(flow$flow) | is.nan(flow$flow))>= 100 | length(flow$date) < 358) {
      
      #If it does then set all the metrics to NA
      FA_Tim[i] <- NA
      Wet_Tim[i] <- NA
      FA_Mag[i] <- NA
      FA_Dur[i] <- NA
      FA_Dif_ratio[i] <- NA
      FA_Dif_num[i] <- NA
      next
    }
    #Conversion from cfs to kacft/year
    cfs_to_kafpyear <- (724/1000)
    
    #Calculate the total volume for the water year
    Ann_vol_cfs <- sum(flow$flow)
    
    #conver to 
    Ann_vol_kacftpyear <- Ann_vol_cfs*cfs_to_kafpyear
    
  }
  
}