
library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);
library(ggplot2);library(dplyr); library(pracma); library(lubridate)


#Define a function that finds the Fall Metrics and Wet Season timing
Altered_Fall_Wet_Timing <- function(FlowYear, DS_Tim) {
  cat("\n Calculating the Fall and Wet Season Metrics \n")
  
  #Set up Output Vectors
  FA_Tim <- c()
  Wet_Tim <- c()
  FA_Mag <- c()
  FA_Dur <- c()
  FA_Dif_ratio <- c()
  FA_Dif_num <- c()
  
  
  #Set up place holder vectors
  Temp_FA_Tim <- c()
  Temp_Wet_Tim <- c()
  Temp_DS_flow <- c()
  
  #Determine how many water years there are 
  Water_Years <- unique(FlowYear$water_year)
  
  for (i in 1:length(Water_Years)) {
    
    cat("\n New Water Year: ",Water_Years[i], "\n")

    #If this is the first year then skip since there is not a previous dry season to use
    if (i == 1){
      #If it is the first year then skip since there no former dry season baseflow to compare to so it skips
      FA_Tim[i] <- NA
      Wet_Tim[i] <- NA
      FA_Mag[i] <- NA
      #FA_Dif_ratio[i] <- NA
      FA_Dif_num[i] <- NA
      #Temp_DS_flow_1 <- flow$flow[DS_Tim[i]:length(flow$flow)]
      next
    } 
    #Make a data frame of the flow year to check to see if it qualifies to run
    check_flow <- dplyr::filter(FlowYear, water_year== Water_Years[i])
    
    #check to see if there are too many "0" values or NA/NaN values in the water year
    if (sum(is.na(check_flow$flow) | is.nan(check_flow$flow))>= 100 | sum(check_flow$flow[check_flow$flow==0 & !is.na(check_flow$flow)])>= 364 | length(check_flow$date) < 358 | all(check_flow$flow<1)) {
      
      #If it does then set all the metrics to NA
      FA_Tim[i] <- NA
      Wet_Tim[i] <- NA
      FA_Mag[i] <- NA
      FA_Dur[i] <- NA
      #FA_Dif_ratio[i] <- NA
      FA_Dif_num[i] <- NA
      next
    }    #Check to see if this is a flat lined year
    else if(all(!is.na(check_flow$flow) & !is.nan(check_flow$flow) & check_flow$flow == check_flow$flow[!is.na(check_flow$flow)][1])){
      #cat("Flat lined year")
      #Assign NA values to everything
      FA_Tim[i] <- NA
      Wet_Tim[i] <- NA
      FA_Mag[i] <- NA
      FA_Dur[i] <- NA
      #FA_Dif_ratio[i] <- NA
      FA_Dif_num[i] <- NA
      #Then move to the next year
      next
    }
    #If there is enough data then calculate the fall pulse and wet season timing
    
    
    #Filters flow to the water year of interest and the previous water year
    flow <- dplyr::filter(FlowYear, water_year== Water_Years[i] | water_year== Water_Years[i-1])
    
    #first we need to find the point where our water year of interest starts
    #To do this we will convert the date field of the filtered data into an actual date
    date_data <- as.Date(flow$date, tryFormats = c("%m/%d/%Y"))
    
    #Then we need to find the October 1st start date of the water year of interest
    WY_start <- which(month(date_data ) == 10 & day(date_data) == 1 & year(date_data) == (Water_Years[i]-1))
    

    #cat("\n Length of all flow: ", length(flow$flow), "\n Date of 365 timestep:", as.Date(flow$date[WY_start]) )

    #sets up an initial dry season baseflow estimates
    #Temp_DS_Mag[i] <- median(flow$flow[DS_Tim[i]:length(flow$flow)])
    #Calculates the current water years median 
    WY_median <- median(flow$flow)
    FA_Check <- FALSE #Set the check whether the fall exists to false at the start of each loop
    
    
    #Now that the data has past the checks then we need to replace the NA data
    flow$flow <- replace_na(flow$flow)
    
    median_flow <- median(flow$flow)
    

    #Start by finding peaks that occur from October 1st to December 15th, anything that is less than 7 days counts
    FA_peaks <- findpeaks(flow$flow[WY_start:(WY_start+75)],peakpat = "[+]{1,}[0]{,40}[-]{1,}", threshold = 10) 
    
    #cat("\n WY start +75 = ", as.character( flow$date[(WY_start+75)]),"\n")
    cat(FA_peaks)
    #Check to make sure the is data in the output from the peaks analysis
    if (length(FA_peaks)>0){
      #cat("found a pulse")
      #Loop through the peaks to see if there is a qualifying peak
      for(j in 1:length(FA_peaks[,1])) {
        #cat("in fall loop")

        #See if the last peak met the criteria if so break the loop
        if (FA_Check == TRUE) {
          break
        } 
        
        ##Check to see if the peaks meet 1.5 times the baseline threshold 
        ##First make a vector of the flow values from the previous dry season until the potential peak
        
        #To do this we need to make sure there was a dry season timing next year
        if(is.na(DS_Tim[i-1]) != TRUE & DS_Tim[i-1] > 0) {
          #Then 
          
          Temp_DS_flow <- flow$flow[DS_Tim[i-1]:(FA_peaks[j,2]+WY_start)]
          Temp_DS_Mag <- median(Temp_DS_flow)
        }
        else if (is.na(DS_Tim[i-1]) == TRUE | DS_Tim[i-1] < 0){
          
          Temp_DS_flow <- flow$flow[1:(FA_peaks[j,2]+WY_start)]
          Temp_DS_Mag <- median(Temp_DS_flow)
          
        }
        
        cat("\n Temp_DS: ", Temp_DS_Mag,"\n")
        #Check to see if the peak is larger than the estimated dry season baseflow
        if (FA_peaks[j,1] > 1.5*Temp_DS_Mag & FA_peaks[j,1] >= 1) {

          #Store the identified timing for the fall pulse
          FA_Tim_Temp <- FA_peaks[j,2]
          FA_Dur_Temp<- FA_peaks[j,4] - FA_peaks[j,3]
          FA_Mag_Temp <- FA_peaks[j,1]
          
          #Look for the start of the wet season to check the if the fall peak actually qualifies and 

          #First get the flow after the fall pulse occured
          post_fall_flow <- flow$flow[(FA_Tim_Temp+FA_Dur_Temp+WY_start):length(flow$flow)]
          #WS_peaks <- findpeaks(flow$flow[(FA_Tim_Temp+WY_start):length(flow$flow)],peakpat = "[+]{1,}[0]{,5}[-]{1,}", threshold = min(10,WY_median*.1))
          #cat("Wet season peaks: ", WS_peaks)
          
          #Then will will find the rolling median of flows post fall
          median_array <- sapply(1:length(post_fall_flow), function(i) median(post_fall_flow[1:i]))
          
          #Now find the index of the first flow that is 1.5 times the median
          Temp_Wet_Tim <- which(post_fall_flow > 1.5 * median_array)[1]
          
          #To get the dry season median we need to make sure there was a dry season timing next year
          if(is.na(DS_Tim[i-1]) != TRUE & DS_Tim[i-1] > 0 & is.na(Temp_Wet_Tim) != TRUE) {
            
            cat("DS?" , is.na(DS_Tim[i-1]), "\n", "FA_Tim_Temp?" , is.na(FA_Tim_Temp),"\n","FA_Dur_Temp?" , is.na(FA_Dur_Temp),"\n","Temp_Wet_Tim?" , is.na(Temp_Wet_Tim),"\n","WY_start?" , is.na(WY_start),"\n")

            #Calculate the potential dry season 50th percentile flow
            Temp_DS_Mag <- median(flow$flow[DS_Tim[i-1]:(FA_Tim_Temp+FA_Dur_Temp+Temp_Wet_Tim+WY_start)])
          }
          #If there wasn't a dry season timing then we will look at the entire flow array
          else if (is.na(DS_Tim[i-1]) == TRUE | DS_Tim[i-1] < 0 & is.na(Temp_Wet_Tim) != TRUE){
            #Calculate the potential dry season 50th percentile flow
            Temp_DS_Mag <- median(flow$flow[1:(FA_Tim_Temp+FA_Dur_Temp+Temp_Wet_Tim+WY_start)])
          }
          
          #cat("\n Wet start date mag: " , post_fall_flow[Temp_Wet_Tim], " Potential Fall Pulse: ", FA_Mag_Temp," thershold : ", (1.5*Temp_DS_Mag),"\n")
          #Check to see if the fall pulse is still 1.5 time the dry season 50th percentile flow 
          if (FA_Mag_Temp > 1.5*Temp_DS_Mag) {
            
            FA_Tim[i] <- FA_peaks[j,2]
            #Assign the peak value as the magnitude
            FA_Mag[i] <- FA_peaks[j,1]
            #Calculate the duration of the flush from the start of the rising limb to top of the pluse
            FA_Dur[i] <- FA_peaks[j,2] - FA_peaks[j,3]
            #Calculate the difference metric 
            #FA_Dif_ratio[i] <- FA_peaks[j,1]/Temp_DS_Mag
            FA_Dif_num[i] <- FA_peaks[j,1]-Temp_DS_Mag
            #Set the wet season start timing 
            Wet_Tim[i] <- FA_Tim[i]+FA_Dur_Temp+Temp_Wet_Tim-2 #Need to subtract two from the temporary time since fall timing and duration double count a day as does the duration and temp timing
            
            #cat("\n Fall pluse timing: " ,FA_Tim[i],"\n Wet timing with fall pulse: ",Wet_Tim[i],"\n" )
            
            #cat("\n Fall pluse timing: " ,flow$date[WY_start+ FA_Tim[i]], "\n")
            
            FA_Check <- TRUE
            break
          }
          
          #If none of the post fall pulse peaks meet the criteria it is likely a peak prior to a hat senario and we want to find the last day before the 90th percentile flow

          #post_fall_flow <- flow[(FA_Tim_Temp+FA_Dur_Temp+WY_start):length(flow$flow),]
          #threshold_90 <- quantile(post_fall_flow$flow,0.9)
          #if(all(WS_peaks[,1]<1.5*Temp_DS_Mag) | !is.null(WS_peaks)){
            
            #cat("fall but no peaks")


            #Find subset of data above the 90th percentile
            #post_fall_90th <- subset(post_fall_flow, flow >= threshold_90)
            
            #find the first day that is at or above the 90th percentile of flow
            #index_90 <- which.min(post_fall_90th$flow)
            
            #start_date <- post_fall_90th$date[index_90]
            #start_date <- post_fall_90th$date[1]
            
            #then go one day back to capture the rising limb
            #Wet_start_index <- which(post_fall_flow$date == start_date)
            
            #To get the dry season median we need to make sure there was a dry season timing next year
            #if(is.na(DS_Tim[i-1]) != TRUE & DS_Tim[i-1] > 0) {
              
              #Calculate the potential dry season 50th percentile flow

              #Temp_DS_Mag <- median(flow$flow[DS_Tim[i-1]:(FA_Tim_Temp+Wet_start_index+WY_start)])
            #}
            #else if (is.na(DS_Tim[i-1]) == TRUE | DS_Tim[i-1] < 0){
              #Calculate the potential dry season 50th percentile flow
              #Temp_DS_Mag <- median(flow$flow[1:(FA_Tim_Temp+Wet_start_index+WY_start)])
            #}

            
            
            ##Check to see if the fall pulse is still 1.5 time the dry season 50th percentile flow 
            #if (FA_Mag_Temp > 1.5*Temp_DS_Mag) {
              
              #FA_Tim[i] <- FA_peaks[j,2]
              #Assign the peak value as the magnitude
              #FA_Mag[i] <- FA_peaks[j,1]
              #Calculate the duration of the flush from the start of the rising limb to the peak
              #FA_Dur[i] <- FA_peaks[j,4] - FA_peaks[j,3]
              #Calculate the difference metric 
              #FA_Dif_ratio[i] <- FA_peaks[j,1]/Temp_DS_Mag
              #FA_Dif_num[i] <- FA_peaks[j,1]-Temp_DS_Mag
              #Set the wet timing based on the identified 
              #Wet_Tim[i] <- Wet_start_index+FA_Tim_Temp
              #cat("\n Wet Season Date",flow$date[(365+Wet_Tim[i])],"\n")
              
              #FA_Check <- TRUE
            #}
          #}
          else {
            
            next
          }
          
        }
      }
      
      
      
      if (FA_Check == FALSE) { 
        
        #If there was not then set all Fall metrics besides the Difference metric to NA
        FA_Tim[i] <- NA
        FA_Mag[i] <- NA
        FA_Dur[i] <- NA
        #FA_Dif_ratio[i] <- max(FA_peaks[,1])/Temp_DS_Mag
        FA_Dif_num[i] <- max(FA_peaks[,1])-Temp_DS_Mag
      }
      
      
      
    }
    
    
    #If there are no pulses detected in the required time period then apply NA values to all Fall metrics
    else {
      FA_Tim[i] <- NA
      FA_Mag[i] <- NA
      FA_Dur[i] <- NA
      #FA_Dif_ratio[i] <- 0
      FA_Dif_num[i] <- 0
    }
    
    
    #Now the code will look at if no fall pulses occurred
    if (FA_Check == FALSE) { 
      
      cat("\n just wet ")
      
      #If there was no fall pulse then find the first pulse after that is 1.5 Dry season baseflow after the fall pulse period
      #first get the flow data from after the fall pulse window
      Wet_Peaks_flow <- flow[(WY_start+75):nrow(flow),]
      
      #Now find the peaks after in the post fall window 
      WS_peaks <- findpeaks(Wet_Peaks_flow$flow, peakpat = "[+]{1,}[0]{,5}[-]{1,}",threshold = min((0.1*WY_median),10))
      #cat("Wet Season Peaks",WS_peaks)
      #cat("The length of WS peak",length(WS_peaks))
      #Check to make sure the is data in the output from the peaks analysis
      if (length(WS_peaks)>0){

        #Loop through the peaks to see if there is a qualifying peak
        for(j in 1:length(WS_peaks[,1])) {
          #Check to see if the peaks meet 1.5 times the baseline threshold 
          #First make a vector of the flow values from the previous dry season until the potential peak
          
          #To do this we need to make sure that there was a previous dry season timing
          #To get the dry season median we need to make sure there was a dry season timing next year
          if(is.na(DS_Tim[i-1]) != TRUE & DS_Tim[i-1] > 0) {
            
            #Calculate the potential dry season 50th percentile flow
            Temp_DS_flow <- flow$flow[DS_Tim[i-1]:(WS_peaks[j,2]+WY_start)]
            Temp_DS_Mag <- median(Temp_DS_flow)
          }
          else if (is.na(DS_Tim[i-1]) == TRUE | DS_Tim[i-1] < 0){
            #Calculate the potential dry season 50th percentile flow
            Temp_DS_flow <- flow$flow[1:(WS_peaks[j,2]+WY_start)]
            Temp_DS_Mag <- median(Temp_DS_flow)
          }
          
          
          #Check to see if the peak is larger than the estimated dry season baseflow
          if (WS_peaks[j,1] > 1.5*Temp_DS_Mag) {
            
            #Now we know we either have a peak on or peak before a "hat" scenario,
            # so we need to check the timing of the peak
            
            #To do that we are first going to 90th percentile flow for the current flow year
            current_flowyear <- dplyr::filter(flow, water_year == Water_Years[i])
            
            
            threshold_90 <- quantile(current_flowyear$flow,0.9)
            
            #cat("\n peak: ",WS_peaks[j,1], "\n", 1.5*Temp_DS_Mag)
            
            
            #Find subset of data above the 90th percentile or above 1 cfs if the 90th percentile is less than 
            flow_90th <- subset(Wet_Peaks_flow, flow >= max(threshold_90,1))
            
            
            #If all of the flows are less than 1 cfs then just use the 90th percentile flow
            if(length(flow_90th[,1])<1){
              flow_90th <- subset(Wet_Peaks_flow, flow >= threshold_90)
            }
            
            #find the first day that is at or above the 90th percentile of flow and the last date to check
            #index_90 <- which.min(flow_90th$flow)
            #cat("hi")
            #start_date <- Wet_Peaks_flow$date[index_90]
            start_date <- flow_90th$date[1]
            end_date <- flow_90th$date[length(flow_90th$date)]
            #Now get the index of first 90th percentile flow start date in the wet peaks flow
            Wet_start_index_1 <- which(Wet_Peaks_flow$date == start_date)
            index_1_check <- which(Wet_Peaks_flow$date == end_date)
            #Then get the index of the start of the qualified peak 
            Wet_start_index_2 <- WS_peaks[j,3]
            
            #cat("\n index 1: ",Wet_start_index_1,"  index 2: ", Wet_start_index_2)
            
            #Check to see which potential timing 
            if (index_1_check < Wet_start_index_2){
              #if the first index is smaller then set that as the wet season 
              Wet_Tim[i] <- Wet_start_index_1 + 75 
            }
            else if (index_1_check >= Wet_start_index_2) {
              
              #Set the wet season start timing 
              Wet_Tim[i] <- 75+Wet_start_index_2
              
            }
            #cat("\n Wet Season Date",flow$date[(365+Wet_Tim[i])],"\n")
            
            break
            
          }

        }
        
        #IF all peaks are below the 90th flow percentile then we will just choose the day before the 1st day of 90th flow
        #If none of the post fall pulse peaks meet the criteria it is likely a peak prior to a hat scenario
        #and we want to find the last day before the 90th percentile flow of that flow year
        Temp_dry_flow <- dplyr::filter(FlowYear, water_year == Water_Years[i])
        Temp_dry_flow$flow <- replace_na(Temp_dry_flow$flow)
        threshold_90 <- quantile(Temp_dry_flow$flow,0.9)
        if(all(WS_peaks[,1]<1.5*Temp_DS_Mag) | length(WS_peaks)<=0){
          
          #cat("\n no qaulifed peaks \n")
          #Find subset of data above the 90th percentile
          flow_90th <- subset(Temp_dry_flow, flow >= threshold_90)
          
          #find the first day that is at or above the 90th percentile of flow
          #index_90 <- which.min(flow_90th$flow)
          #start_date <- flow_90th$date[index_90]
          start_date <- flow_90th$date[1]
          
          #then go one day back to capture the rising limb
          Wet_start_index <- (which(Temp_dry_flow$date == start_date)-1)
          
          #Set the wet season timing to that date
          Wet_Tim[i] <- Wet_start_index
        }
        
      }
      else{  
        #If there aren't any peaks then it is likely hat scenario
        #and we want to find the last day before the 90th percentile flow of that flow year
        #cat("Went to hat check")
        Temp_dry_flow <- dplyr::filter(FlowYear, water_year == Water_Years[i])
        Temp_dry_flow$flow <- replace_na(Temp_dry_flow$flow)

        #cat(is.na(DS_Tim[i-1]) != TRUE & DS_Tim[i-1] > 0)
        #cat("or",(is.na(DS_Tim[i-1]) == TRUE | DS_Tim[i-1] < 0) )
        if(is.na(DS_Tim[i-1]) != TRUE & DS_Tim[i-1] > 0 ) {

          #Calculate the potential dry season 50th percentile flow
          Temp_DS_flow <- flow$flow[DS_Tim[i-1]:length(flow$flow)]
          Temp_DS_Mag <- median(Temp_DS_flow)
        }

        else if ((is.na(DS_Tim[i-1]) == TRUE | DS_Tim[i-1]) < 0 & length(WS_peaks)>0){
          cat("\n second loop")

          #Calculate the potential dry season 50th percentile flow
          Temp_DS_flow <- flow$flow[1:(WS_peaks[length(WS_peaks[,1]),2]+WY_start)]
          Temp_DS_Mag <- median(Temp_DS_flow)
        }

        else {
          Temp_DS_flow <- flow$flow
          Temp_DS_Mag <- median(Temp_DS_flow)
        }

        threshold_90 <- quantile(Temp_dry_flow$flow,0.9)
        if(all(WS_peaks[,1]<1.5*Temp_DS_Mag) | length(WS_peaks)<=0){
          
          cat("\n hat \n")
          
          #Find subset of data above the 90th percentile
          flow_90th <- subset(Temp_dry_flow, flow >= max(threshold_90 )& flow >0)
          
          #find the first day that is at or above the 90th percentile of flow
          #index_90 <- which.min(flow_90th$flow)
          #start_date <- flow_90th$date[index_90]
          start_date <- flow_90th$date[1]
          
          #then go one day back to capture the rising limb
          Wet_start_index <- (which(Temp_dry_flow$date == start_date)-1)
          
          #Set the wet season timing to that date
          Wet_Tim[i] <- Wet_start_index
        }
        
        
      }
      
    }
    
  }
  
  #Put Everything into a list for the ourput
  Fall_Metrics_and_Wet_tim <- list("FA_Tim"=FA_Tim,"FA_Mag"=FA_Mag, "FA_Dur"=FA_Dur,"FA_Dif_num"=FA_Dif_num,"Wet_Tim"=Wet_Tim)
  
  #Return the calculated metrics
  return(Fall_Metrics_and_Wet_tim)
}
