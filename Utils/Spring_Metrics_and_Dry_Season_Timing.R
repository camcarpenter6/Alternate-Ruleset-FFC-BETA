#Define a function that calculates the rate of change for a time series
library(segmented)
library(strucchange)


#Define a function that calculates the rate of change for a time series
rate_of_change <- function(flow) {
  #get the changes in flow 
  diffs <- diff(flow)
  #now divide the change by the flow
  roc <- diffs/flow[-length(flow)]
  #in the case where the flow is 0 then the roc value will be a NaN value and need to be replaced with 0
  roc[is.nan(roc)] <- 0 
  roc <- c(NA,roc)
  
}

#Define a function that finds the dry season timing given the flow after the top of the spring recession until the end of the water year
Altered_Summer_Dry_Season_Tim_Varied <- function(flow, day_thresh = 5, roc_thresh = 0.02) {
  median <- median(flow)
  roc <- rate_of_change(flow)
  dif <- c(NA,diff(flow))
  # Initialize variables
  n_consec <- 0
  n_neg <-0
  idx_consec <- NULL
  idx_start <- NULL
  DS_Tim <- NULL
  #cat("\n",day_thresh, "  ", roc_thresh)
  # Loop through the time series
  for (i in seq_along(flow)) {
    #If the program has not found a qualifying timing in entire vector then assign a NA to the dry season timing
    #Since there isn't a rate of change value for index 1 set is at the first possible start and continue the loop
    if(i==1){
      idx_start <- i
      next
    }
    # Check if this is the first value in a potential consecutive sequence
    if (is.null(idx_start)) {
      idx_start <- i
      
    }
    # Check if the current value is within 2% of the previous value
    if ((abs(roc[i]) <= roc_thresh| flow[i] < 2 | (flow[i]<20 & (!is.na(dif[i]&dif[i]<=1)))) & !is.null(idx_start)) {
      n_consec <- n_consec + 1
      if (roc[i]<0){
        n_neg <- n_neg+1
      }
      # Check if this is the fourth consecutive value that meets the criteria
      if (n_consec == day_thresh) {
        #cat("\n \n Summer should be done")
        idx_consec <- seq(idx_start, i)

        break
      }
    }
    # Reset the consecutive counter if the current value is not within the rate of change threshold of the previous value
    else {
      n_consec <- 0
      n_neg <-0
      idx_start <- NULL
    }
  }
  #This is just for testing on FNF date but is there is a year where there aren't 5 days of 2% change or less than try less than 5%
  if (is.null(idx_consec) == TRUE){
    #cat("\n no summer found \n")
    #If the change threshold was below 5% then try increasing it by 1%
    if(roc_thresh < 0.05){

      roc_thresh = roc_thresh+.01
      #cat(roc_thresh,"\n")
      #rerun this code to look for a new dry season timing
      DS_Tim <- Altered_Summer_Dry_Season_Tim_Varied(flow = flow, day_thresh = day_thresh, roc_thresh = roc_thresh )
    }
    #else if the change threshold is at 5% then try decreasing the number of days
    else if(roc_thresh > 0.045 & day_thresh >= 3){
      #if the threshold is more than 3 then try decreasing the day threshold by one
      day_thresh = day_thresh - 1
      #cat(day_thresh,"\n")
      #Rerun the code with the new thresholds
      DS_Tim <- Altered_Summer_Dry_Season_Tim_Varied(flow = flow, day_thresh = day_thresh, roc_thresh = roc_thresh)
      
    }
    
  }
  #cat("dry season timing: ", DS_Tim)
  #Since we call the funcion in itself above this if statement is only needed to find 
  if(is.null(DS_Tim)){
    #cat("Dry season timing set")
    if (n_neg > 3){
      DS_Tim <- (idx_consec[length(idx_consec)]+1)
    }
    else if (n_neg <=3){
      DS_Tim <- (idx_consec[1]+1)
    }
    #cat("dry season timing: ", DS_Tim)
  }
  
  #cat("\n DS tim: ",DS_Tim, "\n Should go out of the loop \n")
  return(DS_Tim)
}

#Define the function that calculates the spring metrics and Dry season start timing
Altered_Spring_Recession <- function(FlowYear) {
  cat("\n Calculating the Spring and Summer metrics \n")
  
  #Setup the output vectors
  SP_Mag<- c()
  SP_Tim_test <- c()
  SP_Tim<- c()
  SP_ROC <- c()
  SP_ROC_Max <- c()
  SP_Dur <- c()
  DS_Tim <- c()
  
  #Determine how many water years there are 
  Water_Years <- unique(FlowYear$water_year)
  
  #Loop through all of the water years
  
  for (i in 1:length(Water_Years)) {
    #cat("\n \n Water Year: ", Water_Years[i])
    #Filter the flow data to the individual water year
    flow <- filter(FlowYear, FlowYear$water_year== Water_Years[i])
    WY_median <- median(flow$flow)
    #Skip the year if there are more than 100 NA flow data points
    if ((sum(is.na(flow$flow)) +  sum(is.nan(flow$flow))) > 100 | length(flow$date) < 358) {
      #Assign Null value if there are more than 100 NA flow data points
      #cat("Broke too many NA values")
      SP_Tim[i] <- NA
      SP_Mag[i] <- NA
      SP_ROC[i] <- NA
      SP_Dur[i] <- NA
      SP_ROC_Max[i] <- NA
      DS_Tim[i] <- NA
      next
    } 
    #Check to see if there are more than the allowable number of 0s in the vector
    else if (sum(flow$flow[flow$flow==0 & !is.na(flow$flow)])>= 365){
      #cat("Too many 0's ")
      #Assign NA values to everything
      SP_Tim[i] <- NA
      SP_Mag[i] <- NA
      SP_ROC[i] <- NA
      SP_Dur[i] <- NA
      SP_ROC_Max[i] <- NA
      DS_Tim[i] <- NA
      #Then move to the next year
      next
      
    }
    #Check to see if this is a flat lined year
    else if(all(!is.na(flow$flow) & !is.nan(flow$flow) & flow$flow == flow$flow[!is.na(flow$flow)][1])){
      #cat("Flat lined year")
      #Assign NA values to everything
      SP_Tim[i] <- NA
      SP_Mag[i] <- NA
      SP_ROC[i] <- NA
      SP_Dur[i] <- NA
      SP_ROC_Max[i] <- NA
      DS_Tim[i] <- NA
      #Then move to the next year
      next
    }
    else {
      #Now that the data has past the checks then we need to replace the NA data
      flow$flow <- replace_na(flow$flow)
      
      #calculate the 50th and 9th percentile for the flows
      quants <- quantile(flow$flow, probs = c(0.5, 0.9),na.rm = FALSE, names = FALSE)
      #Calculate the peaks that occur throughout the year
      peaks <- as.data.frame(findpeaks(flow$flow),threshold = min((0.15*WY_median),15))
      
      #We also want to consider peaks that are flat the limit was set as 3 "flat" points for these
      peaks_2 <- as.data.frame(findpeaks(flow$flow, peakpat = "[+]{1,}[0]{1,3}[-]{1,}"),threshold = min((0.15*WY_median),15))
      
      #combine the two data sets of peaks
      peaks_all <- bind_rows(peaks,peaks_2)
      
      #cat("broke after peaks")
      #Check to make sure there is data in the peaks 
      if (length(peaks_all)>1 ){
        #If there is data in the data frame then makes sure it is more than just the titles
        if (length(peaks_all[,1]) > 0){
          #Sort all the peaks by the date of the peak
          peaks_all <- peaks_all[order(peaks_all$V2,decreasing = FALSE),]
          

          #Filter out peaks that are not above the 90th percentile and peaks that are in September
          peaks_90 <- dplyr::filter(peaks_all, peaks_all$V1 > quants[2])
          peaks_90 <- dplyr::filter(peaks_90, peaks_90$V2 <345)

          
        }
        else {
          peaks_90 <- NULL
        }
        
      }
      else {
        peaks_90 <- NULL
      }
      
      
      #cat("broke after making peaks_90")
      #Check to make sure that there are qualified peaks 
      if (is.null(peaks_90) == FALSE & length(peaks_90[,1]) >0) {
        
        
        #Assign the last peak of the year as the first potential spring timing
        springindex_PH1 <- tail(peaks_90[,2],1)
        
        #Check to see if this peak is also the fall pulse
        if (springindex_PH1 <= 75 & length(peaks_90[,2]) <2){
          #If it is then it doesn't not count as the spring as well
          #springindex_PH1 <- NULL
        }
        if(Water_Years[i] == 2013){
          #cat("\n Peaks Timing: ", peaks_90[,1] , "\n ")
        }
      }
      
      #if there are no qualifying peaks set this placeholder to null value
      else {
        springindex_PH1 = NULL
      }
      #cat("broke after making PH1 NULL")
      #Find the index of flows at or above the 90th percentile
      highflows <- which(flow$flow >= quants[2])
      
      #Assign the last index above 90th percentile flow as the second potential spring recession index 
      springindex_PH2 <- max(highflows)
      #cat("broke after making PH2")
      #If the first placeholder is not valid then use the second place holder vlue
      if ( is.null(springindex_PH1) ) { #(springindex_PH1+30)<springindex_PH2 || ###OTHER OPTION
        #Set the index of the spring timing to the second place holder
        springindex <- springindex_PH2
      }
      else {
        #Otherwise set the spring index to the first placement
        springindex <- springindex_PH1
      }
      #cat("\n PH1:" , springindex_PH1, " PH 2: ", springindex_PH2)
      #Set the spring timing to index identified 
      SP_Tim[i] <- springindex
      #SP_Tim_test[i] <- append(as.Date(flow$date[springindex], "%m/%d/%Y"))
      
      #Set the spring magnitude to the flow on the index of the timing
      SP_Mag[i] <- as.numeric(flow$flow[springindex])
    }
    
    #make a new data frame with just the flows after the top of the spring recession
    flow_post_SP <- flow %>% slice(springindex:length(flow))
    if(Water_Years[i] == 2013){
      test_post_flow <- flow_post_SP
    }
    #Calculate the rate of change for the rest of the year after the top of the spring recession
    roc <- rate_of_change(flow_post_SP$flow)
    
    #Set a min flow threshold for the dry season to start based on the spring and min dry season baseflow
    #This is based on the threshold in the original calculator
    min_summer_flow_percent <- 0.125
    WY_max_flow <- max(flow$flow,na.rm = TRUE)
    post_SP_min_flow <- min(flow_post_SP$flow, na.rm = TRUE) 
    Min_DS_Threshold <- post_SP_min_flow + (WY_max_flow-post_SP_min_flow)*min_summer_flow_percent
    #cat("\n thresh DS: ", Min_DS_Threshold, "\n")  
    #cat("\n",SP_Tim[i],"\n" )
    #calculate the dry season start timing by subtracting the length of the water year by the time remaining 
    #after the spring recession peak and then add the timing of the start of the dry season after the spring peak

    PH_DS_Tim <- as.numeric(Altered_Summer_Dry_Season_Tim_Varied(flow_post_SP$flow,flow_thresh = Min_DS_Threshold)) #varying qualification Code
    
    
    #cat("\n PH Dry",PH_DS_Tim,"\n is the place holder null", is.null(PH_DS_Tim))
    
    if (is.null(PH_DS_Tim) | length(PH_DS_Tim)<1){
      #cat("Error No Dry Season Start identified")
      DS_Tim[i] <- -9999
      SP_Dur[i] <- -9999
      SP_ROC[i] <- -9999
      SP_ROC_Max[i] <- -9999
      
      #go to the next water year
      next
    }
    
    if (i == length(Water_Years) & length(as.numeric(Altered_Summer_Dry_Season_Tim_Varied(flow_post_SP$flow,flow_thresh = Min_DS_Threshold)))<1){
      PH_DS_Tim <- length(flow_post_SP$flow)
    }
    #It is possible that if all the flows are below 2 cfs then the dry season will be set to 0
    else if (!is.null(PH_DS_Tim) & PH_DS_Tim == 0){
      PH_DS_Tim <- 1
    }
    
    #cat("\n Dry season place holder:", PH_DS_Tim)
    #DS_Tim[i] <- length(flow$flow)-length(flow_post_SP$flow)+PH_DS_Tim ORIGINAL CODE
    DS_Tim[i] <- PH_DS_Tim+SP_Tim[i]
    #The spring duration is the time between spring timing and dry season start timing
    SP_Dur[i] <- DS_Tim[i]-SP_Tim[i]
    
    
    #Make an array of the rate of change values after the spring peak until the
    #Start of the dry season. This needs to start at the second value since the
    #first value is the NA. This is because there is nothing to compare the first value to.
    SP_recs_temp <- roc[2:(SP_Dur[i]+1)]
    #cat("\n SP recs ", SP_recs_temp)
    #The spring rate of change metric is the median of the of all the negative roc values
    SP_ROC[i] <- abs(median(SP_recs_temp[SP_recs_temp<0]))
    
    #The spring maximum rate of change metric is the largest  negative roc value
    SP_ROC_Max[i] <- max(abs(SP_recs_temp[SP_recs_temp<0]))
  }
  
  #Put all the metrics into a list
  SP_Metrics_and_Dry_Season_Tim <- list( "SP_Tim"= SP_Tim,"SP_Mag"= SP_Mag, "SP_ROC"= SP_ROC, "SP_Dur"= SP_Dur, "SP_ROC_Max" = SP_ROC_Max, "DS_Tim"= DS_Tim)
  
  #Return all of the metrics
  return(SP_Metrics_and_Dry_Season_Tim)
}

