#This function calculates the Dry and Wet Seasons magnitudes
#The inputs for this function are the timings of the spring recession,
#the dry season start timing, and the start of the wet season timing
Wet_Dry_Season_Non_Tim_Metrics <- function(FlowYear,SP_Tim,DS_Tim, Wet_Tim){
  cat("\n Calculating the wet and dry season magnitudes \n")
  #Get all the of the water years in question
  WYs <- unique(FlowYear$water_year)
  
  #Set up the result matrices 
  Wet_BFL_Mag_10 <- c()
  Wet_BFL_Mag_50 <- c()
  DS_Mag_50 <- c()
  DS_Mag_90 <- c()
  DS_Dur_WS <- c()
  Wet_BFL_Dur <- c()
  
  #And a place holder flow flat years 
  flat_year <- c()
  
  #cycle through the water years to calculate the metrics
  for (i in 1:length(WYs)) {
    #cat("\n Water Year", WYs[i],"\n")
    
    #filter the flow to the water year in question
    Filteryear1 <- dplyr::filter(FlowYear, water_year == WYs[i])
    
    #Check to see if there were too many 0's or NA values in this water year
    
    #check to see if there are too many "0" values or NA/NaN values in the water year
    if (sum(is.na(Filteryear1$flow) | is.nan(Filteryear1$flow))>= 100 || sum(Filteryear1$flow[Filteryear1$flow==0 & !is.na(Filteryear1$flow)])>= 365 || length(Filteryear1$flow)<358) {
      
      #If it does then set all the metrics to NA
      Wet_BFL_Mag_10[i] <- NA
      Wet_BFL_Mag_50[i] <- NA
      Wet_BFL_Dur[i] <- NA
      DS_Mag_50[i] <- NA
      DS_Mag_90[i] <- NA
      DS_Dur_WS[i] <- NA
      
      next
    }
    else if(all(!is.na(Filteryear1$flow) & !is.nan(Filteryear1$flow) & Filteryear1$flow == Filteryear1$flow[!is.na(Filteryear1$flow)][1])){
      #cat("Flat lined year")
      #Assign NA values to everything
      Wet_BFL_Mag_10[i] <- NA
      Wet_BFL_Mag_50[i] <- NA
      Wet_BFL_Dur[i] <- NA
      DS_Mag_50[i] <- NA
      DS_Mag_90[i] <- NA
      DS_Dur_WS[i] <- NA
      #Then move to the next year
      flat_year[i]<- WYs[i]
      
      next
    }
    
    #Replace the NA in the data
    Filteryear1$flow <- replace_na(Filteryear1$flow)
    
    if(is.na(Wet_Tim[i]) & i == length(WYs)){
      #If it is the last year and there isn't wet season timing data then make the magnitude and duration NA
      Wet_BFL_Mag_10[i] <- NA
      Wet_BFL_Mag_50[i] <- NA
      Wet_BFL_Dur[i] <- NA
    }
    
    else if(is.na(Wet_Tim[i]) | is.nan(Wet_Tim[i] & is.na(Wet_Tim[i+1]) )) {
      next
    }
    else if( !is.na(Wet_Tim[i]) ){
      WS <- Filteryear1$flow[Wet_Tim[i]:SP_Tim[i]]
      WS_mag <- quantile(WS,c(.1,0.5),na.rm = TRUE)
      Wet_BFL_Mag_10[i] <- WS_mag[1]
      Wet_BFL_Mag_50[i] <- WS_mag[2]
      
      if(DS_Tim[i] > 0){
        Wet_BFL_Dur[i] <- SP_Tim[i]-Wet_Tim[i]
      }
      else if(DS_Tim[i] < 0){
        Wet_BFL_Dur[i] <- SP_Tim[i]-Wet_Tim[i]
      }
      
      #cat("\n wet season",WS_mag)
    }
    if((is.nan(Wet_Tim[i+1])|is.na(Wet_Tim[i+1])) & (!is.nan(Wet_Tim[i+2])|!is.na(Wet_Tim[i+2])) & i != length(WYs) & DS_Tim[i] >= 0){
      DS <- Filteryear1$flow[DS_Tim[i]:length(Filteryear1$flow)]
      #Set the 50th and 90th percentile flows for the dry season and assign it to the magnitude
      DS_mag <- quantile(DS,c(.5,0.9), na.rm = TRUE)
      DS_Mag_50[i] <- DS_mag[1]
      DS_Mag_90[i] <- DS_mag[2]
      
      #Since there is not the next year 
      DS_Dur_WS[i] <- (Wet_Tim[i+2]+(365*2))-DS_Tim[i]
      next
    }
    else if(is.nan(Wet_Tim[i+1])|is.na(Wet_Tim[i+1]) & i == length(WYs)){
      
      #check to see if the dry season timing exists
      #if it does assign then calculate the dry season flow
      if(is.na(DS_Tim[i]) != TRUE & is.nan(DS_Tim[i]) != TRUE & DS_Tim[i]>0){
        DS <- Filteryear1$flow[DS_Tim[i]:length(Filteryear1$flow)]
      }
      #Other wise make it 4 days after the spring similar to the original calculator
      else if(!is.na(SP_Tim[i])){
        DS <- Filteryear1$flow[(SP_Tim[i]+4):length(Filteryear1$flow)]
      }
      else if (is.na(DS_Tim[i]) | DS_Tim[i] < 0 ){
        
        #Set the non timing and duration metrics to the error code
        DS_Mag_50[i] <- -9999
        DS_Mag_90[i] <- -9999
        DS_Dur_WS[i] <- -9999
        
        next
        
      }
      else{
        DS <- Filteryear1$flow[as.integer(mean(SP_Tim,na.rm = TRUE)):length(Filteryear1$flow)]
      }
      
      #Get the 50th and 90th percentile flows for the dry season and assign it to the magnitude
      DS_mag <- quantile(DS,c(.5,0.9), na.rm = TRUE)
      DS_Mag_50[i] <- DS_mag[1]
      DS_Mag_90[i] <- DS_mag[2]
      
      #Since there is not the next year 
      DS_Dur_WS[i] <- NA
      
      #cat("\n dry season",DS_mag)
      next
    }
    
    Filteryear2 <- dplyr::filter(FlowYear, water_year == WYs[i] | water_year == WYs[i]+1)
    
    if(!is.na(DS_Tim[i]) & DS_Tim[i] > 0 ){
      DS <- Filteryear2$flow[DS_Tim[i]:(Wet_Tim[i+1]+365)]
    }
    else if (is.na(DS_Tim[i]) | DS_Tim[i] < 0 ){
      
      #Set the non timing and duration metrics to the error code
      DS_Mag_50[i] <- -9999
      DS_Mag_90[i] <- -9999
      DS_Dur_WS[i] <- -9999
      
      next
    }
    
    #Replace the NA in the data
    DS <- replace_na(DS)
    
    #return(Filteryear2,DS)
    DS_mag <- quantile(DS,c(.5,0.9),na.rm = TRUE)
    DS_Mag_50[i] <- DS_mag[1]
    DS_Mag_90[i] <- DS_mag[2]
    
    if(DS_Tim[i] > 0){
      DS_Dur_WS[i] <- (Wet_Tim[i+1]+365)-DS_Tim[i]
    }
    if(DS_Tim[i] < 0){
      DS_Dur_WS[i] <- (Wet_Tim[i+1]+365)-mean(DS_Tim[DS_Tim>0],na.rm = TRUE)
    }
    #cat("\n dry season",DS_mag, "\n", DS_Dur_WS[i])
    
  }
  #Put all the metrics in a list
  Output_Metrics <- list("Wet_BFL_Mag_10"=Wet_BFL_Mag_10,"Wet_BFL_Mag_50"=Wet_BFL_Mag_50,"Wet_BFL_Dur"=Wet_BFL_Dur,"DS_Mag_50"=DS_Mag_50,"DS_Mag_90"=DS_Mag_90,"DS_Dur_WS"=DS_Dur_WS) 
  
  
  return(Output_Metrics)
}
