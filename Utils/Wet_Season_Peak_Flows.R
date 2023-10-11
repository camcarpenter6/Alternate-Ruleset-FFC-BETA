library(dplyr)
library(stats)


# Function to calculate the median of time
median_of_time <- function(lt) {
  n <- length(lt)
  if (n < 1) {
    return(NA)
  } else if (n %% 2 == 1) {
    return(lt[n%%2])
  } else if (n == 2) {
    first_date <- lt[1]
    second_date <- lt[2]
    return((first_date + second_date) / 2)
  } else {
    first_date <- lt[n %% 2 - 1]
    second_date <- lt[n %% 2 + 1]
    return((first_date + second_date) / 2)
  }
}


#This function returns peak flows based on a return interval using the  Log-Pearson Type III analysis
#given annual flow peaks and a requested return interval
Peak_Flow_Analysis <- function(peaks,Average_Return_Interval){
  
  #First rank the provided peaks
  ranked_peaks <- sort(peaks,decreasing = TRUE)
  
  #assign ranks to each flow
  ranks <- seq_along(length(ranked_peaks))
  
  #Take the log of all the peak flows
  log_ranked_peaks <- log(ranked_peaks)
  
  #get the mean of the peaks
  average_peak <- mean(ranked_peaks)
  
  #This gets the mean of the log peaks
  log_average_peak <- mean(log_ranked_peaks)
  
  #Now calculate the squared mean difference of the log flows
  log_sqr_mean_dif <- (log_agerage_peak-log_average_peak)^2
  
  #And the cubed average difference of the log flows
  log_cbd_mean_dif <- (log_agerage_peak-log_average_peak)^3
  
  #Now calculate the return period for ranked peak
  Return_Int <- (length(ranks)+1)/ranks
  
  #Convert that into exceedance probability
  E_p <- 1/Return_Int
  
  #Calculate the variance
  Variance <- sum(log_sqr_mean_dif)/(length(ranked_peaks)-1)
  
  #Calculate the Standard Deviation
  SD <- sqrt(Variance)
  
  #Calculate the skew
  Skew <- (length(peaks)*sum(log_cbd_mean_dif))/((length(peaks)-1)*(length(peaks)-2)*SD^3)
  
  Z_score <- stats::qnorm(1-1/Average_Return_Interval)
  
  ##Due the highly variable nature of regional skew just station skew has 
  #been used for this analysis. For more information on regional skew and the 
  # effect it has on return flow estimation please  refer to the following documents
  
  #Calculate the K
  Kp <- (2/Skew)*(1 + Skew*Z_score/6 - Skew**2/36)**3 - 2/Skew
  
  #Calculate the return flow in log space
  log_return_Q <- log_average_peak+Kp*SD
    
  #take the calculated return flow back to normal space
  Return_Flow <- exp(log_return_Q)
  
  #Now we need adjust the skew for weighting 
  #First the regional skewness, this comes from figure
  #Cr < 0.2
  
  #Second the variance of regional skewness, from Parrett et al. 2011
  #"Regional skew for California,and flood frequency for selected sites...
  #in the Sacramento–San Joaquin River Basin..." 
  #The average value in study was used
  #V_Cm	<- 0.364	
  
  return(Return_Flow)
  
}


# Function to calculate winter highflow annual metrics
calc_winter_highflow_annual_combined <- function(FlowYear, Original_method = TRUE) {
  
  max_zero_allowed_per_year <- 365
  max_nan_allowed_per_year <- 100
  
  #Set up output arrays
  Peak_Dur_10 <- c()
  Peak_Dur_2 <- c()
  Peak_Dur_5 <- c()
  Peak_10 <- c()
  Peak_2 <- c()
  Peak_5 <- c()
  Peak_Fre_10 <- c()
  Peak_Fre_2 <- c()
  Peak_Fre_5 <- c()
  Peak_Tim_10 <- c()
  Peak_Tim_2 <- c()
  Peak_Tim_5 <- c()
  
  # Define average recurrence interval
  recurance_intervals <- c(2, 5, 10)
  peak_flows <-c()
  
  #Determine how many water years there are 
  Water_Years <- unique(FlowYear$water_year)
  #Cycle through the years to determine the peak flows each year that has data
  for (i in 1:length(Water_Years)) {
    #Filters flow to the water year of interest and the previous water year
    flow <- filter(FlowYear, water_year== Water_Years[i])
    peak_flows[i] <- max(flow$flow,na.rm = TRUE)
    #if(length(flow$flow) >= 358){
    #  peak_flows[i] <- max(flow$flow,na.rm = TRUE)
    #}
    #else{
    #  peak_flows[i] <- NA
    #}
  }
  
  #Array for the return flow thresholds
  peak_exceedance_values <- c()

  #run through each recurrence interval and determine flow threshold for each
  for (ARI in recurance_intervals) {
    #This mode follows the original calculator
    if(Original_method ==TRUE){
      
      peak_exceedance_values <- c(peak_exceedance_values, quantile(peak_flows, 1 - 1 / ARI, na.rm = TRUE))
    }
    #This method uses Log Pearson Type III method to calculate the return flows
    else if(Original_method == FALSE){
      peak_exceedance_values <- c(peak_exceedance_values, Peak_Flow_Analysis(peak_flows, ARI))
    }
  }
  
  #Now iterate through the water years to to see when these flow occur
  for (i in seq_along(Water_Years)) {
    cat(Water_Years[i])
    flow <- filter(FlowYear, water_year == Water_Years[i])
    #Check to make sure that flow years qualifies for the analysis
    if (sum(is.na(flow$flow)) > max_nan_allowed_per_year || #First look at the number of NA values
        sum(flow$flow == 0, na.rm = TRUE) > max_zero_allowed_per_year ||#and the number of 0 flow days
        length(flow$flow) <= 358) { #Or finally that there is enough data
      cat("\n un qualifed year \n")
      #Set all the values to NA besides the peak flow thresholds
      Peak_Dur_10[i] <- NA
      Peak_Dur_2[i] <- NA
      Peak_Dur_5[i] <- NA
      Peak_10[i] <- peak_exceedance_values[3]
      Peak_2[i] <- peak_exceedance_values[1]
      Peak_5[i] <- peak_exceedance_values[2]
      Peak_Fre_10[i] <- NA
      Peak_Fre_2[i] <- NA
      Peak_Fre_5[i] <- NA
      Peak_Tim_10[i] <- NA
      Peak_Tim_2[i] <- NA
      Peak_Tim_5[i] <- NA
      next
    }
    #Now that the data has past the checks then we need to replace the NA data
    flow$flow <- replace_na(flow$flow)
    
    #If the year does qualify then iterate through exceed values 
    for (j in seq_along(peak_exceedance_values)) {
      cat(j)
      #Determine which flows qualify 
      qual_flows <- flow$flow >= peak_exceedance_values[j]
      
      #The duration is calculated by summing the number of days that qualify
      PH_Dur <- sum(qual_flows == TRUE)
      if(PH_Dur <1){
        PH_Dur <- NA
      }
      #Now we determine the median timing
      #first we determine where it transitions from flast to true
      transitions <- c(FALSE, diff(qual_flows) == 1)
      timings <- which(transitions)
      #Find the median start timing
      PH_Tim <- median_of_time(timings)
      
      # Find number of times that the flow crossed the exceed threshold values 
      Qualified_flows <- rle(qual_flows)
      PH_Fre <- sum(Qualified_flows$values & Qualified_flows$lengths >= 1)
      if(PH_Fre <1){
        PH_Fre <- NA
      }
      cat("Water Year: ",Water_Years[i],"\n required flow: ", peak_exceedance_values[j],"\n Dur: ",PH_Dur," Fre: ", PH_Fre," Tim: ",PH_Tim)
      if(j == 1){
        Peak_Dur_2[i] <- PH_Dur
        Peak_2[i] <- peak_exceedance_values[1]
        Peak_Fre_2[i] <- PH_Fre
        Peak_Tim_2[i] <- PH_Tim
      }
      if(j == 2){
        Peak_Dur_5[i] <- PH_Dur
        Peak_5[i] <- peak_exceedance_values[2]
        Peak_Fre_5[i] <- PH_Fre
        Peak_Tim_5[i] <- PH_Tim
      }
      if(j == 3){
        Peak_Dur_10[i] <- PH_Dur
        Peak_10[i] <- peak_exceedance_values[3]
        Peak_Fre_10[i] <- PH_Fre
        Peak_Tim_10[i] <- PH_Tim
      }
    }
    
  }
  High_flow_metrics <- list("Peak_Dur_10"=Peak_Dur_10,"Peak_Dur_2"=Peak_Dur_2,"Peak_Dur_5"=Peak_Dur_5,"Peak_10"=Peak_10,
                            "Peak_2"=Peak_2,"Peak_5"=Peak_5,"Peak_Fre_10"=Peak_Fre_10,"Peak_Fre_2"=Peak_Fre_2,
                            "Peak_Fre_5"=Peak_Fre_5,"Peak_Tim_10"=Peak_Tim_10,"Peak_Tim_2"=Peak_Tim_2,"Peak_Tim_5"=Peak_Tim_5)
  

  return(High_flow_metrics)
}
