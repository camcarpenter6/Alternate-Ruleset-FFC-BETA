library(dplyr)

# Define a function to set user parameters
set_user_params <- function(user_params, def_params) {
  for (key in names(def_params)) {
    if (key %in% names(user_params)) {
      def_params[[key]] <- user_params[[key]]
    }
  }
  return(def_params)
}

# Define a function to calculate the average of each column in a matrix
calculate_average_each_column <- function(matrix) {
  averages <- numeric()
  for (index in 1:ncol(matrix)) {
    averages <- c(averages, mean(matrix[, index], na.rm = TRUE))
  }
  return(averages)
}

# Define a function to create folders
create_folders <- function() {
  folders <- c(
    'post_processedFiles/Boxplots', 
    'post_processedFiles/Wateryear_Type', 
    'post_processedFiles/Supplementary_Metrics', 
    'post_processedFiles/Class-1', 
    'post_processedFiles/Class-2', 
    'post_processedFiles/Class-3', 
    'post_processedFiles/Class-4', 
    'post_processedFiles/Class-5', 
    'post_processedFiles/Class-6', 
    'post_processedFiles/Class-7', 
    'post_processedFiles/Class-8', 
    'post_processedFiles/Class-9'
  )
  
  for (folder in folders) {
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }
  }
}

# Define a function to calculate the median of time
median_of_time <- function(lt) {
  n <- length(lt)
  if (n < 1) {
    return(NULL)
  } else if (n %% 2 == 1) {
    return(lt[[n %/% 2]]$start_date)
  } else if (n == 2) {
    first_date <- lt[[1]]$start_date
    second_date <- lt[[2]]$start_date
    return((first_date + second_date) / 2)
  } else {
    first_date <- lt[[n %/% 2 - 1]]$start_date
    second_date <- lt[[n %/% 2 + 1]]$start_date
    return((first_date + second_date) / 2)
  }
}

# Define a function to calculate the median of magnitude
median_of_magnitude <- function(object_array) {
  flow_array <- numeric()
  for (obj in object_array) {
    flow_array <- c(flow_array, obj$flow)
  }
  return(mean(flow_array, na.rm = TRUE))
}

# Define a function to calculate the peak magnitude
peak_magnitude <- function(object_array) {
  flow_array <- numeric()
  for (obj in object_array) {
    flow_array <- c(flow_array, obj$flow)
  }
  return(max(flow_array, na.rm = TRUE))
}

# Define a class for FlowExceedance
FlowExceedance <- R6::R6Class("FlowExceedance",
                              public = list(
                                start_date = NULL,
                                end_date = NULL,
                                duration = NULL,
                                flow = numeric(),
                                exceedance = NULL,
                                max_magnitude = NULL,
                                initialize = function(start_date, end_date, duration, exceedance) {
                                  self$start_date <- start_date
                                  self$end_date <- end_date
                                  self$duration <- duration
                                  self$flow <- numeric()
                                  self$exceedance <- exceedance
                                  self$max_magnitude <- NULL
                                },
                                add_flow = function(flow_data) {
                                  self$flow <- c(self$flow, flow_data)
                                },
                                get_max_magnitude = function() {
                                  self$max_magnitude <- max(self$flow, na.rm = TRUE)
                                }
                              )
)

# Define a function to calculate winter highflow annual metrics
calc_winter_highflow_annual <- function(matrix, exceedance_percent, winter_params = def_winter_params) {
  params <- set_user_params(winter_params, def_winter_params)
  
  max_zero_allowed_per_year <- params$max_zero_allowed_per_year
  max_nan_allowed_per_year <- params$max_nan_allowed_per_year
  
  # Get peak percentiles calculated from each year's peak flow values
  peak_flows <- apply(matrix, 2, max, na.rm = TRUE)
  peak_percentiles <- c(2, 5, 10, 20, 50) # for peak flow metrics
  high_percentiles <- c(2, 5, 10, 20) # for high flow metrics
  
  peak_exceedance_values <- numeric()
  highflow_exceedance_values <- numeric()
  
  for (percentile in peak_percentiles) {
    peak_exceedance_values <- c(peak_exceedance_values, quantile(peak_flows, 1 - percentile / 100, na.rm = TRUE))
  }
  
  # Add high flow percentiles and peak flow exceedance vals together for final list of exceedance values
  for (i in high_percentiles) {
    highflow_exceedance_values <- c(highflow_exceedance_values, quantile(matrix, 1 - i / 100, na.rm = TRUE))
  }
  
  exceedance_values <- c(peak_exceedance_values, highflow_exceedance_values)
  
  exceedance_value <- list()
  freq <- list()
  duration <- list()
  timing <- list()
  magnitude <- list()
  peak_magnitude <- list()
  
  for (i in seq_along(exceedance_values)) {
    exceedance_value[[i]] <- exceedance_values[i]
    freq[[i]] <- numeric()
    duration[[i]] <- numeric()
    timing[[i]] <- numeric()
    magnitude[[i]] <- numeric()
    peak_magnitude[[i]] <- numeric()
  }
  
  for (column_number in seq_len(ncol(matrix))) {
    if (sum(is.na(matrix[, column_number])) > max_nan_allowed_per_year || sum(matrix[, column_number] == 0, na.rm = TRUE) > max_zero_allowed_per_year) {
      for (i in seq_along(exceedance_values)) {
        freq[[i]] <- c(freq[[i]], NA)
        duration[[i]] <- c(duration[[i]], NA)
        timing[[i]] <- c(timing[[i]], NA)
        magnitude[[i]] <- c(magnitude[[i]], NA)
        peak_magnitude[[i]] <- c(peak_magnitude[[i]], NA)
      }
      next
    }
    
    exceedance_object <- list()
    exceedance_duration <- list()
    current_flow_object <- list()
    peak_flow <- list()
    
    for (i in seq_along(exceedance_values)) {
      exceedance_object[[i]] <- list()
      exceedance_duration[[i]] <- numeric()
      current_flow_object[[i]] <- NULL
      peak_flow[[i]] <- numeric()
    }
    
    for (row_number in seq_len(nrow(matrix))) {
      for (i in seq_along(exceedance_values)) {
        if ((flow_row <- matrix[row_number, column_number]) < exceedance_value[[i]] && !is.null(current_flow_object[[i]]