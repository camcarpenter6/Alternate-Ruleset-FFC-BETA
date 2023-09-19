#code updated from original calculator to remove NA values from flow time series
replace_na <- function(flow_data) {
  for (index in seq_along(flow_data)) {
    if (index == 1 & is.na(flow_data[index])) {
      flow_data[index] <- 0
    }
    else if(is.na(flow_data[index])){
      flow_data[index] <- flow_data[index-1]
    }
  }
  return(flow_data)
}
