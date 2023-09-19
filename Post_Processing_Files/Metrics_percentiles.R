#Makes percentiles dataframe from outputs
get_percentiles <- function(results_df, comid, percentiles, quantile_type){
  if(missing(percentiles)){
    percentiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  }
  
  if(missing(quantile_type)){
    quantile_type = 7
  }
  
  #set formatting for output
  options(scipen = 100, digits = 1)
  
  metrics_list <- list()
  for (metric in colnames(results_df)){
    if (metric == "Year" || metric == "WY_Cat" ){
      next
    }
    cat(metric)
    metrics_list[[metric]] = quantile(results_df[metric[metric>0]], probs=percentiles, na.rm=TRUE, names=TRUE, type=quantile_type)
  }
  output_data <- t(data.frame(metrics_list))
  colnames(output_data) <- paste("p", percentiles * 100, sep="")
  output_data <- as.data.frame(output_data)
  output_data["metric"] <- rownames(output_data)
  output_data["comid"] <- comid  # attach the comid column
  output_data["result_type"] <- "observed"
  return(output_data)
  
}

