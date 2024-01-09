library(dplyr);library(ggplot2);library(here)

#This code plots the metrics over the flow data for one data series



single_series_HTML <- function(flow,results_df,gage_id,figuretitle){
  
  
  #Make the title for the figure
  fig_title <- paste("Gage ID:",gage_id,"-", figuretitle)
  
  #With the updated code we can have -9999 values which need to be removed since they will mess up the date
  Clean_metrics <- results_df %>% mutate_all(~ ifelse(. == -9999,NA, .))

# NOTE: timing is output in water year days. Need to convert to Julian days before plotting
  FF_Dates <- Clean_metrics %>%  #Update the name of the data from
    mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y')-92, #Date in Julian days
           FA_Tim_Dt = as.Date(paste(FA_Tim, Year-1), '%j %Y')+273, #add 274 because water year starts Oct 1
           Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y')-92, #Date in Julian days
           SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y')-92) #Date in Julian days

  # Add timing for each flow metric
  Flow_MetTim <- flow %>% 
    left_join(dplyr::select(FF_Dates, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt"))%>% 
    left_join(dplyr::select(FF_Dates, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt"))%>% 
    left_join(dplyr::select(FF_Dates, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))

# Create interactive figure
  Flow_metric_fig <- plot_ly(data = Flow_MetTim, x = ~date, y = ~flow, type = 'scatter', mode = 'line',
                             name = "Discharge") %>%
    add_trace(y = ~DS_Mag_50,  name = 'DS_Mag_50 + DS_Tim', mode = 'markers') %>%  #Add DS timing
    add_trace(y = ~FA_Mag,  name = 'FA_Mag + FA_Tim', mode = 'markers') %>%  #Add Fall timing
    add_trace(y = ~Wet_BFL_Mag_10,  name = 'Wet_BFL_Mag_10 + Wet_Tm', mode = 'markers') %>%  #Add Wet timing
    add_trace(y = ~SP_Mag,  name = 'SP_Mag + SP_Tm', mode = 'markers') %>%  #Add Spring timing
    layout(title = fig_title, #Change site name here
           xaxis = list(title = "Date"),
           yaxis = list (title = "Streamflow (CFS)"))

  Flow_metric_fig

  #Make the output file path
  fig_path <- file.path(here(), "Outputs", gage_name_cleaned, paste0(gage_name_cleaned,"-",figuretitle, "_Timeseries.html"))

  #Save interactive hydrograph as .html webpage
  htmlwidgets::saveWidget(as_widget(Flow_metric_fig), fig_path)

  
}

HTML_comparison <- function(flow_1, flow_2, metrics_1, metrics_2, flow_3 = NULL, metrics_3 = NULL, output_file_name) {
  
  # With the updated code we can have -9999 values which need to be removed since they will mess up the date
  Clean_metrics_1 <- metrics_1 %>% mutate_all(~ ifelse(. == -9999, NA, .))
  Clean_metrics_2 <- metrics_2 %>% mutate_all(~ ifelse(. == -9999, NA, .))
  
  if (!is.null(metrics_3)) {
    Clean_metrics_3 <- metrics_3 %>% mutate_all(~ ifelse(. == -9999, NA, .))
  }
  
  # Set the names of the data of interest
  name_1 <- readline("Please enter the name you want to use for the first set of flow data: ") 
  name_2 <- readline("Please enter the name you want to use for the second set of flow data: ")
  
  if (!is.null(flow_3)) {
    !is.null(flow_3)
    name_3 <- readline("Please enter the name you want to use for the third set of flow data: ")
  }
  
  # Enter the title for the figures title
  fig_title <- readline("Please enter the title you want for this comparison figure: ")
  
  fig_file_path <- file.path(here(), "Outputs", output_file_name, paste0(fig_title, ".html"))
  
  # Find date of all timing metrics
  
  # NOTE: 1) timing is output in water year days. Need to convert to Julian days before plotting
  #       2) Metrics from the original calculator need to be adjusted by -93 and 274, since python indexes different than R
  FF_Dates_1 <- Clean_metrics_1 %>% 
    mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y') - 92,
           FA_Tim_Dt = as.Date(paste(FA_Tim, Year - 1), '%j %Y') + 273,
           Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y') - 92,
           SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y') - 92)
  
  FF_Dates_2 <- Clean_metrics_2 %>% 
    mutate(DS_Tim_Dt = as.Date(paste((DS_Tim - 3), Year), '%j %Y') - 92,
           FA_Tim_Dt = as.Date(paste(FA_Tim, Year - 1), '%j %Y') + 273,
           Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y') - 92,
           SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y') - 92)
  
  if (!is.null(metrics_3)) {
    FF_Dates_3 <- Clean_metrics_3 %>% 
      mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y') - 92,
             FA_Tim_Dt = as.Date(paste(FA_Tim, Year - 1), '%j %Y') + 273,
             Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y') - 92,
             SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y') - 92)
  }
  
  # Add timing for each flow metric
  Flow_MetTim_1 <- flow_1 %>% 
    left_join(dplyr::select(FF_Dates_1, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_1, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_1, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_1, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))
  
  Flow_MetTim_2 <- flow_2 %>% 
    left_join(dplyr::select(FF_Dates_2, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_2, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_2, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_2, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))
  
  if (!is.null(metrics_3)) {
    Flow_MetTim_3 <- flow_3 %>% 
      left_join(dplyr::select(FF_Dates_3, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt")) %>% 
      left_join(dplyr::select(FF_Dates_3, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt")) %>% 
      left_join(dplyr::select(FF_Dates_3, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
      left_join(dplyr::select(FF_Dates_3, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))
  }
  
  # Create interactive figure
  test_fig <- ggplot() +
    geom_line(data = Flow_MetTim_1, aes(x = date, y = flow, color = name_1)) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = DS_Mag_50, color = name_1, shape = "DS_Mag_50 + DS_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = FA_Mag, color = name_1, shape = "FA_Mag + FA_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = Wet_BFL_Mag_10, color = name_1, shape = "Wet_BFL_Mag_10"), size = 1.5) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = SP_Mag, color = name_1, shape = "SP_Mag + SP_Tm"), size = 1.5) +
    geom_line(data = Flow_MetTim_2, aes(x = date, y = flow, color = name_2)) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = DS_Mag_50, color = name_2, shape = "DS_Mag_50 + DS_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = FA_Mag, color = name_2, shape = "FA_Mag + FA_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = Wet_BFL_Mag_10, color = name_2, shape = "Wet_BFL_Mag_10"), size = 1.5) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = SP_Mag, color = name_2, shape = "SP_Mag + SP_Tm"), size = 1.5)
  
  if (!is.null(metrics_3)) {
    test_fig <- test_fig +
      geom_line(data = Flow_MetTim_3, aes(x = date, y = flow, color = name_3)) +
      geom_point(data = Flow_MetTim_3, aes(x = date, y = DS_Mag_50, color = name_3, shape = "DS_Mag_50 + DS_Tim_Flow"), size = 1.5) +
      geom_point(data = Flow_MetTim_3, aes(x = date, y = FA_Mag, color = name_3, shape = "FA_Mag + FA_Tim_Flow"), size = 1.5) +
      geom_point(data = Flow_MetTim_3, aes(x = date, y = Wet_BFL_Mag_10, color = name_3, shape = "Wet_BFL_Mag_10"), size = 1.5) +
      geom_point(data = Flow_MetTim_3, aes(x = date, y = SP_Mag, color = name_3, shape = "SP_Mag + SP_Tm"), size = 1.5)
  }
  
  test_fig <- test_fig +
    scale_colour_manual(values = c("chartreuse3", "blue")) +
    scale_shape_manual(values = c(0, 2, 1, 5)) +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5)) +
    labs(x = "Date", y = "Streamflow (cfs)") +
    ggtitle(fig_title)
  
  print_fig <- ggplotly(test_fig, dynamicTicks = "y")
  
  print(print_fig)  
  
  # Save interactive hydrograph as .html webpage
  htmlwidgets::saveWidget(widget = print_fig, file = fig_file_path, selfcontained = TRUE)
}


HTML_comparison_OrginalvsAlternate <- function(flow_1, flow_2, metrics_1, metrics_2, gage_id, output_file_name) {
  
  # With the updated code we can have -9999 values which need to be removed since they will mess up the date
  Clean_metrics_1 <- metrics_1 %>% mutate_all(~ ifelse(. == -9999, NA, .))
  Clean_metrics_2 <- metrics_2 %>% mutate_all(~ ifelse(. == -9999, NA, .))
  
  #check to if all of the a column is NA if so change it all to 0s
  for (col in names(Clean_metrics_1)) {
    if(all(is.na(Clean_metrics_1[[col]]))) {
      # Replace all NA values in the column with zeros
      Clean_metrics_1[[col]][is.na(Clean_metrics_1[[col]])] <- 0
    }
  }
  
  #Do the same for the other metrics
  for (col in names(Clean_metrics_2)) {
    if(all(is.na(Clean_metrics_2[[col]]))) {
      # Replace all NA values in the column with zeros
      Clean_metrics_2[[col]][is.na(Clean_metrics_2[[col]])] <- 0
    }
  }
  
  # Set the names of the data of interest
  name_1 <- "Alternate FFC Metrics" 
  name_2 <- "Original Calculator Metrics"
  
  # Enter the title for the figures title
  fig_title <- paste0("Gage ",gage_id," Alternate and Original Comparison")
  
  fig_file_path <- file.path(here(), "Outputs", output_file_name, paste0(fig_title, ".html"))
  
  # Find date of all timing metrics
  
  # NOTE: 1) timing is output in water year days. Need to convert to Julian days before plotting
  #       2) Metrics from the original calculator need to be adjusted by -93 and 274, since python indexes different than R
  FF_Dates_1 <- Clean_metrics_1 %>% 
    mutate(DS_Tim_Dt = as.Date(paste(DS_Tim, Year), '%j %Y') - 92,
           FA_Tim_Dt = as.Date(paste(FA_Tim, Year - 1), '%j %Y') + 273,
           Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y') - 92,
           SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y') - 92)
  
  FF_Dates_2 <- Clean_metrics_2 %>% 
    mutate(DS_Tim_Dt = as.Date(paste((DS_Tim - 3), Year), '%j %Y') - 91,
           FA_Tim_Dt = as.Date(paste(FA_Tim, Year - 1), '%j %Y') + 274,
           Wet_Tim_Dt = as.Date(paste(Wet_Tim, Year), '%j %Y') - 91,
           SP_Tim_Dt = as.Date(paste(SP_Tim, Year), '%j %Y') - 91)

  
  # Add timing for each flow metric
  Flow_MetTim_1 <- flow_1 %>% 
    left_join(dplyr::select(FF_Dates_1, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_1, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_1, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_1, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))

  Flow_MetTim_2 <- flow_2 %>% 
    left_join(dplyr::select(FF_Dates_2, DS_Tim_Dt, DS_Mag_50), by = c("date" = "DS_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_2, FA_Tim_Dt, FA_Mag), by = c("date" = "FA_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_2, Wet_Tim_Dt, Wet_BFL_Mag_10), by = c("date" = "Wet_Tim_Dt")) %>% 
    left_join(dplyr::select(FF_Dates_2, SP_Tim_Dt, SP_Mag), by = c("date" = "SP_Tim_Dt"))

  
  # Create interactive figure
  test_fig <- ggplot() +
    geom_line(data = Flow_MetTim_1, aes(x = date, y = flow, color = name_1)) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = DS_Mag_50, color = name_1, shape = "DS_Mag_50 + DS_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = FA_Mag, color = name_1, shape = "FA_Mag + FA_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = Wet_BFL_Mag_10, color = name_1, shape = "Wet_BFL_Mag_10"), size = 1.5) +
    geom_point(data = Flow_MetTim_1, aes(x = date, y = SP_Mag, color = name_1, shape = "SP_Mag + SP_Tm"), size = 1.5) +
    geom_line(data = Flow_MetTim_2, aes(x = date, y = flow, color = name_2)) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = DS_Mag_50, color = name_2, shape = "DS_Mag_50 + DS_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = FA_Mag, color = name_2, shape = "FA_Mag + FA_Tim_Flow"), size = 1.5) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = Wet_BFL_Mag_10, color = name_2, shape = "Wet_BFL_Mag_10"), size = 1.5) +
    geom_point(data = Flow_MetTim_2, aes(x = date, y = SP_Mag, color = name_2, shape = "SP_Mag + SP_Tm"), size = 1.5)

  
  test_fig <- test_fig +
    scale_colour_manual(values = c("chartreuse3", "blue")) +
    scale_shape_manual(values = c(0, 2, 1, 5)) +
    scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5)) +
    labs(x = "Date", y = "Streamflow (cfs)") +
    ggtitle(fig_title)

  print_fig <- ggplotly(test_fig, dynamicTicks = "y")
  cat("\n broke after print fig  \n")
  print(print_fig)  
  
  # Save interactive hydrograph as .html webpage
  htmlwidgets::saveWidget(widget = print_fig, file = fig_file_path, selfcontained = TRUE)
}

