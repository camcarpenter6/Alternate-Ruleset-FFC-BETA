#This script either makes single box plots for or comparison box plots to upto three data sets

single_box_plots <- function(results_df, data_title,save_loc){
  
  cat("\n Making box plots \n")
  
  lable_df <- read.csv("./Post_Processing_Files/Box_plot_lables.csv",header = T)
  
  lable_df <- lable_df[1:24,]
  
  cnames <- colnames(results_df)
    
  for (col in cnames) {

    

    if(all(is.na(results_df[[col]]))){
      cat("\n All of metrics for the following were NA and not plotted ",col, "\n")
      next
    }
    col_index <- which(lable_df$Flow.Metric.Code==col)
    
    Boxplot <- ggplot() +
      geom_boxplot(aes(x = col, y = as.numeric(results_df[[col]])),colour = 'blue' ) +
      geom_jitter(aes(x = col, y = as.numeric(results_df[[col]])),colour = 'darkblue',alpha = 0.6, width = 0.2, height = 0) +
      labs(x = data_title, y = lable_df$Unit[col_index], title = paste(data_title,": \n", lable_df$Flow.Metric.Name[col_index]))
    
    print(Boxplot)
    
    ggsave(paste0(save_loc,"/",lable_df$Flow.Metric.Name[col_index],".png"), plot = Boxplot, device = "png")
  }
  
  
  
}

#defining the function that will plot the metrics 
comparison_box_plot_print<- function(metrics_1,name_1, metrics_2,name_2, metrics_3= NULL,name_3 = NULL,Year_type,save_loc,data_title){
  
  lable_df <- read.csv("./Post_Processing_Files/Box_plot_lables.csv",header = T)
  
  lable_df <- lable_df[1:24,]
  
  cnames <- colnames(metrics_1)
  
  for (col in cnames[2:length(cnames)-1]) {
    #Check to see if the entire column are NA or NAN values
    if (all(is.na(metrics_1[[col]]) | is.nan(metrics_1[[col]]))) {
      # Set all values in the column to 0
      metrics_1[[col]] <- 0
    }
    #Move data from the dataframe to the placeholder list
    placeholder1 <- metrics_1[col]
    
    
    #Check to see if the entire column are NA or NAN values
    if (all(is.na(metrics_2[[col]]) | is.nan(metrics_2[[col]]))) {
      # Set all values in the column to 0
      metrics_2[[col]] <- 0
    }
    
    placeholder2 <- metrics_2[col]
    
    
    if(!is.null(metrics_3)){
    #Check to see if the entire column are NA or NAN values
      if (all(is.na(metrics_3[[col]]) | is.nan(metrics_3[[col]]))) {
        # Set all values in the column to 0
        metrics_3[[col]] <- 0
      }
      placeholder3 <- metrics_3[col]
      # Check if all values in the list are NA or NaN
      
    }
    
    col_index <- which(lable_df$Flow.Metric.Code==col)
    
    #cat(col)
    
    Boxplot <- ggplot()+
      geom_boxplot(aes(x= name_1, y=as.numeric(unlist(placeholder1))), color = "green")+
      geom_jitter(aes(x= name_1, y=as.numeric(unlist(placeholder1))),width = 0.2, height = 0, color = "darkgreen", alpha = 0.6) +
      geom_boxplot(aes(x= name_2, y=as.numeric(unlist(placeholder2))) , color = 'red')+
      geom_jitter(aes(x= name_2, y=as.numeric(unlist(placeholder2))) ,width = 0.2, height = 0, color = "darkred", alpha = 0.6) +
      
    #ggtitle("Middle Fork Yuba Flow Conditions Comparison Using Current FFC")
    
    if(!is.null(metrics_3)){
      Boxplot <- Boxplot +
        geom_boxplot(aes(x= name_3, y= as.numeric(unlist(placeholder3))), color = 'blue' )+
        geom_jitter(aes(x= name_3, y= as.numeric(unlist(placeholder3))),width = 0.2, height = 0, color = "darkblue", alpha = 0.6)
    }
    
    Boxplot <- Boxplot +
      labs(x = "Flow Regimes", y = lable_df$Unit[col_index], title = paste(data_title,"-", lable_df$Flow.Metric.Name[col_index]))
    
    plot(Boxplot)
    ggsave(paste0(save_loc,"/",Year_type,"figures","/",lable_df$Flow.Metric.Name[col_index]," - Year Type:",Year_type,".png"), plot = Boxplot, device = "png")
  }
}


comparison_boxplots <- function(Metric_dataframe_1,name_1,Metric_dataframe_2,name_2 ,Metric_dataframe_3=NULL,name_3=NULL,save_loc){
##Now make comparison Bar charts Need to run through step 4 to produce
##Loop through all the column names besides years to make the box plots

  Year_Typ <- c("All Years", "wet year" , "mod year" , "dry year" )
  
  data_title <- readline("Enter the Title you want for the box figures, the metric will be added to the end: ")
  
  for (typ in Year_Typ){
    if(typ == "All Years"){
      if(!is.null(Metric_dataframe_3)){
        comparison_box_plot_print(metrics_1 =  Metric_dataframe_1,name_1 = name_1,metrics_2 =  Metric_dataframe_2,name_2 = name_2 ,metrics_3 =   Metric_dataframe_3 ,name_3 = name_3, Year_type =  typ,save_loc =save_loc, data_title = data_title)#
      }else{
        comparison_box_plot_print(metrics_1 =  Metric_dataframe_1,name_1 = name_1,metrics_2 =  Metric_dataframe_2,name_2 = name_2,Year_type  =  typ,save_loc =  save_loc,data_title = data_title)#
      }
    }
    if(typ == "wet year"){
      cat(typ)
      Metric_set_1 <- filter(Metric_dataframe_1, WY_Cat == typ)
      Metric_set_2 <- filter(Metric_dataframe_2, WY_Cat == typ)
      if(!is.null(Metric_dataframe_3)){
      Metric_set_3 <- filter(Metric_dataframe_3, WY_Cat == typ)
      comparison_box_plot_print(metrics_1 =  Metric_set_1,name_1 = name_1,metrics_2 =  Metric_set_2,name_2 = name_2,metrics_3 =  Metric_set_3,name_3 =  name_3,Year_type =  typ,save_loc =  save_loc,data_title = data_title)
      }else{
        comparison_box_plot_print(metrics_1 =  Metric_set_1,name_1 = name_1,metrics_2 =  Metric_set_2,name_2 = name_2,Year_type =  typ,save_loc =  save_loc,data_title = data_title)
      }
    }
    if(typ == "mod year"){
      Metric_set_1 <- filter(Metric_dataframe_1, WY_Cat == typ)
      Metric_set_2 <- filter(Metric_dataframe_2, WY_Cat == typ)
      if(!is.null(Metric_dataframe_3)){
        Metric_set_3 <- filter(Metric_dataframe_3, WY_Cat == typ)
        comparison_box_plot_print(metrics_1 =  Metric_set_1,name_1 = name_1,metrics_2 =  Metric_set_2,name_2 = name_2,metrics_3 =  Metric_set_3,name_3 =  name_3,Year_type =  typ,save_loc =  save_loc,data_title = data_title)
      }else{
        comparison_box_plot_print(metrics_1 =  Metric_set_1,name_1 = name_1,metrics_2 =  Metric_set_2,name_2 = name_2,Year_type =  typ,save_loc =  save_loc,data_title = data_title)
      }
    }
    if(typ == "dry year"){
      Metric_set_1 <- filter(Metric_dataframe_1, WY_Cat == typ)
      Metric_set_2 <- filter(Metric_dataframe_2, WY_Cat == typ)
      if(!is.null(Metric_dataframe_3)){
        Metric_set_3 <- filter(Metric_dataframe_3, WY_Cat == typ)
        comparison_box_plot_print(metrics_1 =  Metric_set_1,name_1 = name_1,metrics_2 =  Metric_set_2,name_2 = name_2,metrics_3 =  Metric_set_3,name_3 =  name_3,Year_type =  typ,save_loc =  save_loc,data_title = data_title)
      }else{
        comparison_box_plot_print(metrics_1 =  Metric_set_1,name_1 = name_1,metrics_2 =  Metric_set_2,name_2 = name_2,Year_type =  typ,save_loc =  save_loc,data_title = data_title)
      }
    }

  }
}
