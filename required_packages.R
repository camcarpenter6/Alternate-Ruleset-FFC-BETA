#This script installs the required packages

cat("if your computer promts you to restart RStudio, select 'No' or you will get stuck in an endless loop")

packages <- c("dataRetrieval","devtools","dplyr","fs","ggplot2","glue","here","janitor","plotly","lubridate","segmented","tidyverse","ffcAPIClient","pracma","readr","devtools","strucchange","zoo")

for (i in packages){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
}



library(devtools) 

if(!require("ffcAPIClient")){
  devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient') 
  install.packages("ffcAPIClient")
}
if(!require("wateRshedTools")){
  devtools::install_github("ryanpeek/wateRshedTools") 
  install.packages("wateRshedTools")
}
