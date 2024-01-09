#This script installs the required packages

cat("if your computer promts you to restart RStudio, select 'No' or you will get stuck in an endless loop")

packages <- c("dataRetrieval","devtools","dplyr","fs","ggplot2","glue","here","janitor","plotly","lubridate","segmented","tidyverse","ffcAPIClient","pracma","readr","devtools","strucchange","zoo","smoother","here")

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

ffctoken<- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJDYW1lcm9uIiwibGFzdE5hbWUiOiJDYXJwZW50ZXIiLCJlbWFpbCI6ImNhbWNhcnBlbnRlckB1Y2RhdmlzLmVkdSIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNjY5NzQxMjg2fQ.WoTh0hQX7oluRxjoTg3A0N5PJD6HnMCQs10CsgZqOTo"
clean_account(ffctoken)
set_token(ffctoken)
