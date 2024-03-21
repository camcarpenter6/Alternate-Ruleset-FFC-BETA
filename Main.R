#This is the main script for running the altered function flow metrics calculator calculator 

#Please make sure you open up the files from the Rproject

#First check to see if all the required packages are installed
source("./required_packages.R")

#Now the packages are referenced
library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);library(smoother)
library(ggplot2);library(dplyr); library(pracma);library(here);library(dataRetrieval)


#Clean the ffc account before running the calculator
#This only needs to be done once

ffctoken<- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJDYW1lcm9uIiwibGFzdE5hbWUiOiJDYXJwZW50ZXIiLCJlbWFpbCI6ImNhbWNhcnBlbnRlckB1Y2RhdmlzLmVkdSIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNjY5NzQxMjg2fQ.WoTh0hQX7oluRxjoTg3A0N5PJD6HnMCQs10CsgZqOTo"
clean_account(ffctoken)
set_token(ffctoken)

source(here("Preprocess","get_calculation_data.R"))
