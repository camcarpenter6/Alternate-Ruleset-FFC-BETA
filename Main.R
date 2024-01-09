#This is the main script for running the altered function flow metrics calculator calculator 

#Please make sure you open up the files from the Rproject

#Firstcheck to see if all the required packages are installed
source("./required_packages.R")

#Now the packages are referenced
library(tidyverse); library(ffcAPIClient); library(plotly); library(devtools);library(smoother)
library(ggplot2);library(dplyr); library(pracma);library(here);library(dataRetrieval)

source(here("Preprocess","get_calculation_data.R"))
