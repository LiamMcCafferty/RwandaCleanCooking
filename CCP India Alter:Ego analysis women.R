#Empties Global Environment cache
rm(list = ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Desktop/India Clean Cooking Project")

#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each
#  library()
library(tidyverse)

load("alter_data.rda")

#Read in data
#Imports data and assigns it to variable "sample_data"
sample_data <- read.csv("SNA data_women.csv", 
                        stringsAsFactors = FALSE)
#Stores "sample_data" as a table data frame for easier reading
sample_data <- tbl_df(sample_data)

alter_frame$snaw_redcap_id















