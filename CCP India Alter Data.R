################################################################################
# PROJECT: Harvard NFL Players Health Study 
# PURPOSE: Import raw data from REDCap NFL survey and clean up alter centric data
# DIR:     "~/Desktop/NPHS Analysis"
# INPUTS:  Fake data created in REDCap ("fakedata_double.csv") 
#          Can be replaced with real data of participants.
# OUTPUTS: A alter_data.rda file that contains data on each alter of the networks.
# AUTHORS: Liam McCafferty, Amar Dhand
# CREATED: 10/29/18
# LATEST:  11/6/18
# PSERIES: NA
# NSERIES: NA
# NOTES:   
# ##############################################################################

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

#Read in data
#Imports data and assigns it to variable "sample_data"
sample_data <- read.csv("SNA data_women.csv", 
                        stringsAsFactors = FALSE)
#Stores "sample_data" as a table data frame for easier reading
sample_data <- tbl_df(sample_data)

#Creating the name_# keep/remove variables
alter_keeps <- sample_data %>% select(snaw_redcap_id, snaw_g7_name1:snaw_g7_name20)
alter_keeps[alter_keeps == ""] <- NA

for(i in 2:ncol(alter_keeps)){
  alter_keeps[, i] <- !is.na(alter_keeps[, i])
}


########################## Making the Dataframe ###############################

#Creating dataframe we will store all of our data in
alter_frame <- data.frame(snaw_redcap_id = NA, alter_ids = NA, alter_nums = NA)

#alter_keeps <- alter_keeps[1,]
#Function which creates dataframe containing alter info
alter_frame_maker <- function(alter_keeps){
  #This function is designed to take a single line from a selected set from the
  #alter keep/remove section. It then creates a dataframe that has the snaw_redcap_id
  #as its first variable, and the names of the vector as the values for "alter
  #names" the intention of this dataframe is to have it be added together with
  #all other dataframe of the data.
  
  #Saving the alter data
  workbench <- unlist(alter_keeps[1,2:21])
  #Saving the study id
  snaw_redcap_id <- unlist(alter_keeps[1,1])
  
  #Eliminating alters which are not marked as "keep", aka 1's.
  workbench <- names(workbench)[workbench == 1]
  
  test_frame <- data.frame(snaw_redcap_id = rep(snaw_redcap_id, length(workbench)))
  test_frame$alter_ids <- sub("snaw_g7_name","snaw_gX_name",workbench)
  test_frame$alter_nums <- as.integer(sub("snaw_g7_name","",workbench))
  
  
  return(test_frame)
}

#Appends alter info dataframes
for(i in 1:nrow(alter_keeps)){
  alter_frame <- rbind(alter_frame, alter_frame_maker(alter_keeps[i,]))
}
#Removes pre-set NA row from alter frame
alter_frame <- alter_frame[!is.na(alter_frame$snaw_redcap_id),]

########################## Important Functions ################################
# #Useful for accessing stuff:
# alter_set <- alter_frame[alter_frame$snaw_redcap_id  == 440,]
# suffix <- 22

info_finder <- function(alter_set,suffix){
  #Function which finds the data associated with alters at a certain set of
  #  numbers.
  #The var suffix has to be entered exactly to the name of the variable in the
  #  dataset that we are accessing. "Name" and the number are pre-provided. For
  #  example, accessing "name#" would be suffix = "", accessing "name#educ" would
  #  be suffix = "educ". Returns an unlisted vector.
  #Note that this function is only designed for a single study id, thus it
  #  should be used in a forloop or by function
  
  #Note only works on radial or text enter data types. Not checkbox. I will need
  #to do checkbox manually
  
  #Error check to make sure that there is only one study id being accessed.
  if(length(unique(alter_set$snaw_redcap_id)) != 1){
    stop("Too many study ID's in input, isolate them")
  }
  #Isolating study id and accessing sample_data
  snaw_redcap_id <- unique(alter_set$snaw_redcap_id)
  workbench <- sample_data[sample_data$snaw_redcap_id == snaw_redcap_id,]
  
  #Adding info from dataset to blank vector depending upon entered suffix variable
  alter_info <- unlist(workbench[sub("X",suffix,alter_set$alter_ids)])
  
  return(alter_info)
}


############# Creating an ordered factor to keep order consistent #############

alter_id_factor <- factor(alter_frame$snaw_redcap_id,
                          levels = unique(alter_frame$snaw_redcap_id))

############################# Adding alter names ##############################

alter_names <- unlist(by(alter_frame, alter_id_factor, info_finder, suffix = "7"))
alter_names <- sapply(alter_names,function(x){return(trimws(gsub("[)]","",unlist(strsplit(x, "[(]"))[2]), "both"))})


alter_frame$alter_names <- alter_names

############################ Adding Sex #######################################

alter_frame$sex <- unlist(by(alter_frame, alter_id_factor,
                             info_finder, suffix = "11"))

alter_frame$sex <- factor(alter_frame$sex, levels = c(1,2))
levels(alter_frame$sex) <- c("Male","Female")

############################ Adding Age #######################################

alter_frame$age <- unlist(by(alter_frame, alter_id_factor,
                             info_finder, suffix = "12"))

############################ Adding Education Highest #########################

alter_frame$education <- unlist(by(alter_frame, alter_id_factor,
                             info_finder, suffix = "13"))

alter_frame$education <- factor(alter_frame$education, levels = c(1,2,3,4,5,6,9))
levels(alter_frame$education) <- c("none", "up to class 4", "class 5 to class 8",
                                   "class 9 to class 10", "class 11 to class 12",
                                   "college", "Don't know")

############################ Adding SHG #######################################

alter_frame$shg <- unlist(by(alter_frame, alter_id_factor,
                             info_finder, suffix = "14"))

alter_frame$shg <- factor(alter_frame$shg, levels = c(1,0))
levels(alter_frame$shg) <- c("Yes","No")

############################ Adding Relationships #############################

alter_frame$relationship <- unlist(by(alter_frame, alter_id_factor,
                             info_finder, suffix = "15"))

alter_frame$relationship <- factor(alter_frame$relationship, levels = c(1,2,3,4,5,6,7,8,9,10,11,99))
levels(alter_frame$relationship) <- c("Spouse", "Parent", "Sibling", "Child",
                                      "Other family", "Coworker", "Co-member in org",
                                      "Neighbor", "Friend", "Advisor", "Other", "Don't know")

############################ Adding LPG Connection ############################

alter_frame$lpg <- unlist(by(alter_frame, alter_id_factor,
                                      info_finder, suffix = "16"))

alter_frame$lpg <- factor(alter_frame$lpg, levels = c(1,2,9))
levels(alter_frame$lpg) <- c("yes", "no", "Don't know")

############################ Adding LPG Connection Number #####################

alter_frame$lpg_num <- unlist(by(alter_frame, alter_id_factor,
                             info_finder, suffix = "17"))

alter_frame$lpg_num <- factor(alter_frame$lpg_num, levels = c(1,2,3,4,5,6,7,10,9))
levels(alter_frame$lpg_num) <- c("1", "2", "3", "4", "5", "6", "More than 6", "10", "Don't know")

############################ Adding Stacking LPG ##############################

alter_frame$stacking <- unlist(by(alter_frame, alter_id_factor,
                                 info_finder, suffix = "17"))

alter_frame$stacking <- factor(alter_frame$stacking, levels = c(1,2,3,9))
levels(alter_frame$stacking) <- c("only LPG", "Both LPG and other stoves", "Does not own LPG", "don't know")

### Incomplete from here: enter all of the other variables that match up.


#Exporting data
save(alter_frame, file = "alter_data.rda")
