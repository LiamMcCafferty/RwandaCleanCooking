#Empties Global Environment cache
rm(list = ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
setwd("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/India Clean Cooking Project")

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

ego_data_add <- read.csv("LPG project ego info-women.csv")
ego_data_add$household_group <- factor(ego_data_add$household_group,
																			 levels = c(2,1,3),
																			 labels = c("no", "yes", "Don't know"))

ego_data_add$caste_of_the_respondent <- 
	factor(ego_data_add$caste_of_the_respondent,
				 levels = c(1,3,2,4,5,9),
				 label = c("General", "SC/ST", "OBC", "other religious minorities", "Others", "Don't know"))


ego_data_add$record_id %in% sample_data$snaw_redcap_id
sample_data$snaw_redcap_id %in% ego_data_add$record_id

sample_data <- left_join(sample_data,
												 select(ego_data_add, "snaw_redcap_id" = "record_id",
												 			 "household_group", "caste_of_the_respondent"),
												 by = "snaw_redcap_id")



#Making all "Don't know's" under lpg into NA's
table(alter_frame$lpg, useNA = "always")
levels(alter_frame$lpg) <- c("yes", "no", NA)
table(alter_frame$lpg, useNA = "always")


input_frame <- alter_frame %>% subset(snaw_redcap_id == 323)

lpg_byproduct <- by(alter_frame,
	 factor(alter_frame$snaw_redcap_id, levels = unique(alter_frame$snaw_redcap_id)),
	 function(input_frame){
	 	lpg_count <- input_frame$lpg == "yes"
	 	total_lpg_count <- !(input_frame$lpg %>% is.na())
	 	total_caste_count <- !(input_frame$caste %>% is.na())
	 	
	 	ouput_prop <- (lpg_count %>% sum()) / (total_lpg_count %>% sum())
	 	
	 	input_sample <- sample_data[sample_data$snaw_redcap_id %in%
	 																unique(input_frame$snaw_redcap_id),]
	 	
	 	caste_match <- input_sample$caste_of_the_respondent == input_frame$caste
	 	
	 	lpg_caste_match <- ((lpg_count & caste_match) %>% sum()) /
	 		((total_lpg_count & total_caste_count) %>% sum())
	 	
	 	return(c(unique(input_frame$snaw_redcap_id), ouput_prop, lpg_caste_match))
	 })

lpg_matrix <- matrix(data = unlist(lpg_byproduct),
			 ncol = length(lpg_byproduct[[1]]),
			 nrow = length(lpg_byproduct),
			 byrow = TRUE)

lpg_matrix <- as.data.frame(lpg_matrix) %>%
	"colnames<-"(c("snaw_redcap_id", "prop_lpg", "lpg_caste_match"))

sample_data <- left_join(sample_data, lpg_matrix, by = "snaw_redcap_id")

hist(sample_data$lpg_caste_match)


#Function which makes a basic network matrix used by multiple functions
make_base_mat <- function(x){
	##########
	# Function: Creates an NA-stripped matrix from a single row dataset
	# Inputs: x = Variable that stores the dataset
	# Ouputs: matrix "mat", the matrix will be stripped of people which have zero 
	#					ties, the matrix will also turn diagonal NA's (and mistaken NA's) 
	#					into 0's
	##########
	
	#Saves the ties (edge data) of all egos and nodes as a 2D vector
	shape <- select(x, "snaw_g9_name1":"snaw_g10_name19name20")
	shape_alter <- shape %>% select(-snaw_g9_name1:-snaw_g9_name20)
	shape_alter <- shape_alter - 1
	shape <- cbind(select(shape, snaw_g9_name1:snaw_g9_name20), shape_alter)
	
	shape[shape %in% 1] <- 9
	shape[shape %in% 2] <- 1
	shape[shape %in% 9] <- 2
	
	#Saves tie values as a 1D vector of integers.
	ties  <- as.integer(shape)
	#Creates a blank matrix
	mat   <- matrix(NA, 21, 21)
	#Fills the lower triangle of the matrix with the vector "ties"
	mat[lower.tri(mat)] <- ties
	#Transposes the lower triangle into the upper triangle of the matrix
	mat   <- t(mat)
	#Refills the lower triangle of the matrix with ties
	mat[lower.tri(mat)] <- ties
	#Names the columns and rows with EGO as the first row/col, row/col 2:16 are numbered
	#  1:15 respectively.
	colnames(mat) <- rownames(mat) <- c("EGO", "1", "2", "3", "4", "5", "6", "7", 
																			"8", "9", "10", "11", "12", "13", "14", "15",
																			"16", "17", "18", "19", "20")
	
	#Removes columns and rows which have no tie entries, this removes people who are
	#  duplicates or over 10 and thus were not given any tie values.
	mat <- mat[(!colSums(mat,1) == 0), (!colSums(mat,1) == 0)]
	#Fills diagonal with 0s
	diag(mat) <- 0
	
	#Saves the named social ties from the survey
	name_ties <- x %>% select(snaw_g7_name1:snaw_g7_name20)
	name_ties <- apply(name_ties,2,function(x){return(trimws(gsub("[)]","",unlist(strsplit(x, "[(]"))[2]), "both"))})
	
	#Converts vector of names into a dataframe
	name_ties <- data.frame(Row = name_ties)
	#Add a column to name_ties which matches the names of the matrix coloumns (1-15)
	name_ties$Replacement <- c("1", "2", "3", "4", "5", "6", "7", 
														 "8", "9", "10", "11", "12", "13", "14",
														 "15", "16", "17", "18", "19", "20")
	#Saves names to the columns of name_ties for sorting
	colnames(name_ties) <- c("Name", "Current")
	#Create a new row to replace the name "EGO" from the matrix with the word "You"
	ego_df <- c("EGO", "EGO")
	ego_df <- as.data.frame(t(ego_df))
	colnames(ego_df) <- c("Name", "Current") 
	#Bind the name_ties with the new ego_df
	name_ties <- rbind(ego_df, name_ties)
	
	#Replace the matrix names with those from name_ties
	names <- match(colnames(mat), name_ties$Current) 
	colnames(mat) <- rownames(mat) <- name_ties$Name[names]
	
	return(mat)
}


sample_data$alter_count <- NA

for(i in 1:nrow(sample_data)){
	sample_data$alter_count[i] <- make_base_mat(sample_data[i,]) %>% nrow()
}

save(sample_data, file = "women_ego_alter_data.rda")




