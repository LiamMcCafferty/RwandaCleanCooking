##### Setup for code and establishing functions #####

#Empties Global Environment cache
rm(list = ls())

setwd("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/India Clean Cooking Project")

load("alter_data.rda")

load("women_ego_alter_data.rda")

# Cutting out inviduals with 0 people w/in their network
sample_data <- sample_data %>% subset(alter_count > 0)


#Detatches all packages from current iteration of R, most packages interfere with this code
detach_all_packages <- function() {
	##########
	# Function: Detatches all attatched packages from current instance of R
	# Inputs: none, just call the function
	# Ouputs: none
	# Credit to mjaniec on stack overflow for function logic
	##########  
	basic.packages <- c("package:stats", "package:graphics", "package:grDevices",
											"package:utils", "package:datasets",
											"package:methods", "package:base")
	package.list <- search()[ifelse(unlist(gregexpr("package:",
																									search())) == 1, TRUE, FALSE)]
	package.list <- setdiff(package.list, basic.packages)
	if (length(package.list) > 0)  for (package in package.list) detach(package,
																																			character.only = TRUE)
}
detach_all_packages()


library(tidyverse) # For data management
library(igraph) # To transform and analyze network data

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

##### Constructing Matrix Vector and Graphs of all egos #######

mats <- vector(mode = "list", length = nrow(sample_data))

for(i in 1:nrow(sample_data)){
	mats[[i]] <- make_base_mat(sample_data[i,])
}

names(mats) <- sample_data$snaw_redcap_id

calc_graphs <- vector(mode = "list", length = nrow(sample_data))

graph.adjacency(mats[[30]][-1,-1], mode = "undirected", weighted = TRUE)

calc_graphs <- lapply(
	lapply(mats, function(str_matrix){str_matrix[-1,-1] %>% return()}),
	graph.adjacency, mode = "undirected", weighted = TRUE)

# calc_graphs[[12]]
# mats[[12]]

######## Network Calculations #######

# Calculate maximum, avg degree, and density of alters after removal of Ego
max_degree <- unlist(lapply(calc_graphs, function(x){max(degree(x))}))

mean_degree <- unlist(lapply(calc_graphs, function(x){mean(degree(x))}))

density <- unlist(lapply(calc_graphs, function(x){graph.density(x)}))

#*Important to note: The above is calculated WITHOUT THE EGO, whereas below is 
#  calculated WITH THE EGO.*
#Calculate constraint and effective size (Burt, 1992)
library(egonet) # careful of interference with igraph functions. Do not load 
#                 before calculating degree and density without EGO (above)

#Calculate constraint with EGO
constraint <- unlist(lapply(mats, function(x){as.numeric(index.egonet(x, index = list("constraint")))}))
constraintInt <- constraint * 100

#Calculate effective size with EGO
effsize <- unlist(lapply(mats, function(x){as.numeric(index.egonet(x, index = list("effsize")))}))

structure_df <- data.frame("snaw_redcap_id" = as.integer(names(max_degree)),
													 max_degree, mean_degree, density,
													 constraint, effsize, constraintInt)

sample_data <- left_join(sample_data, structure_df, by = "snaw_redcap_id")

###### Calculating Compositional Metrics #######

input_frame <- alter_frame %>% subset(snaw_redcap_id == 129)

homophily_by_product <- by(alter_frame,
	 factor(alter_frame$snaw_redcap_id, levels = unique(alter_frame$snaw_redcap_id)),
	 function(input_frame){
	 	input_id <- input_frame$snaw_redcap_id %>% unique()
	 	
	 	ego_data <- sample_data %>% subset(sample_data$snaw_redcap_id == input_id)
	 	
	 	lpg_match <- (ego_data$household_group == input_frame$lpg) %>% sum(na.rm = TRUE)
	 	caste_match <- (ego_data$caste_of_the_respondent == input_frame$caste) %>% sum(na.rm = TRUE)
	 	shg_match <- (ego_data$self_help_group == input_frame$shg) %>% sum(na.rm = TRUE)
	 	
	 	lpg_denom <- (!is.na(input_frame$lpg)) %>% sum()
	 	caste_denom <- (!is.na(input_frame$caste)) %>% sum()
	 	shg_denom <- (!is.na(input_frame$shg)) %>% sum()
	 	
	 	output_vector <- c(input_id, (lpg_match / lpg_denom),
	 		(caste_match / caste_denom), (shg_match / shg_denom))
	 	
	 	names(output_vector) <- c("snaw_redcap_id", "lpg_homophily",
	 														"caste_homophily", "shg_homophily")
	 	
	 	return(output_vector)
	 })

homophily_matrix <- matrix(data = unlist(homophily_by_product), nrow = length(homophily_by_product),
			 ncol = length(homophily_by_product[[1]]), byrow = TRUE) %>% as.data.frame()

colnames(homophily_matrix) <- c("snaw_redcap_id", "lpg_homophily",
																"caste_homophily", "shg_homophily")

sample_data <- left_join(sample_data, homophily_matrix, by = "snaw_redcap_id")

##### Participant Characteristics ########

output_dataframe <- data.frame()

lpg_stats <- sapply(c("alter_count", "density", "effsize", "constraint", "caste_homophily",
				 "lpg_homophily", "shg_homophily", "prop_lpg"), function(input_name){
				 	input_table <- sample_data %>% subset(household_group == "yes")
				 	
				 	summary_stats <- input_table[[input_name]] %>% summary()
				 	iqr_stats <- input_table[[input_name]] %>% IQR(na.rm = TRUE)
				 	sd_stats <- input_table[[input_name]] %>% sd(na.rm = TRUE)
				 	if(length(summary_stats) == 6){
				 		summary_stats <- c(summary_stats, "NA" = 0)
				 	}
				 	return(c(summary_stats, "IQR" = iqr_stats, "SD" = sd_stats))
				 }) %>% t()

non_lpg_stats <- sapply(c("alter_count", "density", "effsize", "constraint", "caste_homophily",
				 "lpg_homophily", "shg_homophily", "prop_lpg"), function(input_name){
				 	input_table <- sample_data %>% subset(household_group == "no")
				 	
				 	summary_stats <- input_table[[input_name]] %>% summary()
				 	iqr_stats <- input_table[[input_name]] %>% IQR(na.rm = TRUE)
				 	sd_stats <- input_table[[input_name]] %>% sd(na.rm = TRUE)
				 	if(length(summary_stats) == 6){
				 		summary_stats <- c(summary_stats, "NA" = 0)
				 	}
				 	return(c(summary_stats, "IQR" = iqr_stats, "SD" = sd_stats))
				 }) %>% t()

colnames(lpg_stats) <- paste0(colnames(lpg_stats), "_lpg")
colnames(non_lpg_stats) <- paste0(colnames(non_lpg_stats), "_nolpg")
rownames(lpg_stats) <- rownames(non_lpg_stats) <- c("Network Size", "Density", "Effective Size", "Constraint", "Proportion of Caste Homophily", "Proportion of LPG Homophily", "Proportion of SHG Homophily", "Proportion of LPG Adoption")

output_total <- sample_data %>% select("snaw_redcap_id", "household_group":"shg_homophily")

write.csv(output_total, file = "snaw_women_processed_data.csv")
write.csv(non_lpg_stats, file = "non_lpg_stats.csv")
write.csv(lpg_stats, file = "lpg_stats.csv")

ggplot(sample_data, aes(x = lpg_homophily)) +
	geom_histogram(bins = 10)

ggplot(sample_data, aes(x = lpg_homophily)) +
	geom_histogram(aes(fill = household_group), alpha = .5, bins = 10)

ggplot(sample_data, aes(x = lpg_homophily)) +
	geom_density(aes(fill = household_group), alpha = .5) +
	geom_vline(xintercept = median(sample_data %>%
																 	subset(household_group == "yes") %>%
																 	"$"(lpg_homophily)), color = "blue") +
	geom_vline(xintercept = median(sample_data %>%
																 	subset(household_group == "no") %>%
																 	"$"(lpg_homophily)), color = "red")

ggplot(sample_data, aes(x = prop_lpg)) +
	geom_density(aes(fill = household_group), alpha = .5) +
	geom_vline(xintercept = median(sample_data %>%
																 	subset(household_group == "yes") %>%
																 	"$"(lpg_homophily)), color = "blue") +
	geom_vline(xintercept = median(sample_data %>%
																 	subset(household_group == "no") %>%
																 	"$"(lpg_homophily)), color = "red")

sample_data$caste_of_the_respondent_2 <- sample_data$caste_of_the_respondent
levels(sample_data$caste_of_the_respondent_2) <- c("General", "SC/ST", "OBC",
																									 "OBC", NA, NA)

ggplot(sample_data, aes(x = lpg_homophily)) +
	geom_density(aes(fill = caste_of_the_respondent_2), alpha = .5)

ggplot(sample_data, aes(x = lpg_homophily)) +
	geom_histogram(bins = 8) +
	facet_wrap(~caste_of_the_respondent_2)

ggplot(sample_data, aes(x = lpg_homophily)) +
	geom_density() +
	facet_wrap(~caste_of_the_respondent_2)
