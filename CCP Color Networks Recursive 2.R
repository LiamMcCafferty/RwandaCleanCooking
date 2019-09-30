

setwd("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/India Clean Cooking Project")
rm(list = ls())


#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each
#  library()
library(tidyverse) # For data management
library(igraph) # To transform and analyze network data
library(ggnetwork) # To make good-looking network graphs
library(scales) # To add percentages
library(gridExtra) # For montage of networks
library(grid) # For montage of networks

#Read in data
#Imports data and assigns it to variable "sample_data"
sample_data <- read.csv("SNA data_women.csv", 
												stringsAsFactors = FALSE)

load("alter_data.rda")
alter_data <- alter_frame

sample_data$alter_count <- sample_data %>%
	select(snaw_g7_name1:snaw_g7_name20) %>% "!="("") %>% apply(1, sum)

sample_data <- sample_data %>% subset(alter_count > 1)

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

processed_data <- read.csv("snaw_women_processed_data.csv", stringsAsFactors = FALSE)

sample_data <- left_join(sample_data,
												 processed_data %>% select(snaw_redcap_id, prop_lpg:shg_homophily),
												 by = "snaw_redcap_id")

sample_data <- sample_data[order(sample_data$lpg_homophily),]

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


alter_select <- "caste"
ego_select <- "caste_of_the_respondent"
graph_colors <- c("red", "blue", "green", "yellow", "purple", "black")
data_row <- sample_data[sample_data$snaw_redcap_id == 308,]
alter_base = alter_frame
data_base = sample_data
ego_select_shape <- "household_group"
alter_select_shape <- "lpg"
graph_shape <- c("21", "24", "22")




category_plot_basic <- function(data_row, alter_select, alter_select_shape,
																ego_select, ego_select_shape,
																graph_colors, graph_shape,
																alter_base = alter_frame, data_base = sample_data){
	#Creates a network matrix from input dataset file
	mat <- make_base_mat(data_row)
	
	if(nrow(mat) < 3){
		stop("Network is too small for function to construct network graph")
	}
	
	#Creates alters of our selected
	alter_bench <- alter_base %>% subset(snaw_redcap_id == data_row$snaw_redcap_id)
	#Acess processed data_base from ego_data
	ego_data <- data_base %>% subset(snaw_redcap_id == data_row$snaw_redcap_id)
	
	#### Setting up alter_bench ####
	
	#Ordering alter bench in case it is not in the same order as the names within the matrix mat.
	if(all(colnames(mat)[-1] %in% alter_bench$alter_names)){
		alter_bench <- alter_bench[match(colnames(mat)[-1], alter_bench$alter_names),]
	}
	
	########## Categorical Graph Output #########
	
	#Accessing values assigned to each category for a categorical variable
	# colors_nodes <- alter_bench[[alter_select]]
	# older_levels <- levels(colors_nodes)
	# old_levels <- c(levels(colors_nodes), "NA")
	# colors_nodes <- addNA(colors_nodes)
	# levels(colors_nodes) <- c(older_levels, "NA")
	# colors_nodes <- factor(c(levels(ego_data[[ego_select]])[ego_data[[ego_select]]], levels(colors_nodes)[colors_nodes]), levels = old_levels)
	
	#Accessing variables storing color values
	if(any(!levels(ego_data[[ego_select]]) %in% levels(alter_bench[[alter_select]]))){
		stop("Levels for Alter and Ego selected variables do not match, make them match first : COLOR/FILL")
	}else{
		color_nodes <- c(levels(ego_data[[ego_select]])[ego_data[[ego_select]][]],
										 levels(alter_bench[[alter_select]])[alter_bench[[alter_select]]]
		)
	}
	
	#Accessing variables storing shape values
	if(any(!levels(ego_data[[ego_select_shape]]) %in% levels(alter_bench[[alter_select_shape]]))){
		stop("Levels for Alter and Ego selected variables do not match, make them match first : SHAPE")
	}else{
		shape_nodes <- c(levels(ego_data[[ego_select_shape]])[ego_data[[ego_select_shape]][]],
										 levels(alter_bench[[alter_select_shape]])[alter_bench[[alter_select_shape]]]
		)
	}

	#Saving the old levels
	old_levels <- levels(ego_data[[ego_select]])
	old_levels_shape <- levels(ego_data[[ego_select]])

	#Saves important values for determining graph formatting
	ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
	vertex_attr(ego4.g, "coloration") <- color_nodes
	vertex_attr(ego4.g, "shaper") <- shape_nodes
	
	
	#Deletes colors that are not being used due to no data assigned to it
	graph_colors <- graph_colors[sapply(old_levels, function(old_level_itr){
		return(old_level_itr %in% color_nodes)
	})]
	
	graph_shape <- graph_shape[sapply(old_levels_shape, function(old_level_itr){
		return(old_level_itr %in% shape_nodes)
	})]
	
	V(ego4.g)$sizer <- ifelse(V(ego4.g)$name == "EGO", "3", "2")
	
	#Assigns fill, makes it so all white inside has black outside otherwise it won't be visible
	graph_fill <- sub("white","black",graph_colors)
	

	#Added in ggnetwork, which just allows us to customize the shape of the graph.
	plot_output <- ggplot(ggnetwork(ego4.g, layout = "circle"),
												aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
		#Determines coloration and style of network edges
		#geom_edges(aes(linetype = weight_type), curvature = 0.1, size = .4) +
		#Fills nodes with ego_color pallate
		geom_nodes(aes(fill = factor(coloration, levels = old_levels),
									 color = factor(coloration, levels = old_levels),
									 shape = shaper, size = sizer)) +
		#Names each node with alter names
		#geom_nodelabel(label = rownames(mat)) +
		theme_blank() +
		#Formats the legend which describes edge weight to the reader
		theme(legend.position = "none") +
		#Determins the margins around plot
		expand_limits(y = c(-0.1, 1.1), x = c(-0.1, 1.1)) +
		scale_size_manual(breaks = c("3", "2"), values = c(2, 3)) +
		scale_shape_manual(breaks = c("24", "22", "21"), values = c(24, 22, 21)) +
		scale_fill_manual(values = graph_colors) +
		scale_color_manual(values = graph_fill) +
		#ggtitle(label = paste(data_row$study_id, substr(data_row$race1, 1, 1))) +
		coord_fixed()
	
	return(plot_output)
}


category_plot_basic(data_row = sample_data[197,],
										alter_select = "caste",ego_select = "caste_of_the_respondent",
										alter_select_shape = "lpg", ego_select_shape = "household_group",
										graph_colors = c("red", "navyblue", "purple", "brown", "magenta", "black"),
										graph_shape = c("21", "24", "22"), 
										)

plot_array <- vector("list", nrow(sample_data))

for(i in 1:nrow(sample_data)){
	plot_array[[i]] <- category_plot_basic(data_row = sample_data[i,],
																				 alter_select = "caste",ego_select = "caste_of_the_respondent",
																				 alter_select_shape = "lpg", ego_select_shape = "household_group",
																				 graph_colors = c("red", "navyblue", "purple",
																				 								 "brown", "magenta", "black"),
																				 graph_shape = c("21", "24", "22"), 
																				 )
}


pdf(width = 16, height = 30, file = "Network Montage by Caste and LPG.pdf")
grid.arrange(grobs = plot_array, ncol = 13)
dev.off()

