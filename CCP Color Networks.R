
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
					select(ego_data_add, "snaw_redcap_id" = "record_id", "household_group", "caste_of_the_respondent"),
					by = "snaw_redcap_id")

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

#Function which makes Social Network Image 
make_image <- function(x) {
	##########
	# Function: Creates and outputs a network graph with nodes named and ties colored
	# Inputs: x = input dataset with 1 row
	# Ouputs: plot1, a single network graph
	##########
	
	#transform data to dataframe-table
	x <- tbl_df(x)
	
	#Creates a network matrix from input dataset file
	mat <- make_base_mat(x)
	
	#Saves important values for determining graph formatting
	ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
	colors  <- c("blue", "red") #create color palette for ties
	ego_col <- ifelse(V(ego4.g)$name == "You", "grey17", "white") 
	
	
	
	#Saves logic to determine the strength of ties between nodes
	weight.ego <- sapply(E(ego4.g)$weight, function(yk){
		if(is.na(yk)){
			return("Unknown")
		}else if(yk == 1){
			return("Weak Tie")
		}else{
			return("Strong Tie")
		}
	})
	
	
	if ("Unknown" %in% weight.ego ){
		#Error check to see if network has sufficient ties, will output a blank graph with
		#  error message.
		print("Error: Some networks ties are unknown ")
		plot1 <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) +
			geom_blank() + ggtitle("Data doesn't work: some network ties are unknown")
		
	}else if(nrow(mat) < 2){
		
		#Setting the factors which determine the color and linetype of the single tie.
		two_tie_strength <- factor("Strong Tie", levels = c("Strong Tie", "Weak Tie"))
		two_tie_opposite <- factor("Weak Tie", levels = c("Strong Tie", "Weak Tie"))
		
		df2 <- data.frame(x=c(5,5), y=c(5,5))
		
		plot1 <- ggplot(df2, aes(x,y)) +
			geom_point(fill=c("black"), shape = 21, size = 14) +
			geom_nodelabel(label = c("You")) +
			geom_line(aes(color = two_tie_strength, linetype = two_tie_strength), size = 2, alpha = 1) +
			geom_line(aes(color = two_tie_opposite, linetype = two_tie_opposite), size = 2, alpha = 1) +
			theme_blank() +
			#Formats the legend which describes edge weight to the reader
			theme(legend.position = "bottom", #format the legend 
						legend.title = element_text(face = "bold", size = 15),
						legend.text = element_text(size = 10)) + 
			theme(legend.title.align = 0.5) + 
			theme(plot.title = element_text(size = 18, face = "bold")) +
			scale_colour_manual(name = "Tie Strength", values = c("red", "blue"))+
			scale_shape_manual(name = "Tie Strength", values = c(22, 21)) +
			scale_linetype_manual(name = "Tie Strength", values = c("solid", "dashed")) +
			#Determins the margins around plot
			theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
			#Formatting for legend's keys
			theme(legend.direction = 'vertical',
						legend.key.height = unit(1, "line"),
						legend.key = element_rect(colour = NA, fill = NA))
	}else if(nrow(mat) == 2){
		#Note that this network graph is not technically created from our network
		#  data as the ggnet package cannot process networks of 2 or less nodes. Thus
		#  we have to "fake" it using a self-actuated graph.
		
		#Setting the factors which determine the color and linetype of the single tie.
		two_tie_strength <- factor(ifelse(mat[1,2] == 2, "Strong Tie", "Weak Tie"),
															 levels = c("Strong Tie", "Weak Tie"))
		two_tie_opposite <- factor(ifelse(mat[1,2] == 1, "Strong Tie", "Weak Tie"),
															 levels = c("Strong Tie", "Weak Tie"))
		
		#Generating locations for the two nodes
		df1 <- data.frame(x=c(10,5), y=c(10,5))
		
		#Creates actual network graph
		plot1 <- ggplot(df1, aes(x,y)) + 
			geom_line(aes(color = two_tie_opposite, linetype = two_tie_opposite), size = 2, alpha = 0) +
			geom_line(aes(color = two_tie_strength, linetype = two_tie_strength), size = 2) +
			geom_point(fill=c("black", "white"), shape = 21, size = 14) + 
			geom_nodelabel(label = c(rownames(mat))) +
			theme_blank() +
			#Formats the legend which describes edge weight to the reader
			theme(legend.position = "bottom", #format the legend 
						legend.title = element_text(face = "bold", size = 15),
						legend.text = element_text(size = 10)) + 
			theme(legend.title.align = 0.5) + 
			theme(plot.title = element_text(size = 18, face = "bold")) +
			scale_colour_manual(name = "Tie Strength", values = c("red", "blue"))+
			scale_shape_manual(name = "Tie Strength", values = c(22, 21)) +
			scale_linetype_manual(name = "Tie Strength", values = c("solid", "dashed")) +
			#Determins the margins around plot
			theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
			#Formatting for legend's keys
			theme(legend.direction = 'vertical',
						legend.key.height = unit(1, "line"),
						legend.key = element_rect(colour = NA, fill = NA))
	}else{
		
		V(ego4.g)$coloration <- colors_nodes
		E(ego4.g)$weight <- weight.ego
		#Creates actual network graph
		plot1 <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
			#Determines coloration and style of network edges
			geom_edges(aes(linetype = as.factor(weight), color = (weight)), curvature = 0.1) +
			#Fills nodes with ego_color pallate
			geom_nodes(aes(fill = coloration), size = 14, shape = 21) +
			#Names each node with alter names
			geom_nodelabel(label = rownames(mat)) +
			theme_blank() +
			#Formats the legend which describes edge weight to the reader
			theme(legend.position = "bottom", #format the legend 
						legend.title = element_text(face = "bold", size = 15),
						legend.text = element_text(size = 10)) + 
			theme(legend.title.align = 0.5) + 
			theme(plot.title = element_text(size = 18, face = "bold")) +
			scale_colour_manual(name = "Tie Strength", values = c("red", "blue"))+
			scale_shape_manual(name = "Tie Strength", values = c(22, 21)) +
			scale_linetype_manual(name = "Tie Strength", values = c("solid", "dashed")) +
			#Determins the margins around plot
			theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
			#Formatting for legend's keys
			theme(legend.direction = 'vertical',
						legend.key.height = unit(1, "line"),
						legend.key = element_rect(colour = NA, fill = NA))
	}
	return(plot1)
}

#I'll lay out what I want to try to let this function do:

#1: if the person has the exact name of the variable they want, they can simply
#plug it in and it will generate a network graph which is colored by this.
# - I guess if they input a second one we can do shape, a third would be tough.
# - Name needs to accept following by 1's or none

#2: If the person has the general name of the variable they want they can enter
#it in and it does the same thing as 1
# - Problem with this is I can easily come up with some reasonable names for
#     each variable that we have currently within alter_data. However, the point of
#     this code was to make it so someone could add their own variables to
#     alter_data pretty easily. Unfortunately for us that means that I can't just
#     make a reasonable name for variables I don't know the name of. This could just
#     be a feature that only works with my current alter_data.

#3: If a person has the name of a categorical variable, they can then define
#which levels which they want to be different colors.
# - As well, I guess I can provide an option to self-actualize the colors and the
#     names of the groups. Default colors are just what ggplot does, default
#     group-names will be a pasted together kronenburg of the items in the given list.

#Setting up x or input dataset
x <- sample_data[37,]

#### Getting our datasets together ####

#Creates a network matrix from input dataset file
mat <- make_base_mat(x)
#Creates alters of our selected
alter_bench <- alter_data %>% subset(snaw_redcap_id == x$snaw_redcap_id)
#Acess processed sample_data from ego_data
ego_data <- sample_data %>% subset(snaw_redcap_id == x$snaw_redcap_id)

#### Setting up alter_bench ####

#Ordering alter bench in case it is not in the same order as the names within the matrix mat.
if(all(colnames(mat)[-1] %in% alter_bench$alter_names)){
	alter_bench <- alter_bench[match(colnames(mat)[-1], alter_bench$alter_names),]
}

#### Figuring out which variables w/in alter_bench are categorical, checkbox, or continuous ####

#Constructing a dictionary of the levels of each variable within the alter
#bench. We do this so that we can still use newly added variables to alter_data
alter_levels <- sapply(alter_bench, levels)

#Checks if the variables are checkboxes. This is true if they are duplicated (as
#checkbox defaults to having duplicates). As well, they are not null
is_checkbox <- alter_levels %>% duplicated(fromLast = TRUE) &
	duplicated(sub("[1-9]$", "", names(alter_levels)), fromLast = TRUE) |
	alter_levels %>% duplicated() & duplicated(sub("[1-9]$", "", names(alter_levels))) &
	!sapply(alter_levels, is.null)

#If it does not have levels then it is continuous (unless coded wrong on the alter_data side)
is_continuous <- sapply(alter_levels, is.null)

#Everything that isn't checkbox or continuous is categorical, so there's our answer.
is_categorical <- !is_checkbox & !is_continuous

#### Block ####

#Where we assign the two variables we select
alter_select <- "lpg"
ego_select <- "household_group"
category_title <- "LPG"
graph_colors <- "none"
color_labels <- "none"
data_row <- sample_data[73,]
alter_base = alter_data
data_base = sample_data

color_graphs <- function(data_row, alter_select, ego_select = "ego", category_title = "none",
												 graph_colors = "none", color_labels = "none",
												 alter_base = alter_data, data_base = sample_data, no_legend = FALSE
){
	
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
	
	#### Figuring out which variables w/in alter_bench are categorical, checkbox, or continuous ####
	
	#Constructing a dictionary of the levels of each variable within the alter
	#bench. We do this so that we can still use newly added variables to alter_data
	alter_levels <- sapply(alter_bench, levels)
	
	#Checks if the variables are checkboxes. This is true if they are duplicated (as
	#checkbox defaults to having duplicates). As well, they are not null
	is_checkbox <- alter_levels %>% duplicated(fromLast = TRUE) &
		duplicated(sub("[1-9]$", "", names(alter_levels)), fromLast = TRUE) |
		alter_levels %>% duplicated() & duplicated(sub("[1-9]$", "", names(alter_levels))) &
		!sapply(alter_levels, is.null)
	
	#If it does not have levels then it is continuous (unless coded wrong on the alter_base side)
	is_continuous <- sapply(alter_levels, is.null)
	
	#Everything that isn't checkbox or continuous is categorical, so there's our answer.
	is_categorical <- !is_checkbox & !is_continuous
	
	
	#Confirms if the checkbox is present within, and adds in all the necessary numbers
	if(any(paste0(alter_select, c(1:10000)) %in% names(alter_levels))){
		alter_select <- paste0(alter_select, c(1:10000))[paste0(alter_select, c(1:10000)) %in% names(alter_levels)]
	}
	
	#Logic basis for when we do different actions depending upon if it is a checkbox/continuous/categorical
	if(all(alter_select %in% names(alter_levels)[is_checkbox]) &
		 #This second part is if the checkbox's 2nd set of values are all NA's, then
		 #  it is effectively a categorical vairable and thus we skip to the next one.
		 !alter_bench[alter_select][,-1] %>% is.na() %>% all()){
		print("Checkbox Graph")
		graph_type <- "checkbox"
		print(alter_select)
	}else if(any(alter_select %in% names(alter_levels)[is_categorical]) | length(alter_select) > 1){
		print("Categorical Graph")
		graph_type <- "categorical"
		alter_select <- alter_select[1]
		print(alter_select)
	}else if(any(alter_select %in% names(alter_levels)[is_continuous])){
		#Note that the length > 1 part is to capture checkboxes that had NA's in all other columns.
		if(length(alter_select) > 1){
			alter_select <- alter_select[1]
		}
		print("Continuous Graph")
		graph_type <- "continuous"
		print(alter_select)
	}else{
		print("WHUT")
		graph_type <- "WHUT"
	}
	
	#Setting the title of the category. If set to default "none" then it takes on
	#  the name of "alter_select", otherwise if someone has entered in some sort of
	#  title it will keep that title.
	category_title <- ifelse(category_title == "none", alter_select, category_title)
	
	if(graph_type == "categorical"){
		########## Categorical Graph Output #########
		
		#Accessing values assigned to each category for a categorical variable
		colors_nodes <- alter_bench[[alter_select]]
		old_levels <- levels(colors_nodes)
		
		if((color_labels != "none")[1] & length(color_labels) == length(levels(colors_nodes))){
			levels(colors_nodes) <- color_labels
		}else if((color_labels != "none")[1] & length(color_labels) != length(levels(colors_nodes))){
			stop("Input color_nodes not equal length to levels of selected category")
		}
		
		if(!ego_select %in% c("Ego", "ego", "You", "you") & ego_select %in% colnames(ego_data)){
			ego_selected <- ego_data[[ego_select]]
			ego_selected <- old_levels[old_levels %in% ego_selected]
		}else{
			ego_selected <- "Ego"
		}
		
		base_levels <- levels(colors_nodes)
		
		if(ego_select == "Ego" | ego_select == "ego" | ego_select == "You" | ego_select == "you" |
			 !ego_select %in% colnames(ego_data)){
			
			colors_nodes <- c("Ego", levels(colors_nodes)[colors_nodes])
			base_levels <- c(base_levels, "Ego")
			
			colors_nodes <- factor(colors_nodes, levels = base_levels)
		}else{
			colors_nodes <- factor(c(ego_selected, levels(colors_nodes)[colors_nodes]), levels = base_levels)
		}
		
		
		#Saves important values for determining graph formatting
		ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
		colors  <- c("blue", "red") #create color palette for ties
		vertex_attr(ego4.g, "coloration") <- levels(colors_nodes)[colors_nodes]
		
		#Saves logic to determine the strength of ties between nodes
		weight.ego <- sapply(E(ego4.g)$weight, function(yk){
			if(is.na(yk)){
				return("Unknown")
			}else if(yk == 1){
				return("Weak Tie")
			}else{
				return("Strong Tie")
			}
		})
		
		if(is.null(category_title)){
			#We just do the base scale_fill_discrete
			category_title
		}else if(!is.null(category_title) & is.character(category_title) & length(category_title) == 1){
			category_title
			#We do the scale_fill_discrete with category_title as the name
		}else{
			stop("category_title is not a length 1 character vector")
		}
		
		
		weight_type <- factor(E(ego4.g)$weight, levels = c(1,2), labels = c("Weak Tie", "Strong Tie"))
		E(ego4.g)$weight_type <- levels(weight_type)[weight_type]
		
		# Need to work on this section to try to get weights to be more consistent. I should probably figure out how to copy it from our base script.
		
		plot_output <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
			#Determines coloration and style of network edges
			geom_edges(aes(linetype = weight_type, color = weight_type), curvature = 0.1) +
			#Fills nodes with ego_color pallate
			geom_nodes(aes(fill = factor(coloration, levels = base_levels)), size = 14, shape = 21) +
			#Names each node with alter names
			geom_nodelabel(label = rownames(mat)) +
			theme_blank() +
			#Formats the legend which describes edge weight to the reader
			theme(legend.position = "bottom", #format the legend 
						legend.title = element_text(face = "bold", size = 15),
						legend.text = element_text(size = 10)) + 
			theme(legend.title.align = 0.5) + 
			theme(plot.title = element_text(size = 18, face = "bold")) +
			scale_colour_manual(name = "Tie Strength", values = c("red", "blue")) +
			scale_shape_manual(name = "Tie Strength", values = c(22, 21)) +
			scale_linetype_manual(name = "Tie Strength", values = c("solid", "dashed")) +
			guides(fill = guide_legend(override.aes = list(size=5))) +
			#Determins the margins around plot
			theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
			#Formatting for legend's keys
			theme(legend.direction = 'vertical',
						legend.key.height = unit(1, "line"),
						legend.key = element_rect(colour = NA, fill = NA)) +
			expand_limits(y = c(-0.1, 1.1), x = c(-0.1, 1.1))
		
		if(all(graph_colors != "none") & length(graph_colors) != length(base_levels)){
			stop(paste0("Incorrect number of colors in variable graph_colors, color",
									" count needs to match levels found in dataframe alter_base.",
									" Entered color count is: ", length(graph_colors),
									". Number of levels is: ", length(base_levels),
									ifelse(ego_selected == "Ego", " Note that Ego is considered the final color", "")))
		}
		
		if(all(graph_colors != "none")){
			plot_output <- plot_output + scale_fill_manual(name = category_title, values = graph_colors)
		}else{
			plot_output <- plot_output + scale_fill_discrete(name = category_title)
		}
		
		if(no_legend == TRUE){
			plot_output <- plot_output +
				guides(fill = FALSE, color = FALSE, shape = FALSE, linetype = FALSE)
		}
		
	}else if(graph_type == "continuous"){
		######### Continuous Graph Output ##########
		
		if(length(graph_colors) != 2 & all(graph_colors != "none")){
			stop(paste0("Incorrect length for graph_colors for continuous variables. Given color count is: ",
									length(graph_colors), ". Color count needs to be length: 2"))
		}else if(all(graph_colors == "none")){
			graph_colors <- c("red", "blue")
		}
		
		
		colors_continuous <- alter_bench[[alter_select]]
		if(ego_select == "Ego" | ego_select == "ego" | ego_select == "You" |
			 ego_select == "you" | !ego_select %in% colnames(ego_data)){
			colors_continuous <- c(NA, colors_continuous)
		}else{
			colors_continuous <- c(ego_data[[ego_select]], colors_continuous)
		}
		
		#Saves important values for determining graph formatting
		ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
		colors  <- c("blue", "red") #create color palette for ties
		V(ego4.g)$cont_value <- colors_continuous
		
		weight.ego <- sapply(E(ego4.g)$weight, function(yk){
			if(is.na(yk)){
				return("Unknown")
			}else if(yk == 1){
				return("Weak Tie")
			}else{
				return("Strong Tie")
			}
		})
		
		weight_type <- factor(E(ego4.g)$weight, levels = c(1,2), labels = c("Weak Tie", "Strong Tie"))
		E(ego4.g)$weight_type <- levels(weight_type)[weight_type]
		
		plot_output <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
			#Determines coloration and style of network edges
			geom_edges(aes(linetype = weight_type, color = weight_type), curvature = 0.1) +
			#Fills nodes with ego_color pallate
			geom_nodes(aes(fill = cont_value), size = 14, shape = 21) +
			#Names each node with alter names
			geom_nodelabel(label = rownames(mat)) +
			theme_blank() +
			#Formats the legend which describes edge weight to the reader
			theme(legend.position = "bottom", #format the legend 
						legend.title = element_text(face = "bold", size = 15),
						legend.text = element_text(size = 10)) + 
			theme(legend.title.align = 0.5) + 
			theme(plot.title = element_text(size = 18, face = "bold")) +
			scale_colour_manual(name = "Tie Strength", values = c("red", "blue")) +
			scale_shape_manual(name = "Tie Strength", values = c(22, 21)) +
			scale_linetype_manual(name = "Tie Strength", values = c("solid", "dashed")) +
			scale_fill_gradient(name = category_title, low = graph_colors[1], high = graph_colors[2]) +
			guides(fill = guide_legend(override.aes = list(size=5))) +
			#Determins the margins around plot
			theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
			#Formatting for legend's keys
			theme(legend.direction = 'vertical',
						legend.key.height = unit(1, "line"),
						legend.key = element_rect(colour = NA, fill = NA)) +
			expand_limits(y = c(-0.1, 1.1), x = c(-0.1, 1.1))
	}else if(graph_type == "checkbox"){
		####### Checkbox Graph Output ##########
		
		#Experimenting w/ making pie charts for color labels.
		library(scatterpie)
		
		#Constructing igraph object used to make network
		ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
		
		weight.ego <- sapply(E(ego4.g)$weight, function(yk){
			if(is.na(yk)){
				return("Unknown")
			}else if(yk == 1){
				return("Weak Tie")
			}else{
				return("Strong Tie")
			}
		})
		
		#
		weight_type <- factor(E(ego4.g)$weight, levels = c(1,2), labels = c("Weak Tie", "Strong Tie"))
		E(ego4.g)$weight_type <- levels(weight_type)[weight_type]
		
		plot1 <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
			#Determines coloration and style of network edges
			geom_edges(aes(linetype = weight_type, color = weight_type), curvature = 0.1) +
			#Fills nodes with ego_color pallate
			geom_nodes(size = 14, shape = 21) +
			#Names each node with alter names
			geom_nodelabel(label = rownames(mat))
		
		
		plotting <- ggplot_build(plot1)$data[[2]] %>% select(x, y)
		plotting$alter_names <- ggplot_build(plot1)$data[[3]]$label
		
		checkbox_names <- alter_select
		
		plotting <- left_join(plotting, alter_bench %>% select("alter_names", checkbox_names), by = "alter_names")
		
		plotting_levels <- plotting[[checkbox_names[1]]] %>% levels()
		
		pie_fractions <- plotting %>% select(checkbox_names) %>% apply(1, function(x, plotting_levels){
			level_counts <- length(x[!is.na(x)])
			level_fraction <- 1 / ifelse(level_counts > 0, level_counts, 1)
			level_holder <- rep(x = 0, times = length(plotting_levels))
			names(level_holder) <- plotting_levels
			
			for(i in 1:length(plotting_levels)){
				level_holder[i] <- ifelse(plotting_levels[i] %in% x, level_fraction, 0)
			}
			
			if(sum(level_holder) == 0){
				level_holder[length(level_holder) + 1] <- 1
				names(level_holder) <- c(names(level_holder)[-(length(level_holder))], "Ego")
			}else{
				level_holder[length(level_holder) + 1] <- 0
				names(level_holder) <- c(names(level_holder)[-(length(level_holder))], "Ego")
			}
			return(level_holder)
		}, plotting_levels = plotting_levels)
		
		pie_fractions <- pie_fractions %>% as.data.frame() %>% t()
		rownames(pie_fractions) <- NULL
		
		plotting <- cbind(plotting, pie_fractions)
		plotting$radius <- .07
		
		# ggplot() + geom_scatterpie(aes(x = x, y = y, r = radius), data = plotting,
		#                            cols = plotting_levels)
		
		if(ego_select %in% colnames(ego_data)){
			plotting[1,alter_select] <- levels(ego_data[[ego_select]])[ego_data[[ego_select]]]
		}
		
		lining <- ggplot_build(plot1)$data[[1]]
		lining$colour <- factor(lining$colour, levels = c("#00BFC4", "#F8766D"),
														labels = c("Weak Tie", "Strong Tie"))
		
		plot_output <- ggplot(lining, aes(x = x, y = y, xend = xend, yend = yend, color = colour, linetype = colour)) +
			geom_curve(curvature = 0.1) +
			scale_colour_manual(name = "Tie Strength", values = c("blue", "red")) +
			scale_linetype_manual(name = "Tie Strength", values = c("dashed", "solid")) +
			geom_scatterpie(aes(x = x, y = y, r = radius), data = plotting, cols = c(plotting_levels, "Ego")) +
			# scale_fill_discrete(breaks = c(plotting_levels, "NA"),
			#                   #values = c("red", "blue", "green", "purple", "orange", "brown"),
			#                   name = category_title) +
			theme_blank() +
			theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
			#Formatting for legend's keys
			theme(legend.direction = 'vertical',
						legend.key.height = unit(1, "line"),
						legend.key = element_rect(colour = NA, fill = NA)) +
			theme(legend.position = "bottom", #format the legend 
						legend.title = element_text(face = "bold", size = 15),
						legend.text = element_text(size = 10)) + 
			theme(legend.title.align = 0.5) + 
			theme(plot.title = element_text(size = 18, face = "bold")) +
			geom_label(data = plotting, aes(x = x, y = y, label=alter_names), inherit.aes = FALSE) +
			coord_equal()
		
		
		if(color_labels[1] != "none" & length(color_labels) != length(c(plotting_levels, "Ego"))){
			stop(paste0("Color count not equal to number of categories. Given count: ", length(color_labels),
									". Category count: ", length(c(plotting_levels, "Ego"))))
		}else if(color_labels[1] != "none" & length(color_labels) == length(c(plotting_levels, "Ego"))){
			plot_output <- plot_output  +
				scale_fill_manual(breaks = c(plotting_levels, "NA"),
													values = color_labels,
													name = category_title)
		}else{
			plot_output <- plot_output  +
				scale_fill_discrete(breaks = c(plotting_levels, "Ego"), name = category_title)
		}
	}
	
	plot_output %>% return()
}

color_graphs(sample_data[22,], "caste", "caste_of_the_respondent", category_title = "Caste")



