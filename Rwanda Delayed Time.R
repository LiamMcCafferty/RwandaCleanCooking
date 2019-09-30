rm(list = ls())

setwd("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/India Clean Cooking Project")

library(tidyverse)
library(readxl)
library(igraph)
library(ggnetwork)
library(gridExtra) # For montage of networks
library(grid) # For montage of networks

dataset <- read_excel("midline.xlsx")
dataset_orig <- dataset

dataset <- dataset %>% select(UID, adults_count, net_tier4plus, net_belowtier4,
															net_proptier4, tot_camp, prop_camp, resp_stovetype,
															new_all_rev)

#Factoring whether individuals are early adoptors, purchasors, or non-purchase
dataset$new_all_rev <- factor(dataset$new_all_rev,
																 levels = c(0:2),
																 labels = c("Early Adopter", "Purchaser",
																 					 "Non-Purchaser"))

dataset$resp_stovetype <- factor(dataset$resp_stovetype,
																 levels = c(2:3))

#Checking to see if we have errors in our proportions
dataset[dataset$prop_camp > 1,]
dataset[dataset$net_proptier4 > 1,]

#Clearing out data w/ errors
dataset <- dataset %>% subset(!prop_camp > 1)
dataset <- dataset %>% subset(!net_proptier4 > 1)

#Creating variable which says the number of alters who don't live in camp
dataset$tot_nocamp <- dataset$adults_count * (1 - dataset$prop_camp)

#Checking how many tier4+ and tier4- match alter_count
table((apply(dataset[,c("net_tier4plus", "net_belowtier4")], 1, sum) %>% unlist()) == dataset$adults_count)

#Looking at dataset
dataset[(apply(dataset[,c("net_tier4plus", "net_belowtier4")], 1, sum) %>% unlist()) != dataset$adults_count,] %>% view()

#Pulling out non-matches
# dataset <- dataset[(apply(dataset[,c("net_tier4plus", "net_belowtier4")], 1, sum) %>%
# 											unlist()) == dataset$adults_count,]

###### Contructing the graphs #####

i <- 1
data_row <- dataset[i,c("UID", "adults_count", "net_tier4plus", "net_belowtier4")]

alter_count <- data_row$adults_count
var1_count <- data_row$net_tier4plus
var2_count <- data_row$net_belowtier4
var1_name <- "Tier 4+"
var2_name <- "<Tier 4"
input_colors <- c("red", "blue")
uid <- data_row$UID

# # # # # # # # # # # # # # # # # # # # # # #

basic_graph <- function(alter_count, var1_count, var2_count, var1_name = "var1",
				 var2_name = "var2", input_colors = c("red", "blue"), uid = "unknown"){
	
	if(alter_count < 1){
		stop("Network Too Small")
	}
	
	if(var1_count + var2_count != alter_count){
		stop("Sum of variables not equal to alter count")
	}
	
	input_colors <- c("black", input_colors)
	coloration <- c("EGO", rep(var1_name, var1_count), rep(var2_name, var2_count))
	
	input_colors <- input_colors[c(TRUE, var1_count != 0, var2_count != 0)]
	input_colors <- input_colors[order(unique(coloration))]
	
	if(alter_count == 1){
		plot_output <- ggplot(data.frame(x = c(0.5,0.5), y = c(1,0)),
													aes(x = x, y = y)) +
			geom_point(aes(color = coloration, fill = coloration)) +
			scale_fill_manual(breaks = c("EGO", var1_name, var2_name), values = input_colors) +
			scale_color_manual(breaks = c("EGO", var1_name, var2_name), values = input_colors) +
			theme_blank() +
			#Formats the legend which describes edge weight to the reader
			theme(legend.position = "none",
						plot.title = element_text(size = 6)) +
			#Determins the margins around plot
			expand_limits(y = c(-0.1, 1.1), x = c(-0.1, 1.1)) +
			coord_fixed() +
			ggtitle(uid)
			
	}else if(alter_count > 1){
		blank_network <- matrix(nrow = alter_count + 1, ncol = alter_count + 1, data = 1) %>%
			"colnames<-"(c("EGO", paste0("Alter ", 1:alter_count))) %>%
			"rownames<-"(c("EGO", paste0("Alter ", 1:alter_count)))
		
		diag(blank_network) <- 0
		
		blank_graph <- graph.adjacency(blank_network,mode = "undirected", weighted = TRUE)
		V(blank_graph)$coloration <- coloration
		
		plot_output <- ggplot(ggnetwork(blank_graph, layout = "circle"),
													aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
			geom_nodes(aes(fill = coloration, color = coloration), shape = 21, size = 1) +
			scale_fill_manual(breaks = c("EGO", var1_name, var2_name), values = input_colors) +
			scale_color_manual(breaks = c("EGO", var1_name, var2_name), values = input_colors) +
			theme_blank() +
			#Formats the legend which describes edge weight to the reader
			theme(legend.position = "none",
						plot.title = element_text(size = 6)) +
			#Determins the margins around plot
			expand_limits(y = c(-0.1, 1.1), x = c(-0.1, 1.1)) +
			coord_fixed() +
			ggtitle(uid)
	}
	
	return(plot_output)
}

basic_graph(20,15,5)
basic_graph(2,1,1)
basic_graph(1,0,1)
# 
# input_dataset <- dataset[1:100,]
# 
# stove_tier_array <- vector("list", nrow(input_dataset))
# 
# for(i in 1:nrow(input_dataset)){
# 	print(i)
# 	data_row <- input_dataset[i,c("UID", "adults_count", "net_tier4plus", "net_belowtier4")]
# 	
# 	stove_tier_array[[i]] <- basic_graph(data_row$adults_count,
# 																			 data_row$net_tier4plus,
# 																			 data_row$net_belowtier4,
# 																			 "Tier 4+", "<Tier 4", c("red", "blue"),
# 																			 data_row$UID)
# }
# 
# 
# pdf(width = 16, height = 30, file = "Rwanda Base Montage.pdf")
# grid.arrange(grobs = stove_tier_array, ncol = 13)
# dev.off()

# 
# camp_array <- vector("list", nrow(input_dataset))
# 
# for(i in 1:nrow(input_dataset)){
# 	print(i)
# 	data_row <- input_dataset[i,c("UID", "adults_count", "tot_camp", "tot_nocamp")]
# 	
# 	camp_array[[i]] <- basic_graph(data_row$adults_count,
# 																			 data_row$tot_camp,
# 																			 data_row$tot_nocamp,
# 																			 "Camp", "NoCamp", c("purple", "green"),
# 																			 data_row$UID)
# }
# 
# 
# pdf(width = 10, height = 10, file = "Rwanda Camp Montage.pdf")
# grid.arrange(grobs = camp_array, ncol = 10)
# dev.off()


dataset_early <- dataset %>% subset(new_all_rev == "Early Adopter")

camp_early_array <- vector("list", nrow(dataset_early))

for(i in 1:nrow(dataset_early)){
	print(i)
	data_row <- dataset_early[i,c("UID", "adults_count", "tot_camp", "tot_nocamp")]
	
	camp_early_array[[i]] <- basic_graph(data_row$adults_count,
																 data_row$tot_camp,
																 data_row$tot_nocamp,
																 "Camp", "NoCamp", c("purple", "green"),
																 data_row$UID)
}


pdf(width = 10, height = 30, file = "Rwanda Camp Early Montage.pdf")
grid.arrange(grobs = camp_early_array, ncol = 10)
dev.off()



dataset_purch <- dataset %>% subset(new_all_rev == "Purchaser")

camp_purch_array <- vector("list", nrow(dataset_purch))

for(i in 1:nrow(dataset_purch)){
	print(i)
	data_row <- dataset_purch[i,c("UID", "adults_count", "tot_camp", "tot_nocamp")]
	
	camp_purch_array[[i]] <- basic_graph(data_row$adults_count,
																			 data_row$tot_camp,
																			 data_row$tot_nocamp,
																			 "Camp", "NoCamp", c("purple", "green"),
																			 data_row$UID)
}


pdf(width = 10, height = 50, file = "Rwanda Camp Purchase Montage.pdf")
grid.arrange(grobs = camp_purch_array, ncol = 10)
dev.off()



dataset_no_purch <- dataset %>% subset(new_all_rev == "Non-Purchaser")

camp_nopurch_array <- vector("list", nrow(dataset_no_purch))

for(i in 1:nrow(dataset_no_purch)){
	print(i)
	data_row <- dataset_no_purch[i,c("UID", "adults_count", "tot_camp", "tot_nocamp")]
	
	camp_nopurch_array[[i]] <- basic_graph(data_row$adults_count,
																			 data_row$tot_camp,
																			 data_row$tot_nocamp,
																			 "Camp", "NoCamp", c("purple", "green"),
																			 data_row$UID)
}


pdf(width = 10, height = 40, file = "Rwanda Camp NoPurchase Montage.pdf")
grid.arrange(grobs = camp_nopurch_array, ncol = 10)
dev.off()



ggplot(dataset, aes(x = prop_camp)) +
	geom_density(aes(color = new_all_rev))

kruskal.test(prop_camp ~ new_all_rev, data = dataset)

ggplot(dataset, aes(y = tot_camp, x = new_all_rev)) +
	geom_boxplot()

kruskal.test(tot_camp ~ new_all_rev, data = dataset)

ggplot(dataset, aes(y = adults_count, x = new_all_rev)) +
	geom_boxplot()

kruskal.test(adults_count ~ new_all_rev, data = dataset)

ggplot(dataset, aes(y = adults_count, x = new_all_rev)) +
	geom_boxplot()

kruskal.test(adults_count ~ new_all_rev, data = dataset)

ggplot(dataset, aes(y = adults_count, x = resp_stovetype)) +
	geom_boxplot()

kruskal.test(adults_count ~ resp_stovetype, data = dataset)

ggplot(dataset, aes(y = tot_camp, x = resp_stovetype)) +
	geom_boxplot()

kruskal.test(tot_camp ~ resp_stovetype, data = dataset)

ggplot(dataset, aes(y = prop_camp, x = resp_stovetype)) +
	geom_boxplot()

kruskal.test(prop_camp ~ resp_stovetype, data = dataset)
