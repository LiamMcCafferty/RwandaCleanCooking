setwd("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/India Clean Cooking Project")
rm(list = ls())


#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each
#  library()
library(tidyverse) # For data management
library(igraph) # To transform and analyze network data
library(ggnetwork) # To make good-looking network graphs

                      #E,1,2,3,4,5,6
example_mat <- rbind(c(0,2,2,2,2,2,2),#E
										 c(2,0,2,2,2,2,2),#1
										 c(2,2,0,2,2,2,2),#2
										 c(2,2,2,0,2,2,2),#3
										 c(2,2,2,2,0,2,2),#4
										 c(2,2,2,2,2,0,2),#5
										 c(2,2,2,2,2,2,0))#6

colnames(example_mat) <- rownames(example_mat) <- c("EGO", 1:6)

example_graph <- graph.adjacency(example_mat, weighted = TRUE, mode = "undirected")
plot(example_graph)

V(example_graph)$lpg <- c("LPG", "LPG", "LPG", "LPG", "LPG", "LPG", "No LPG")
##   No       Yes     Other
c("green", "grey40", "black")

example_plot <- ggplot(ggnetwork(example_graph, layout = "circle"), aes(x = x, y = y, xend = xend, yend = yend)) +
	geom_edges(aes(color = factor(weight, levels = c(1,2)))) +
	geom_nodes(aes(shape = ifelse(vertex.names == "EGO", "Ego", "Alter"),
								 fill = lpg), size = 8) +
  scale_fill_manual(breaks = c("LPG", "No LPG"), values = c("green", "grey40"), 
  									name = "Stove Ownership") +
	scale_color_manual(breaks = c("1", "2"), values = c("red", "blue"),
										 labels = c("Weak Tie", "Strong Tie"), name = "Tie Strength") +
	scale_shape_manual(breaks = c("Ego", "Alter"), values = c(21, 24), name = "Node Type") +
	theme_blank() +
	guides(fill = guide_legend(override.aes = list(shape = 21))) +
	#Formats the legend which describes edge weight to the reader
	theme(legend.position = "bottom", #format the legend 
				legend.title = element_text(face = "bold", size = 15),
				legend.text = element_text(size = 10)) + 
	theme(legend.title.align = 0.5) + 
	theme(plot.title = element_text(size = 18, face = "bold")) +
	theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
	#Formatting for legend's keys
	theme(legend.direction = 'vertical',
				legend.key.height = unit(1, "line"),
				legend.key = element_rect(colour = NA, fill = NA)) +
	coord_equal()
	
pdf(file = "example plot LPG owner color LPG.pdf")
example_plot
dev.off()
