# Empties Global Environment cache
rm(list=ls())

#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".

#Importing packages. If not yet installed, packages can be installed by going to:
#Tools -> Install Packages, then enter their exact names from within each 
#library()
library(tidyverse) # For data management
library(igraph) # To transform and analyze network data
library(ggnetwork) # To make good-looking network graphs
library(scales) # To add percentages
library(gridExtra) # For montage of networks
library(grid) # For montage of networks
#Although not supposed to load here, the functions below auto-loads the 
#following. If not already, make sure to install these packages as well.
#  egonet
#  sna
#  statnet.common
#  network
setwd("~/Desktop/India Clean Cooking Project")
#Imports data and assigns it to variable "dataset", make all strings into
#  non-factors to preserve names.

load("matrix_list_women.rda")
load("alter_data.rda")

alter_frame$snaw_redcap_id_factor <- factor(alter_frame$snaw_redcap_id,
                                            levels = unique(alter_frame$snaw_redcap_id))

c(7, 85)

matrix_name <- "7"
mat <- matrix_list[[match(matrix_name, names(matrix_list))]]
alter_frame %>% subset(snaw_redcap_id == as.integer(matrix_name)) %>% View()
color_var <- "caste"
shape_var <- "lpg"

levels(alter_frame$caste)

input <- c("EGO", "yes", "yes", "no", "no")
check <- c("EGO", "yes", "no")
output_if <- c(21, 22, 24)
if_else_custom <- function(input, check, output_if){
  input %in% check[1]
  rep(output_if[1], length(input))
  ifelse(input == check[1], output_if[1], ifelse(input == check[2], output_if[2], ifelse(input == check[3], output_if[3], NA)))
}

#Function which makes Social Network Image 
make_image <- function(mat, matrix_name, shape_var, color_var) {
  ##########
  # Function: Creates and outputs a network graph with nodes named and ties colored
  # Inputs: x = input dataset with 1 row
  # Ouputs: plot1, a single network graph
  ##########
  
  alter_workbench <- alter_frame %>%
    subset(snaw_redcap_id %in% unique(as.integer(matrix_name)))
  
  alter_workbench[[shape_var]]
  alter_workbench[[color_var]]
  
  #Saves important values for determining graph formatting
  ego4.g  <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
  V(ego4.g)$color <- c("EGO", levels(alter_workbench[[color_var]])[alter_workbench[[color_var]]])
  V(ego4.g)$shape <- c("EGO", levels(alter_workbench[[shape_var]])[alter_workbench[[shape_var]]])
  colors  <- c("blue", "red") #create color palette for ties
  ego_col <- ifelse(V(ego4.g)$name == "EGO", "grey17", ifelse(V(ego4.g)$color == "General","red",
                                                              ifelse(V(ego4.g)$color == "SC/ST","blue",
                                                                     ifelse(V(ego4.g)$color == "OBC","green",
                                                                            ifelse(V(ego4.g)$color == "other religious minorities","purple","white"))))) 
  ego_shape <- ifelse(V(ego4.g)$name == "EGO", 21, ifelse(V(ego4.g)$shape == "yes", 22, 24))
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
    
  }else{
    E(ego4.g)$weight <- weight.ego
    #Creates actual network graph
    plot1 <- ggplot(ego4.g, aes(x = x, y = y, xend = xend, yend = yend, na.rm = FALSE)) + 
      #Determines coloration and style of network edges
      geom_edges(aes(linetype = as.factor(weight), color = (weight)), curvature = 0.1) +
      #Fills nodes with ego_color pallate
      geom_nodes(fill = ego_col, size = 14, shape = ego_shape) +
      #Names each node with alter names
      geom_nodelabel(label = rownames(mat))+
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


# variable_select <- "lpg"
# answers <- "yes"

by_prop <- function(alter_workbench, variable_select, answers){
  sum(alter_workbench[[variable_select]] %in% answers, na.rm = TRUE) /
    sum(!is.na(alter_workbench[[variable_select]]))
}

#Example for turning "don't know's" into NA's.
# levels(alter_frame$lpg) <- c("yes", "no", NA)
# summary(alter_frame$lpg)

# by_product <- by(alter_frame, alter_frame$snaw_redcap_id_factor, by_prop, variable_select = "lpg", answers = "yes")
by_breaker <- function(by_product){
  return(data.frame(
    id = as.integer(names(by_product)),
    values = as.numeric(by_product)))
}

# by_breaker(by(alter_frame, alter_frame$snaw_redcap_id_factor, by_prop, variable_select = "lpg", answers = "yes"))

for(i in 1:length(matrix_list)){
  pdf(file = paste0("Network Graphs/Network Graph ID ", names(matrix_list)[i], ".pdf"))
  make_image(matrix_list[[i]], names(matrix_list)[i], shape_var = "lpg", color_var = "caste")
  dev.off()
}
