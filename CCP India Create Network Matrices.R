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
dat_women <- read.csv("SNA data_women.csv", stringsAsFactors = FALSE)

#Selecting the names of the alters, and changing all blank entries into NA's
name_select <- dat_women %>% select(snaw_g7_name1:snaw_g7_name20)
name_select[name_select == ""] <- NA

#Calculating the network size by counting how many values are not NA's (aka, have names)
sum(!is.na(name_select[1,]))

network_size <- apply(name_select, 1, function(x){return(sum(!is.na(x)))})
dat_women$network_size <- network_size

dat_women <- dat_women[!dat_women$network_size == 0,]

# x <- dat_women[1,]

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

# for(i in 1:nrow(dat_women)){
#   print(i)
#   print(make_base_mat(dat_women[i,]))
# }


# x <- dat_women[15,]

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
  ego_col <- ifelse(V(ego4.g)$name == "EGO", "grey17", "white") 
  
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
      geom_nodes(fill = ego_col, size = 14, shape = 21) +
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

matrix_list <- vector("list", nrow(dat_women))

#for-loop which iterates a number of times equal to the number of rows in the dataset.
#  Each iteration will call the function "make_mat", inputing the dataset and also
#  inputing the for-loops's iteration number (to call that row from the dataset).
#  "make_mat"'s  network matrix output is then assigned to matrix_list at the same index
#  as the row the network matrix was created from.
for(i in 1:nrow(dat_women)){
  matrix_list[[i]] <- make_base_mat(dat_women[i,])
}


