

setwd("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/India Clean Cooking Project")
rm(list = ls())


#Importing packages. If not yet installed, packages can be installed by going to:
#  Tools -> Install Packages, then enter their exact names from within each
#  library()
library(tidyverse) # For data management
library(tableone)

processed_data <- read.csv("snaw_women_processed_data.csv", stringsAsFactors = FALSE)


table2 <- CreateTableOne(data = processed_data,
							 vars = c("prop_lpg", "lpg_caste_match", "alter_count" , "max_degree",
							 "mean_degree", "density", "constraint", "effsize", "constraintInt",
							 "lpg_homophily", "caste_homophily", "shg_homophily"),
							 strata = "household_group")

sapply(processed_data %>% select(c("prop_lpg", "lpg_caste_match", "alter_count" , "max_degree",
																	 "mean_degree", "density", "constraint", "effsize", "constraintInt",
																	 "lpg_homophily", "caste_homophily", "shg_homophily")), shapiro.test)

shapiro.test(processed_data$alter_count)

ggplot(processed_data, aes(x = alter_count)) +
	geom_density()



