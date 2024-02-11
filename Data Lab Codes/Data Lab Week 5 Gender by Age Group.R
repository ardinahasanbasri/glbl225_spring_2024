#-----------------------------------------------------------------------------
# Code for Discussion Section Week 5: Getting More Statistics!
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

# The three commands we learned last week are very versitile. 
# This code gives additional practice for using these commands to get other types of statistics. 

# Solutions will be up after discussion sections are done. 

# Set up your directory and library as usual. 

library(haven)
library(dplyr)      # For the command summarize()
library(flashlight) # For the command grouped_stats() - allow for statistics with weights

setwd("...") # Fill this in. 

hh_info   <- read_dta("hh_mod_a_filt.dta") # Household-level data.
ind_data  <- read_dta("HH_MOD_B.dta")      # Individual-level data. 

data <- merge(ind_data, hh_info, by.x="case_id", by.y="case_id")

##############
# TASK     : You are a research assistant tasked to get additional descriptive statistics for Malawi.   
# QUESTION : What is the share of men and women by age group? 
##############

# Your supervisor wants individuals in 6 age group: 15 - 20, 21 - 30, 31 - 40, 41 - 50, 51 - 60, 61 - 80.
# You can ignore other ages for now. 
# Hint: Create a group variable from 1 to 5. Then can use summarize to get the weighted_mean of gender (need to change 0 and 1).
# You can use summarize by gender and age group. 
# Age comes from hh_b05a
# Gender is hh_b03
# Don't forget the weights! 

# It might be helpful to first this what you want your final output to look like. 
# This will help you figure out what is the plan for the code. 
# See if you are able to do this task in two ways: using summarize() and using grouped_stats() 

