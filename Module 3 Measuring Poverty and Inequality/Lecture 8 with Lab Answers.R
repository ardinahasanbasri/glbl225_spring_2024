#----------------------------------------------------------------------------
# Code for Lecture 8: Aggregating Expenditure/Consumption using Malawi data 
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# LESSON AND CODING NOTES 
#
# Goal: How many people's food consumption is below the poverty line? 
#       (We will focus on food consumption for now because we are working on food first.)
# 
# Additional questions: What are the patterns of consumption by different regions/groups? 
#     
# Coding skills to grasp for students:
#       1) collapsing/aggregating the data by group using the summarize function 
#       2) basics of merging multiple datasets together
#       3) getting basic summary statistics by group & simple graphs
#
# Data used: Datasets from the Malawi Integrated Household Survey
#            This dataset contains 11434 households. 
#            Multiple datasets are used: HH_MOD_G1, hh_mod_a_filt.dta
#            Also useful is the household questionnaires available in the folder.
#------------------------------------------------------------------------------

# HH_MOD_G1 file is quite large: please download it through canvas since github limits file size. 
#                                the link is located under Lecture 8

###                                                                    ###
### 1) The Usual: Setting Up                                           ###
###                                                                    ###

library(here)
library(haven)
library(dplyr)      # For the command summarize()
library(flashlight) # For the command grouped_stats() - allow for statistics with weights

setwd(here())

food_cons <- read_dta("HH_MOD_G1.dta")
hh_info   <- read_dta("hh_mod_a_filt.dta")

food_cons <- data.frame(food_cons)
hh_info   <- data.frame(hh_info)


###                                                                    ###
### 2) Getting Food Consumption per Household                          ###
###                                                                    ###

# View the food consumption data and make sure you understand how to read the data
# using the household questionnaire. 
# The current data is too detailed for us, let's aggregate the data using the summarize command. 

# Summarize will take   i) the data: food_cons
#                      ii) how do you want to aggregate. 
#                          we want to sum() for hh_g05 and make sure to remove na (missings)
#                     iii) how do you want to group the aggregation
#                          we want to aggregate by case_id which is the household identifier. 

food_exp_by_hh <- summarize(food_cons, Total_Food_Exp = sum(hh_g05,na.rm = TRUE), .by = (case_id))


###                                                                    ###
### 3) Merge the data with another dataset                             ###
###                                                                    ###

merged_data <- merge(x=hh_info, y=food_exp_by_hh, by.x="case_id", by.y="case_id")


###                                                                    ###
### 4) Getting basic summary statistics with weights                   ###
###                                                                    ###

# Observe what happens when you look at the statistics with weights vs no weights. 

unique(merged_data$region) # To see what the value labels are for region.

grouped_stats(merged_data, "Total_Food_Exp", by="region")
grouped_stats(merged_data, "Total_Food_Exp", w = "hh_wgt", by="region")

grouped_stats(merged_data, "Total_Food_Exp", by=c("region", "reside"))
grouped_stats(merged_data, "Total_Food_Exp", w = "hh_wgt", by=c("region", "reside"))

# How many individuals are spending (just on food since this is the data we have now) under $2 a day? 
# International Poverty Line 656.7 in Malawi Kwacha (2019) or $2.15 dollar a day. According to: https://databankfiles.worldbank.org/public/ddpext_download/poverty/987B9C90-CB9F-4D93-AE8C-750588BF00QA/current/Global_POVEQ_MWI.pdf
# Our consumption data is for a week. 
# Let's set our weekly poverty rate as 656.7*7=4596.9

merged_data$below_poverty_line[merged_data$Total_Food_Exp<4596.9]  <- 1
merged_data$below_poverty_line[merged_data$Total_Food_Exp>=4596.9] <- 0

mean(merged_data$below_poverty_line) # This is not truly the correct measure since we are only looking at food consumption. 
# This is not correct because we don't have weights!
# This is not correct because its not per-capita. 

grouped_stats(merged_data, "below_poverty_line", w = "hh_wgt")
grouped_stats(merged_data, "below_poverty_line", w = "hh_wgt", by=c("region", "reside"))





# Questions to discuss in this week's data lab: 
# 1) What would the poverty rate be if we look at per-capita food consumption? 
#    Check if you have household size in the data.
#    This helps with your homework since your homework asks poverty measure based on per-capita measurement. 

merged_data$food_exp_pc <- merged_data$Total_Food_Exp/merged_data$hhsize

merged_data$below_poverty_line2[merged_data$food_exp_pc<4596.9]  <- 1
merged_data$below_poverty_line2[merged_data$food_exp_pc>=4596.9] <- 0

grouped_stats(merged_data, "below_poverty_line2", w = "hh_wgt")
grouped_stats(merged_data, "below_poverty_line2", w = "hh_wgt", by=c("region", "reside"))


# 2) Practice summarize() by district and find the district with the highest total consumption? 

food_tot_dist <- summarize(merged_data, Total_Food_Exp = sum(Total_Food_Exp,na.rm = TRUE), .by = (district))
View(food_tot_dist[order(food_tot_dist$Total_Food_Exp),]) # District 210. 

# 3) Now identify the district with the highest poverty rate. 

# Way 1: use summarize and weighted.mean
pov_rate_by_dist <- summarize(merged_data, pov_dist = weighted.mean(below_poverty_line2,na.rm = TRUE, w=hh_wgt), .by = (district))
View(pov_rate_by_dist[order(pov_rate_by_dist$pov_dist),]) # District 209. 

# Way 2: use group stats and save the data
pov_rate_by_dist <- grouped_stats(merged_data, "below_poverty_line2", w = "hh_wgt", by=c("district"))
View(pov_rate_by_dist[order(pov_rate_by_dist$below_poverty_line2),]) # District 209. 

