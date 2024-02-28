#-----------------------------------------------------------------------------
# Code for Lecture 14: Replicating Targeting the Ultra Poor Program Paper 
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# LESSON AND CODING NOTES 
#
# Goal: Show how to run a regression for the TUP paper and replicate one of its graph.   
#
# Coding skills to grasp for students:
#       1) running a regression 
#       2) reshape (used to rearrange data)
#       3) ggplot basics
#       4) understanding the use of factor()
#
# Data used: Data comes from the Banerjee, Duflo, and Sharma (2021) paper.
#            One of the file is too large. Please download from weekly page under lecture 14. 
#
# Additional reference link: 
# https://rkabacoff.github.io/datavis/Bivariate.html#bar-chart-on-summary-statistics
# https://www.geeksforgeeks.org/how-to-create-a-grouped-barplot-in-r/
#------------------------------------------------------------------------------

library(haven)    # For uploading stata data
library(ggplot2)  # For graphing 
library(dplyr)    # For summarize 
library(estimatr) # For robust standard errors replicating stata results.  

setwd("C:/Users/ardin/OneDrive/GLBL 225 Spring 2024/glbl225_spring_2024/Module 4 Causal Inference Approaches")

# ------------------------------- #
# Replicate Regression from Paper #
# ------------------------------- # 

data <- read_dta("TUP_HH_Constructed.dta")

# The code below is to clean the variables before we use them. 
# We will replicate the first number from table 1. 
# The variable of interest is the asset index: "asset_ind_tot".
# bl means baseline, el1 means endline 1 or 18 months.
# You can run this code for other endline. Endline 2 is at 3 years. 

data$var_of_interest <- data$asset_ind_tot_el1
data$var_of_interest_baseline <- data$asset_ind_tot_bl

# The code in the paper also controls for missing baseline. 
# The code below tags if baseline is missing and replaces it with 0. 
data$var_of_interest_baseline[is.na(data$var_of_interest_baseline)] <- 0
data$missing_baseline <- 0
data$missing_baseline[is.na(data$asset_ind_tot_bl)] <- 1


# A regular regression would be below. 
model1_reg <- lm(var_of_interest ~ treatment + var_of_interest_baseline + missing_baseline  + factor(vil_ham)  , data=data)

# The one from the paper is below. 
model1 <- lm_robust(var_of_interest~ treatment + var_of_interest_baseline + missing_baseline  + factor(vil_ham)  , data=data, se_type = "stata")

summary(model1)

# Let's do another regression replication for the first number of table 3. 

data$var_of_interest <- data$liverev_el1
data$var_of_interest_baseline <- data$liverev_bl
data$var_of_interest_baseline[is.na(data$var_of_interest_baseline)] <- 0
data$missing_baseline <- 0
data$missing_baseline[is.na(data$liverev_bl)] <- 1

model2 <- lm_robust(var_of_interest ~ treatment + var_of_interest_baseline + missing_baseline  + factor(vil_ham)  , data=data, se_type = "stata")

summary(model2)

# ------------------------------- #
# Replicate Figure from the Paper #
# ------------------------------- # 

data <- read_dta("TUP_HH_Constructed_for_figures.dta")

# To make things easier, let just focus on enough food. 
data_enoughfood <- data[, c("hhid","treatment", 
                            "enoughfood_el1", "enoughfood_el2", "enoughfood_el3", "enoughfood_el4")]

# We need to reshape the data before summarizing and graphing. 
reshape_data <- reshape(data_enoughfood,
                        idvar= c("hhid","treatment"),
                        varying = c("enoughfood_el1", "enoughfood_el2", "enoughfood_el3", "enoughfood_el4"), 
                        v.names = "enoughfood_el", 
                        timevar= "wave",
                        times = c(1, 2, 3, 4),
                        direction = "long")

final_data <- summarize(reshape_data, mean = mean(enoughfood_el, na.rm=TRUE), .by = c("wave", "treatment"))

# Add these factors so that the labels will show up on the graph. 
final_data$treatment <- factor(final_data$treatment, label=c("Control", "Treatment"))
final_data$wave      <- factor(final_data$wave, label=c("Month 18", "Year 3", "Year 7", "Year 10"))

ggplot(final_data, aes(x=wave, y=mean, fill= treatment)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(final_data, aes(x=wave, y = mean, fill = treatment)) + 
  geom_bar(position = "dodge", stat="identity") +
  theme_minimal() +
  ggtitle("Per Capita Monthly Consumption") +
  ylab("Mean in 2018 Dollars") +
  scale_fill_manual(values=c("#7DCFB6", "#FBD1A2")) +
  labs(fill=NULL) #Get rid of legend label.
