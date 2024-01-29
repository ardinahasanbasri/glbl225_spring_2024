#-----------------------------------------------------------------------------
# Code for Lecture 5: Development Accounting 
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# LESSON AND CODING NOTES 
#
# Goal: Which "component" contributes the most to cross country inequality?
#
# Additional question to reflect on: 
#       Given the answer to the question above, what can policymakers do? 
# 
# Coding skills to grasp for students:
#       1) Filtering a dataset for specific countries or years 
#       2) sorting data using order()
#       3) Be careful with missing data for certain commands
#
# Data used: A modified version of Penn World Tables Version 10.01
#            pwt1001.dta (Stata dataset; Original pwt1001)
#------------------------------------------------------------------------------


###                                                                    ###
### 1) The Usual: Setting Up                                           ###
###                                                                    ###

# Packages needed for this lesson. 

library(haven) # for read_dta()
library(dplyr) # for filter()
library(here)  # for using here()

setwd(here())  # change this if necessary

# Upload data
data  <- read_dta("pwt1001.dta")  # This is now the full Penn World Table data. 
data  <- data.frame(data)         # Make sure that the data is a dataframe object



###                                                                  ###
### 2) Filtering the data to make it easy to use                     ###
###                                                                  ###

data  <- filter(data, year==2019) # Easier to use filter. 

data  <- data[, c("country", "year", "rgdpo", "emp", "cn", "hc", "pop")]

# Let's sort the data by rgdp.

View(data[order(data$rgdpo)  ,  ])

data[order(data$rgdpo),]

# For fun, which country has the highest GDP per capita? 

data$gdp_pc <- data$rgdpo/data$pop

View(data[order(data$gdp_pc),])

View(data[order(data$gdp_pc, decreasing = TRUE),])


###                                                                  ###
### 3) Creating a measure of success                                 ###
###                                                                  ###

# Remember the goal: To understand which component contributes the most to cross  
#                    country growth, we need to calculate the variance of y_kh () 
#                    and the variance y. 

# Our production function is y = A k^alpha h^(1-alpha). 

data$y    <- data$rgdpo/data$emp  
data$y_k  <- (data$cn/data$emp)^0.3 * (data$h)^0.7

v1 <- var(log(data$y), na.rm=TRUE)
v2 <- var(log(data$y_k), na.rm=TRUE)

success <- v2/v1 
print(success)

# Can you interpret what this measure means? 
