
#-----------------------------------------------------------------------------
# Code for Lecture 3: Growth Accounting 
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# LESSON AND CODING NOTES 
#
# Goal: For country "x", identify which component of the production function
#       contributed the most to the country's growth from year 1960 to 2000. 
#
# Additional question to reflect on: 
#       Can growth be achieved in different ways?
# 
# Coding skills that students should grasp:
#       1) Basics of setting up a directory 
#       2) Uploading and viewing the data 
#       3) Dataframe basics + creating a new variable in the dataframe
#
# Data used: A modified version of Penn World Tables Version 10.01
#            lec3_modified_pwt1001.dta (Stata dataset)
#------------------------------------------------------------------------------


###                                                                    ###
### 1) Setting Up: Packages needed, setting directory, upload the data ###
###                                                                    ###

# Packages needed for this lesson. 

#install.packages("haven") # Only need to install once. 
#install.packages("dplyr") 

library(haven) # Needed for the read_dta() command. 
library(dplyr) # Needed for lag() command. 

# setwd (set working directory) is a function that tells r what path should R look at 
setwd("C:/Users/ardin/OneDrive/GLBL 225 Spring 2024/Lecture Slides/2 The Macro Approach/Macro Approach Lecture Codes") 

# Upload data
data  <- read_dta("lec3_modified_pwt1001.dta")
data  <- data.frame(data)        # Make sure that the data is a dataframe object



###                                                                  ###
### 2) Viewing and understanding the data + some dataframe basics    ###
###                                                                  ###

# You can view the data. We have data for each country and each year. A row is a country-year combination. 

# View(data)

data$country
unique(data$country)

# Because your data is a dataframe, you can view specific rows or columns.
# The row is the first argument inside [,], column is second argument. 

data[data$country=="Republic of Korea",  ]
View(data[data$country=="Republic of Korea", ])

View(data[data$country=="Republic of Korea", c("country", "year", "cn") ])


###                                                                ###
# 3) Continue dataframe basics, creating a new variable            ###
###                                                                ###

# Remember the goal: We want to understand which component lead the high growth 
#                    for the Republic of Korea from 1980 to 2000. 

# Our production function is y = A k^alpha. 

# STEP 3.1 Generate new variables 

# To do growth accounting, we will need to create additional variables: 
# Need log gdp per worker, log capital per worker, and log A  

data$ly <- log(data$rgdpe/data$emp)
data$lk <- log(data$cn/data$emp)
data$lA <- data$ly - 0.3*data$lk



# STEP 3.2 Calculate growth rates  

data$growth_lA <- (data$lA - lag(data$lA, order_by=data$country))
data$growth_lk <- (data$lk - lag(data$lk, order_by=data$country))
data$growth_ly <- (data$ly - lag(data$ly, order_by=data$country))

# The variable below is used just to check if the sum make sense from our model
data$growth_ly_check = data$growth_lA + 0.3*data$growth_lk

# View(data[, c("country", "year", "growth_ly", "growth_ly_check")])



# STEP 3.3 Calculate the contribution of A and k to economic growth. 

data$A_contribution <- data$growth_lA/data$growth_ly
data$k_contribution <- 0.3*data$growth_lk/data$growth_ly   

data$check_sum_to_one <- data$A_contribution + data$k_contribution

data$growth_lk03 <- 0.3*data$growth_lk

# View(data[, c("country", "year", "growth_ly", "growth_lk03", "growth_lA", "A_contribution", "k_contribution")])
