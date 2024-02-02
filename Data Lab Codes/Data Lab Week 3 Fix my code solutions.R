#-----------------------------------------------------------------------------
# Code for Discussion Section Week 3: Can you fix this error? 
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

# This code gives additional practice for:
# - playing around with the data
# - getting used to the commands that we have used so far
# - learn more about fixing common errors

# 
# Task: A student wrote a code but is seeing a lot of red...  
#       Help fix the issues below. 

###                                                                  ###
### Q1) Uploading the Penn World Tables                              ###
###                                                                  ###

# I want to open my data but an error keeps popping up. 
# Add to the code below to fix this. 

# haven and set directory is missing. 
library(haven)
setwd("C:/Users/ah2487/OneDrive/GLBL 225 Spring 2024/glbl225_spring_2024/Data Lab Codes")

data  <- read_dta("pwt1001.dta")  # This is now the full Penn World Table data. 
data  <- data.frame(data)         # Make sure that the data is a dataframe object


###                                                                  ###
### Q2) Viewing country data in 2019                                 ###
###                                                                  ###

# I want to view my dataset for 2019 but the code below doesn't work. 
# Then I just want to save specific columns in my data. 
# Add to the code below to fix this. 

View(data[data$year==2019,]) # The original one missing a , and data$. 

data <- data[, c("country", "year", "rgdpo", "emp", "cn", "hc")]

###                                                                  ###
### Q3) Getting the mean of real GDP per capita                      ###
###                                                                  ###

# I want to get the mean real gdp per capita. This gives me an NA.  
# Add to the code below to fix this. 

mean(data$rgdpo/data$emp, na.rm=TRUE) # Pop doesn't exist which is why this didn't work.
                                      # It needs to be added to line 41 or change this into emp. 

###                                                                  ###
### Q4) Ordering the data with multiple variables                    ###
###                                                                  ###

# Currently the data is ordered by year and the country. 
# I want to view the data to show a list of countries of the same year grouped together instead.
# It will be great if the country is in alphabetical order. 

View(data[order(data$year, data$country),])

###                                                                  ###
### Q5) I want to do the development accounting for 2019 data        ###                                                                  ###
###                                                                  ###

# The code below has so many errors. Can you fix it without looking at our lecture code?
# There are some theoretical errors too!

data <- filter(data, year==2019) # Need to assign this to data, otherwise the exercise is done for all years!!!

data$y    <- data$rgdpo/data$emp  
data$y_k  <- (data$cn/data$emp)^0.3 * (data$h)^0.7 # Should be a multiple not a plus. 

v1 <- var(log(data$y), na.rm=TRUE) # Don't forget the logs! 
v2 <- var(log(data$y_k), na.rm=TRUE)

success <- v2/v1 
print(success) 

# The answer should be 0.277 or 27.7%. 


