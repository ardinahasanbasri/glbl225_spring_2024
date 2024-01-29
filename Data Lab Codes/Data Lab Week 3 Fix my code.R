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

# Solutions will be up after discussion sections are done. 

###                                                                  ###
### Q1) Uploading the Penn World Tables                              ###
###                                                                  ###

# I want to open my data but an error keeps popping up. 
# Add to the code below to fix this. 

data  <- read_dta("pwt1001.dta")  # This is now the full Penn World Table data. 
data  <- data.frame(data)         # Make sure that the data is a dataframe object


###                                                                  ###
### Q2) Viewing country data in 2019                                 ###
###                                                                  ###

# I want to view my dataset for 2019 but the code below doesn't work. 
# Add to the code below to fix this. 

View(data[year==2019])

data <- data[, c(country, year, rgdpo, emp, cn, hc)]

###                                                                  ###
### Q3) Getting the mean of real GDP per capita                      ###
###                                                                  ###

# I want to get the mean real gdp per capita. This gives me an NA.  
# Add to the code below to fix this. 

mean(data$rgdpo/data$pop)

###                                                                  ###
### Q4) Ordering the data with multiple variables                    ###
###                                                                  ###

# Currently the data is ordered by year and the country. 
# I want the data to show a country and then year. 

View(data[order(year, country)])

###                                                                  ###
### Q5) I want to do the development accounting for 2019 data        ###                                                                  ###
###                                                                  ###

# The code below has so many errors. Can you fix it without looking at our lecture code?
# There are some theoretical errors too!

filter(data, year==2019)

data$y    <- rgdpo/emp  
data$y_k  <- (cn/emp)^0.3 + (h)^0.7

v1 <- var(data$y)
v2 <- var(data$y_k)

success <- v2/v1 
print(success) 

# The answer should be 27.7


