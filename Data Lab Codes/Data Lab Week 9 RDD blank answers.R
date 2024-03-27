#-----------------------------------------------------------------------------
# Code for Discussion Section Week 9: Example running a regression discontinuity design
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

# This code gives an additional example of how to run an RDD.
# - Students should know the intuition of why this is an RDD regression. 
# - One way to view the effect is to graph the outcome at the cutoff point! 
# - You will replicate a paper's result. Note that the paper's model is a bit more complicated, 
#   we will just focus on the general intuition of the paper. 
#   The model runs a coupe of RDD-type regressions. 

##############
# Paper of interest :  Do voters elect or affect policies? Lee et al. 2004 
# 
# Paper can be found: https://academic.oup.com/qje/article-abstract/119/3/807/1938834?redirectedFrom=fulltext  
##############

# Set up your directory and library as usual. 

library(rdd)
library(haven)

setwd("...") # Fill this in. 

# Import the voting data from Canvas. 

voting_data  <- read_dta("voting_data_for_RDD.dta")

# See discussion section slide on background information of paper and exercise.  
# Question 1 to 3 are RDD regressions used in the model. 

#----
# 1) Regress score on lag democrat for when lagdemvoteshare is between 0.48 to 0.52, 
#---- 




#----
# 2) Regress score on democrat for when lagdemvoteshare is between 0.48 to 0.52 
#----




#----
# 3) Regress democrat on lagdemocrat, again when lagdemvoteshare is between 0.48 to 0.52.
#----




#----
# 4) How would 1) change when using the entire sample instead of with restrictions? 
#----



# We compare this regression to compare it with the RDD regression above.  
# The difference in ADA score is much larger, but this is due to the fact that 
# we do have very different populations for places where a democrat won versus a republican. 
# This regression would not be a regression where we can make causal inference! 

#----
# 5)  Based on your results in part 1,2, and 3, calculate the "affect" component. 
#     So do voters elect or affect policy based on your results?
#----
# Using the RDD regressions above, we replicate the calculation of the table based on the model. 

# 21.28 - 47.71*0.4843 = -1.825 

# The elect component is so strong that the affect component is negative but small.
# Therefore, the paper result suggest that voters elect politicians, and politicians 
# choose whatever policy they want. 

# Additional code for using a package to get the graph. 

RDD_est <- RDestimate(score~lagdemvoteshare, data=voting_data, 
                      subset=(voting_data$lagdemvoteshare>0.48 & voting_data$lagdemvoteshare<0.52), 
                      cutpoint = 0.5)

plot(RDD_est)
title(xlab="Democrat vote share at time t",ylab="ADA score time t+1")

# Look at that nice jump on the cutoff! RDD paper typically has this type of graph. 