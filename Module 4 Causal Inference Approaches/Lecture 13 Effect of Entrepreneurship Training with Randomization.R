#-----------------------------------------------------------------------------
# Code for Lecture 13: The Effect of Entrepreneurship Training 
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# LESSON AND CODING NOTES 
#
# Goal: Show that randomization leads to ATE = SDO. 
#       This code is the same as the one in lab and for lecture 13. 
#       The difference is the randomization code at the end. 
#       This randomization code is optional. 
#------------------------------------------------------------------------------


#---------
# Step 1: Create the dataframe based on the slide in class. 
#         Then create the individual treatment effect.  
#---------

Data <- data.frame("person_id" = c(1,2,3,4,5,6,7,8,9,10), 
                   "Yi_1"= c(7,5,5,7,4,10,1,5,3,9), 
                   "Yi_0"  = c(1,6,1,8,2,1,10,6,7,8))

# Creating individual treatment effect of the training.  
Data$alpha_i <- Data$Yi_1 - Data$Yi_0 

#----------
# Step 2: As a note to yourself, write down what each column in the dataset means.
#----------

# person_id "Person ID" 
# Yi_1 "Outcome when everyone received training in World 1"
# Yi_0 "Outcome when everyone received no training in World 0"
# alpha_i "Individual treatment effect, how effective is training for the individual"

#---------
# Step 3: Data that the assistant sees.  
#              
#---------

# One possible story is that there is selection. People who know they would benefit from training
# decides to volunteer for the program. 
# Make another column D_i which is 1 if they would benefit from the program (alpha>0) and 0 otherwise. 

Data$D_i[Data$alpha_i>0]  <- 1 
Data$D_i[Data$alpha_i<=0] <- 0 

# D is "In the current world where the assistant lives, the person either gets training or not."

# Make another column showing Y_i that the assistant observes. 

Data$Y_i <- Data$Yi_1    # I copy the column fully. 
Data$Y_i[Data$D_i==0] <- Data$Yi_0[Data$D_i==0]  # For those who didn't get the training, I replace it with Yi_0


#---------
#  Step 4: Using the mean command, calculate ATE, ATT, and ATU (average treatment of the un-treated)
#---------

# ATE, ATT, ATU
mean(Data$alpha_i)                 # Everyone
mean(Data$alpha_i[Data$D_i==1])    # Just treated group
mean(Data$alpha_i[Data$D_i==0])    # Just untreated group


# We will use the above calculations later on. 
# But from the calculation of ATE, on average, it seems training 
# increases income by 0.6 (units of choice) after a year.  


#----------
# Step 5: Find the simple difference in outcomes (SDO).
#         which the average income difference of those in the training group - of those in the no training group. 
#----------

# Mean of group 1 and see the average income change after training
m1 <- mean(Data$Y_i[Data$D_i==1])

# Mean of group 0 and see the average income change after no training
m0 <- mean(Data$Y_i[Data$D_i==0])

# Simple difference of outcomes 

SDO <- m1 - m0 
print(SDO)

# If the assistant takes SDO as the truth, then the number is misleading. 

#--------------------------------
# PART 2: What is SDO  measuring?
#--------------------------------

# Why do we see a misleading SDO? 
# Because SDO is the ATE with selection bias and heterogeneous treatment bias. 
# Refer to equation from slide. 

ATE <- mean(Data$alpha_i)
ATT <- mean(Data$alpha_i[Data$D_i==1])
ATU <- mean(Data$alpha_i[Data$D_i==0])

pi  <- length(Data$alpha_i[Data$D_i==1])/length(Data$alpha_i) 

SDO_calculate <- ATE + 
  mean(Data$Yi_0[Data$D_i==1]) - mean(Data$Yi_0[Data$D_i==0]) +
  (1-pi)*(ATT-ATU)

print(SDO_calculate)
print(SDO)

#--------------------------------
# PART 3: BONUS Randomization Exercise
#--------------------------------

# Suppose we create an experiment where we randomize people into training versus no training. 

set.seed(123) # Set seed so that numbers drawn are the same 

# There are multiple ways to do this randomization.
# This choose randomly 5 entrepreneurs 
First_Draw <- sample(1:10, 5)
Group_D1   <- Data[First_Draw,]
Group_D2   <- Data[-First_Draw,] # The minus takes out the set from the data

SDO_first_draw <- mean(Group_D1$Yi_1) - mean(Group_D2$Yi_0)
SDO_first_draw

# The above is a draft code. Now let's repeat this exercise 1000 times using a loop. 
vector_SDOs <- c() # empty vector to say SDO here later
Group_D1_all <- data.frame()
Group_D2_all <- data.frame()

for(x in 1:1000){
  get_sample  <- sample(1:10, 5)
  Group_D1    <- Data[get_sample,]
  Group_D2    <- Data[-get_sample,] 
  sdo_sample  <- mean(Group_D1$Yi_1) - mean(Group_D2$Yi_0)
  vector_SDOs <- c(vector_SDOs, sdo_sample)
  Group_D1_all<- rbind(Group_D1_all, Group_D1)
  Group_D2_all<- rbind(Group_D2_all, Group_D2)
  }

mean_sdo_experiment <- mean(vector_SDOs)
print(ATE)
print(mean_sdo_experiment)

mean(Group_D1_all$Yi_1) - mean(Group_D2_all$Yi_0) # Same as above.

# The random experiment done multiple times give a closer number to ATE. 

#--------------------------------
# PART 4: BONUS Regression
#--------------------------------

# To show that a regression is the same as calculating the SDO

lm(Y_i~D_i, data=Data) # gives you SDO from the assistant since we used the original data. 

# If you had data where D is randomized, the SDO will then be the same as ATE in the regression as well. 

Group_D1_all$Y_i <- Group_D1_all$Yi_1  # Make a new data based on the randomization loop above. 
Group_D2_all$Y_i <- Group_D2_all$Yi_0
Group_D1_all$D_i <- 1
Group_D2_all$D_i <- 0

Group_random <-rbind(Group_D1_all,Group_D2_all)

mean(Group_random[Group_random$D_i==1,]$Y_i) - mean(Group_random[Group_random$D_i==0,]$Y_i)

lm(Y_i~D_i, data=Group_random) # This one uses the randomization data.  
