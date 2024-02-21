#-----------------------------------------------------------------------------
# Code for Lecture 11: The Effect of Entrepreneurship Training 
# 
# GLBL 225 Approaches to International Development 
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# LESSON AND CODING NOTES 
#
# Goal: Show that the simple differences in outcome is connected to ATE.  
#       (Equation we saw in class.)
# 
# Coding skills to grasp for students:
#       - Manually create a dataframe. 
#       - Focus more on understanding how the code connects to class material.  
#
#------------------------------------------------------------------------------


#---------
# Step 1: Create the dataframe based on the slide in class. 
#         Then create the individual treatment effect.  
#---------

Data <- data.frame("person_id" = c(1,2,3,4,5,6,7,8,9,10), 
                   "Yi_1"= c(7,5,5,7,4,10,1,5,3,9), 
                   "Yi_0"  = c(1,6,1,8,2,1,10,6,7,8))

# Creating individual treatment effect of the blue pill.  
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






#----------
# Step 5: Find the simple difference in outcomes (SDO).
#         which the average income difference of those in the training group - of those in the no training group. 
#----------




#--------------------------------
# PART 2: What is SDO  measuring?
#--------------------------------

# Why do we see a misleading SDO? 
# Because SDO is the ATE with selection bias and heterogeneous treatment bias. 
# Refer to equation from slide. 

