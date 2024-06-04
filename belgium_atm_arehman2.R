# # ===================================================
# GBA464: Assignment 1
# Author: Yufeng Huang
# Description: location choice of Belgium ATMs
# Data: Belgium ATM distribution in 1994
# Source: will reveal later 
# ===================================================
# INSTRUCTIONS
# 1. this assignment is individual and you cannot look at others' code
# 2. this assignment is graded mostly on correctness of the results, but please do 
#       try to maintain readability; please also follow the variable naming instructions
#       as they will be helpful for grading
# 3. variable definitions are downloadable at 
#   https://dl.dropboxusercontent.com/s/i5mb8saii14fa6h/variable_definition.txt
# 4. please make sure your code runs from the start to the end and produces the intended results

# clear everything
rm(list = ls())


# load data
url <- 'https://dl.dropboxusercontent.com/s/q6qzbfa1tdcqv6v/belgium_atm.csv'
df <- read.csv(url, stringsAsFactors = F)

# we can check the structure of the data by running

head(df)

# ==== question 1 ====

# Q1. First, recall that df is a data frame which is like a spreadsheet in Excel. 
#   Let's convert every column into a separate variable using '$'; for example:
population <- df$population
numATMs <- df$numATMs

# do the same for the other columns
ATMwithdr = df$ATMwithdr
withdrvalue = df$withdrvalue
unemprate = df$unemprate
numbranches = df$numbranches





# ------ you should not work with 'df' anywhere beyond this line -----
#   i.e. please only work with the vectors (or create new vectors)
#   for the rest of this assignment


# ==== question 2 ====

# Q2a. Do the necessary conversion for all variables so that you can apply numeric operations on them
#   replace the original value of the vector in case of conversion

#Only ATMwithdr and withdrvalue have type = chr. The other variables don't have missing values(as verified by looking at the dataset and the typeof the variables.)
ATMwithdr = as.numeric(ATMwithdr)
withdrvalue = as.numeric(withdrvalue)



# Q2b. population is in a very different scale. Re-scale it into thousands, i.e., divide population by 1000
#   and replace the variable
population = population / 1000




# ==== question 3 ====

# You want to take average for all variables but you realized that some variables have missing value
#   before taking averages, you need to make sure that all observations are taken from the same sets of 
#   observations (i.e. rows) where no variable is missing 

# Q3a. let's define a logical vector for non-missing rows, i.e. indicate TRUE for rows without any missing values, 
#   and FALSE for rows with missing values. Name the vector 'nm'. Note that the length of nm will be the same as 
#   the number of rows in the original data df. 
#   Note: you might know of the function 'complete.cases'. Please do not use that function for this task. 

nm = !(is.na(ATMwithdr)|is.na(numATMs)|is.na(numbranches)|
  is.na(population)|is.na(unemprate)|is.na(withdrvalue))


# Q3b. count the number of non-missing rows in the data df, name it 'count_nm'
#   count_nm should be one number. Note that there is no such a function 'count'. 
#   Think about how to count the number of non-missing rows. 

count_nm = sum(as.numeric(nm))




# ==== question 4 ====


# Q4. Calculate the averages of number of ATM, number of branches, population, 
#   unemployment rate, number of withdraw per resident and amount per withdrawl.
#   In particular, notice that certain variables have missing values and you might want to  
#   only calculate means for the rows without missing values of any variable
#   (that is, the rows that you use to calculate the average of all variables should be the same).
#   Finally, collect results in a vector called 'mean_nm', name elements in the vector by the original 
#   variable name (e.g., the mean of population in mean_nm will have the name "population")



AvgnumATM = sum(numATMs[nm])/count_nm
AvgnumBranches = sum(numbranches[nm])/count_nm
AvgPopulation = sum(population[nm])/count_nm
AvgUnemRate = sum(unemprate[nm])/count_nm
AvgATMWithdr = sum(ATMwithdr[nm])/count_nm
Avgamnt = sum(withdrvalue[nm])/count_nm

mean_nm = c(numATMs = AvgnumATM, numbranches = AvgnumBranches, population = AvgPopulation,
            unemprate = AvgUnemRate, ATMwithdr = AvgATMWithdr, withdrvalue = Avgamnt)



# ==== question 5 ====

# Q5. You realize that the reason for missing values in the original data is that there are no ATMs.
#   So in that regard you could have defined the missing values to zero
#   Re-define the missings to zero and assign it to the original variable,
#   find the total number of observations in the dataset (call it 'count_all'), 
#   and re-calculate means for the same set of variables and collect results in 'mean_all'. Also name 
#   elements by the original variable name


#Finding which vactors have missing values.
sum(is.na(population))
sum(is.na(numATMs))
sum(is.na(ATMwithdr))
sum(is.na(withdrvalue))
sum(is.na(unemprate))
sum(is.na(numbranches))
# Only the ATMwithdr and withdrvalue have missing values.

#Now, in these two vectors: replacing the missing values with 0


ATMwithdr[!nm] = 0
withdrvalue[!nm] = 0


#count_all

count_all = length(ATMwithdr)

#Checking if the value is correct.
nrow(df)

#mean_all

all_AvgnumATM = sum(numATMs)/count_all
all_AvgnumBranches = sum(numbranches)/count_all
all_AvgPopulation = sum(population)/count_all
all_AvgUnemRate = sum(unemprate)/count_all
all_AvgATMWithdr = sum(ATMwithdr)/count_all
all_Avgamnt = sum(withdrvalue)/count_all

mean_all = c(numATMs = all_AvgnumATM, numbranches = all_AvgnumBranches, population = all_AvgPopulation,
            unemprate = all_AvgUnemRate, ATMwithdr = all_AvgATMWithdr, withdrvalue = all_Avgamnt)

#mean_all
#mean_nm
# ==== question 6 ====

# You decide to investigate what's the average number of withdrawal and amount per withdrawal
#   by areas with different number of ATMs

# Q6a. Let's summarize the average ATMwithdr and average withdrvalue by the number of atms (for range 1-4). 
# 	That is, for all observations with number of ATMs equal to 1, compute the average ATMwithdr and withdrvalue. 
#	Do the same for observations with number of ATMs equal to 2, 3, and 4. Collect results in two separate 
# 	vectors and name them 'mean_atmwithdr' and 'mean_withdrvalue'

Vector_for1 = numATMs == 1
Vector_for2 = numATMs == 2
Vector_for3 = numATMs == 3
Vector_for4 = numATMs == 4

SumAtm1 = sum(ATMwithdr[Vector_for1])
SumAtm2 = sum(ATMwithdr[Vector_for2])
SumAtm3 = sum(ATMwithdr[Vector_for3])
SumAtm4 = sum(ATMwithdr[Vector_for4])

SumWithdr1 = sum(withdrvalue[Vector_for1])
SumWithdr2 = sum(withdrvalue[Vector_for2])
SumWithdr3 = sum(withdrvalue[Vector_for3])
SumWithdr4 = sum(withdrvalue[Vector_for4])


Count1 = sum(Vector_for1)
Count2 = sum(Vector_for2)
Count3 = sum(Vector_for3)
Count4 = sum(Vector_for4)


#Avg for 1
AvgATM1 = SumAtm1 / Count1
AvgWith1 = SumWithdr1 / Count1



#Avg for 2
AvgATM2 = SumAtm2 / Count2
AvgWith2 = SumWithdr2 / Count2


#Avg for 3
AvgATM3 = SumAtm3 / Count3
AvgWith3 = SumWithdr3 / Count3


#Avg for 4
AvgATM4 = SumAtm4 / Count4
AvgWith4 = SumWithdr4 / Count4



mean_atmwithdr = c(AvgATM1, AvgATM2, AvgATM3, AvgATM4)

mean_withdrvalue = c(AvgWith1, AvgWith2, AvgWith3, AvgWith4)





# Q6b. Separately, plot mean_a and mean_w by the number of ATMs; label the x axis as "number of ATMs" 
#	and y axis "average withdrawal per resident" and "average amount per withdrawal", respectively.
#   use line plot by setting type = 'l' as one of the plot function arguments

numATMArray = c(1,2,3,4)
plot(numATMArray, mean_atmwithdr, type = "l", lty = 1, ylab = "average withdrawal per resident", xlab = "number of ATM's")
plot(numATMArray, mean_withdrvalue, type = "l", lty = 1, ylab = "average amount per withdrawal", xlab = "number of ATM's")