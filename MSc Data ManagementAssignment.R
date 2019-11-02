#Import data set
nhanes = read.csv('/Users/carrell/Desktop/nhanes_2015_2016.txt')
head(nhames)

#Import libraries
library(ggplot2)
library(dplyr)

#Rename columns - created a new datas et incase I need to revert back to original
nhanes2 = nhanes %>% rename(Gender = RIAGENDR, Age = RIDAGEYR, Race = RIDRETH1, Education = DMDEDUC2,
                            Household_Size = DMDHHSIZ, Income_to_Pov = INDFMPIR)

#Re-lable some of the catagorical variables in order to visualise more clearly
nhanes2$Gender = nhanes2$Gender <-ifelse(nhanes2$Gender==1, 'Male', 'Female')

nhanes$ALQ101x=as.character(nhanes$ALQ101)

nhanes2$ALQ101x = recode(nhanes2$ALQ101x, '1' = "Yes", '2' = 'No', '7' = 'Refused', '9' = "Don't know")
nhanes2$SMQ020x = recode(nhanes2$SMQ020, '1' = 'Yes', '2' = 'No', '7' = 'Refused', '9' = "Don't know")

nhanes2$Race = as.character(nhanes2$Race) #Change from numeric to character
nhanes2$Race = recode(nhanes2$Race, '1' ='Mexican American', '2' = 'Other Hispanic', 
                          '3'= 'Non-Hispanic White', '4'= 'Non-Hispanic Black', '5'= 'Other Race')
nhanes2$DMDMARTLx = as.character(nhanes2$DMDMARTL) #Change from numeric to character
nhanes2$DMDMARTLx = recode(nhanes2$DMDMARTLx, '1' = 'Married', '2'= 'Widowed', '3'= 'Divorced',
                          '4'= 'Separated', '5'= 'Never Married', '6'= 'Living with partner')
nhanes2$DMDMARTLx

#Create a near variable 'agegroup' in order to display ages into groups for easier and clearer analysis

setDT(nhanes2)[Age >17 & Age <30, agegroup := "18-29"]
nhanes2[Age >29 & Age <40, agegroup := "30-39"]
nhanes2[Age >39 & Age <50, agegroup := "40-49"]
nhanes2[Age >49 & Age <60, agegroup := "50-59"]
nhanes2[Age >59 & Age <70, agegroup := "60-69"]
nhanes2[Age >69 & Age <80, agegroup := "70-80+"]

nhanes2$agegroup
colnames(nhanes2)



