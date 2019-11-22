nhanes2 = nhanes.2015.2016

#Rename columns - created a new datas et incase I need to revert back to original
nhanes2 = nhanes2 %>% rename(Gender = RIAGENDR, Age = RIDAGEYR, Race = RIDRETH1, 
                             Education = DMDEDUC2, Household_Size = DMDHHSIZ, 
                             Income_to_Pov = INDFMPIR, Smoked_100 = SMQ020, 
                             Systolic_BP1 = BPXSY1, Systolic_BP2 = BPXSY2, 
                             Diastolic_BP1 = BPXDI1, Diastolic_BP2 = BPXDI2,
                             Alcohol_Year = ALQ101, Marital_Status = DMDMARTL)



#Remove columns which I won't be using
nhanes2$ALQ110 = NULL
nhanes2$ALQ130 = NULL
nhanes2$DMDCITZN = NULL

colnames(nhanes2)

#Re-lable some of the catagorical variables in order to visualise more clearly
#nhanes2$Gender = as.character(nhanes2$Gender)
nhanes2$Gender = recode(nhanes2$Gender, '1' = "Male", '2' = 'Female')
nhanes2$Gender = as.factor(nhanes2$Gender) 

nhanes2$Alcohol_Year = recode(nhanes2$Alcohol_Year, '1' = "Yes", '2' = 'No', '7' = 'Refused', 
                              '9' = "Don't know")
nhanes2$Alcohol_Year = as.factor(nhanes2$Alcohol_Year) #Change from numeric to factor

nhanes2$Smoked_100 = recode(nhanes2$Smoked_100, '1' = 'Yes', '2' = 'No', '7' = 'Refused', 
                            '9' = "Don't know")
nhanes2$Smoked_100 = as.factor(nhanes2$Smoked_100) #Change from numeric to factor

nhanes2$Race = recode(nhanes2$Race, '1' ='Mexican American', '2' = 'Other Hispanic', 
                      '3'= 'White', '4'= 'Black', 
                      '5'= 'Other Race')
nhanes2$Race = as.factor(nhanes2$Race) #Change from numeric to factor

nhanes2$Marital_Status = as.factor(nhanes2$Marital_Status) #Change from numeric to factor
nhanes2$Marital_Status = recode(nhanes2$Marital_Status, '1' = 'Married', '2'= 'Widowed', 
                                '3'= 'Divorced', '4'= 'Separated', '5'= 'Never Married', 
                                '6'= 'Living with partner')

nhanes2$EducationX = as.factor(nhanes2$Education) #Create as a new variable in order to 
#analyse the correlation of the original (numeric)
nhanes2$EducationX = recode(nhanes2$EducationX, '1' = '< 9th grade', '2' = '9-11th grade',
                            '3' = 'High school graduate', '4' = 'Some college/Uni',
                            '5' = 'College/Uni graduate or above')



#Create a near variable 'agegroup' in order to display ages into groups for easier and 
#clearer analysis
setDT(nhanes2)[Age >17 & Age <30, agegroup := "18-29"]
nhanes2[Age >29 & Age <40, agegroup := "30-39"]
nhanes2[Age >39 & Age <50, agegroup := "40-49"]
nhanes2[Age >49 & Age <60, agegroup := "50-59"]
nhanes2[Age >59 & Age <70, agegroup := "60-69"]
nhanes2[Age >69 & Age <81, agegroup := "70-80+"]

nhanes2$agegroup = as.factor(nhanes2$agegroup) #Change to factor
nhanes2$Household_Size = as.factor((nhanes2$Household_Size))

nhanes2$agegroup = as.factor(nhanes2$agegroup) #Change to factor

#Adding a new variable (Income_Group) for better analysis
setDT(nhanes2)[Income_to_Pov >=0 & Income_to_Pov  <1, Income_Group := "1"]
nhanes2[Income_to_Pov  >=1 & Income_to_Pov  <2, Income_Group := "2"]
nhanes2[Income_to_Pov >=2 & Income_to_Pov <3, Income_Group := "3"]
nhanes2[Income_to_Pov >=3 & Income_to_Pov <4, Income_Group := "4"]
nhanes2[Income_to_Pov >=4 & Income_to_Pov <5, Income_Group := "5"]
nhanes2$Income_Group = as.factor(nhanes2$Income_Group)

nhanes2$agegroup = as.factor(nhanes2$agegroup) #Change to factor
nhanes2$Household_Size = as.factor((nhanes2$Household_Size))