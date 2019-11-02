#Import data set
nhanes = read.csv('/Users/carrell/Desktop/nhanes_2015_2016.txt')
nhanes2 = nhanes
#Import libraries
library(ggplot2)
library(dplyr)
library(utils)
install.packages("corrplot")
library(corrplot)
library(gridExtra)



#Rename columns - created a new datas et incase I need to revert back to original
nhanes2 = nhanes %>% rename(Gender = RIAGENDR, Age = RIDAGEYR, Race = RIDRETH1, Education = DMDEDUC2,
                            Household_Size = DMDHHSIZ, Income_to_Pov = INDFMPIR, Smoked_100 = SMQ020x, 
                            Systolic_BP1 = BPXSY1, Systolic_BP2 = BPXSY2, 
                            Diastolic_BP1 = BPXDI1, Diastolic_BP2 = BPXDI2,
                            Alcohol_Year = ALQ101, Marital_Status = DMDMARTL)

#Re-lable some of the catagorical variables in order to visualise more clearly
nhanes2$Gender = recode(nhanes2$RIAGENDRx, '1' = "Male", '2' = 'Female')
nhanes2$Gender = as.factor(nhanes2$Gender) 

nhanes2$Alcohol_Year = recode(nhanes2$ALQ101x, '1' = "Yes", '2' = 'No', '7' = 'Refused', '9' = "Don't know")
nhanes2$Alcohol_Year = as.factor(nhanes2$Alcohol_Year) #Change from numeric to factor

nhanes2$Smoked_100 = recode(nhanes2$Smoked_100, '1' = 'Yes', '2' = 'No', '7' = 'Refused', '9' = "Don't know")
nhanes2$Smoked_100 = as.factor(nhanes2$Smoked_100) #Change from numeric to factor

nhanes2$Race = recode(nhanes2$Race, '1' ='Mexican American', '2' = 'Other Hispanic', 
                          '3'= 'Non-Hispanic White', '4'= 'Non-Hispanic Black', '5'= 'Other Race')
nhanes2$Race = as.factor(nhanes2$Race) #Change from numeric to factor

nhanes2$Marital_Status = recode(nhanes2$Marital_Status, '1' = 'Married', '2'= 'Widowed', '3'= 'Divorced',
                          '4'= 'Separated', '5'= 'Never Married', '6'= 'Living with partner')
nhanes2$Marital_Status = as.factor(nhanes2$Marital_Status) #Change from numeric to factor




#Create a near variable 'agegroup' in order to display ages into groups for easier and clearer analysis

setDT(nhanes2)[Age >17 & Age <30, agegroup := "18-29"]
nhanes2[Age >29 & Age <40, agegroup := "30-39"]
nhanes2[Age >39 & Age <50, agegroup := "40-49"]
nhanes2[Age >49 & Age <60, agegroup := "50-59"]
nhanes2[Age >59 & Age <70, agegroup := "60-69"]
nhanes2[Age >69 & Age <80, agegroup := "70-80+"]

nhanes2$agegroup
colnames(nhanes2)

p = ggplot(nhanes2, aes(x=agegroup, y=BPXSY1, color=agegroup)) + 
  geom_boxplot()
p

n + scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
n = ggplot(nhanes2, aes(x=Systolic_BP1, y=Systolic_BP2, color=Smoked_100)) + geom_point(alpha=0.5)
n

#o + scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
o = ggplot(nhanes2, aes(x=Systolic_BP1, y=BMXBMI, color = Smoked_100)) + geom_point(alpha=0.5)
o

grid.arrange(
  n, o, nrow=2, top = 'title'
)

length(which(nhanes2$SMQ020x=="Yes"))
length(which(nhanes2$SMQ020x=="Don't know"))
length(nhanes2$SMQ020x)


#Function to take dataset, group_by and analyse variable to output mean, min and max
my_func = function(data, group_var, analyse_var){
  grouped = data %>% group_by(data[[group_var]])
  detail= grouped %>% summarise(Mean=mean(data[[analyse_var]], na.rm = TRUE), Max=max(data[[analyse_var]], na.rm = TRUE), Min=min(data[[analyse_var]], na.rm = TRUE))
  total = data %>% summarise(Mean=mean(data[[analyse_var]], na.rm = TRUE), Max=max(data[[analyse_var]], na.rm = TRUE), Min=min(data[[analyse_var]], na.rm = TRUE))
  print(detail)
  print(total)
}

my_func(nhanes2, 'Gender', 'BMXBMI')


#Find the numeric columns
numeric_vars = unlist(lapply(nhanes2, is.numeric)) 
numeric_data=nhanes2[ , numeric_vars]

#Work out correlations between each of these variables
correlations = cor(numeric_data,use = "complete.obs")

#Plot the correlations to clearly visualise how strongly correlated each comparison is
corrplot(correlations,use = "complete.obs")

#SImple function to quickly work out correlation between 2 variables
corr = function(var1, var2){
  v1 = nhanes2[var1]
  v2 = nhanes2[var2]
  cor(v1, v2, use = 'complete.obs')
}

corr('BMXBMI', 'Systolic_BP1')


lst1 = t(combn(colnames(nhanes2), 2))

lst1

my_func(nhanes2, "Gender", "Age")
by_gender
example(group_by)

