
#create.project('Data_Management_CSC8631')
library(ProjectTemplate)
load.project()

nhanes2 = nhanes.2015.2016


#nhanes2 = read.csv('/Users/carrell/Desktop/nhanes_2015_2016.txt')

colnames(nhanes2)


#Import libraries
# library(ggplot2)
# library(dplyr)
# library(utils)
# install.packages("corrplot")
# library(corrplot)
# library(gridExtra)
# library(data.table)
# library(broom)

class(nhanes2)

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
                      '3'= 'Non-Hispanic White', '4'= 'Non-Hispanic Black', 
                      '5'= 'Other Race')
nhanes2$Race = as.factor(nhanes2$Race) #Change from numeric to factor

nhanes2$Marital_Status = recode(nhanes2$Marital_Status, '1' = 'Married', '2'= 'Widowed', 
                                '3'= 'Divorced', '4'= 'Separated', '5'= 'Never Married', 
                                '6'= 'Living with partner')
nhanes2$Marital_Status = as.factor(nhanes2$Marital_Status) #Change from numeric to factor



#Create a near variable 'agegroup' in order to display ages into groups for easier and 
#clearer analysis

setDT(nhanes2)[Age >17 & Age <30, agegroup := "18-29"]
nhanes2[Age >29 & Age <40, agegroup := "30-39"]
nhanes2[Age >39 & Age <50, agegroup := "40-49"]
nhanes2[Age >49 & Age <60, agegroup := "50-59"]
nhanes2[Age >59 & Age <70, agegroup := "60-69"]
nhanes2[Age >69 & Age <81, agegroup := "70-80+"]

nhanes2$agegroup
colnames(nhanes2)

b1 = ggplot(nhanes2, aes(x=agegroup, y=Systolic_BP1, color=Gender)) + 
  geom_boxplot()
b1

nhanes2$Education = as.factor(nhanes2$Education)
b2 = ggplot(nhanes2, aes(x=Education, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()
b2

nhanes2$Household_Size = as.factor(nhanes2$Household_Size)
b3 = ggplot(nhanes2, aes(x=Household_Size, y=Income_to_Pov, color=Household_Size)) + 
  geom_boxplot()
b3


b4 = ggplot(nhanes2, aes(x=Race, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()
b4

summary(nhanes2$Income_to_Pov)
colnames(nhanes2)
class(nhanes2$Household_Size)

#s1 + scale_color_manual(breaks = c('Male', 'Female'), values = c('blue', 'red'))
s1 = ggplot(nhanes2, aes(x=BMXWAIST, y=BMXBMI, color=Gender)) + geom_point(alpha=0.5) 
s1

#o + scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
s2 = ggplot(nhanes2, aes(x=Systolic_BP1, y=BMXBMI, color = Smoked_100)) + geom_point(alpha=0.5)
s2

s3 = ggplot(nhanes2, aes(x=BMXBMI, y=Age, color = Smoked_100)) + geom_point(alpha=0.5)
s3

s4 = ggplot(nhanes2, aes(x=Age, y=Systolic_BP1, color = Smoked_100)) + geom_point(alpha=0.5)
s4

grid.arrange(
  s1, s2, s3, s4, nrow=2, ncol = 2, top = 'title'
)



#Function to take dataset, group_by and analyse variable to output mean, min and max
my_func = function(data, group_var, analyse_var){
  grouped = data %>% group_by(data[[group_var]])
  detail= grouped %>% summarise(Mean=mean(.data[[analyse_var]], na.rm = TRUE), Max=max(.data[[analyse_var]], na.rm = TRUE), Min=min(.data[[analyse_var]], na.rm = TRUE))
  total = data %>% summarise(Mean=mean(.data[[analyse_var]], na.rm = TRUE), Max=max(.data[[analyse_var]], na.rm = TRUE), Min=min(.data[[analyse_var]], na.rm = TRUE))
  print(detail)
  print(total)
}

my_func(nhanes2, 'Gender', 'BMXBMI')


colnames(nhanes2)

#Find the numeric columns
numeric_vars = unlist(lapply(nhanes2, is.numeric))
numeric_vars

numeric_data = nhanes2[, ..numeric_vars]
numeric_data

#Work out correlations between each of these variables
correlations = cor(numeric_data1,use = "complete.obs")
correlations

#Plot the correlations to clearly visualise how strongly correlated each comparison is
corrplot(correlations)

#SImple function to quickly work out correlation between 2 variables
corr = function(var1, var2){
  v1 = nhanes2[[var1]]
  v2 = nhanes2[[var2]]
  cor(v1, v2, use = 'complete.obs')
}

corr('Systolic_BP1', 'Systolic_BP2')

class(nhanes2$Diastolic_BP2)

lst1 = t(combn(colnames(nhanes2), 2))

lst1

my_func(nhanes2, "Gender", "Age")
by_gender
example(group_by)

lm.res <- summary(lm(BMXWAIST~BMXBMI,data=nhanes2))
lm.res

colnames(nhanes2)

lm(BMXHT~BMXBMI,data=nhanes2)

#Hypoth testing
#1. Person in 50s has higher systolic BP to person in 20s
#2. Somone with degree likely to earn more than someone with high school education
#3. Someone with larger household size will typically earn less than somone in a small 
#household size