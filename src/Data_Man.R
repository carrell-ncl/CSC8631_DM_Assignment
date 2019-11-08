setwd('/Users/carrell/Desktop/MSc/Data Management/CSC8631_DM_Assignment')
#create.project('Data_Management_CSC8631')
library(ProjectTemplate)
load.project()

#Input details of the dataset.
nhanes2 = nhanes.2015.2016

#nhanes2 = read.csv('/Users/carrell/Desktop/nhanes_2015_2016.txt')

colnames(nhanes2)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
#install.packages("ggpubr")
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

nhanes2$Education = as.factor(nhanes2$Education) #Change to factor
nhanes2$Education = recode(nhanes2$Education, '1' = '< 9th grade', '2' = '9-11th grade',
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

nhanes2$agegroup
colnames(nhanes2)

#Find the numeric columns
numeric_vars = unlist(lapply(nhanes2, is.numeric))
numeric_vars

numeric_data = nhanes2[, ..numeric_vars]
numeric_data

#Work out correlations between each of these variables
correlations = cor(numeric_data,use = "complete.obs")
correlations

#Plot the correlations to clearly visualise how strongly correlated each comparison is
corrplot(correlations)

#Simple function to quickly work out correlation between 2 variables
corr = function(var1, var2){
  v1 = nhanes2[[var1]]
  v2 = nhanes2[[var2]]
  cor(v1, v2, use = 'complete.obs')
}

corr('Systolic_BP1', 'Systolic_BP2')

#Function to take dataset, group_by and analyse variable to output mean, min and max
grp = function(data, group_var, analyse_var){
  grouped = data %>% group_by(data[[group_var]])
  detail= grouped %>% summarise(Mean=mean(.data[[analyse_var]], na.rm = TRUE), Max=max(.data[[analyse_var]], na.rm = TRUE), Min=min(.data[[analyse_var]], na.rm = TRUE))
  total = data %>% summarise(Mean=mean(.data[[analyse_var]], na.rm = TRUE), Max=max(.data[[analyse_var]], na.rm = TRUE), Min=min(.data[[analyse_var]], na.rm = TRUE))
  print(detail)
  print(total)
}

#Box plots

b1 = ggplot(nhanes2, aes(x=agegroup, y=Systolic_BP1, color=Gender)) + 
  geom_boxplot()
b1_test = filter(nhanes2, Age>69)
grp(b1_test, 'Gender', 'Systolic_BP1')
#2 hypothesis test. 1 to see if men of all ages have higher Systolic BP than females, and 
#another to test if females aged 70+ have a higher Systolic BP than men.

#Filter dataframe to remove below value
df2 = filter(nhanes2, Education !='9')
b2 = ggplot(df2, aes(x=Education, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()
b2

nhanes2$Household_Size = as.factor(nhanes2$Household_Size)
b3 = ggplot(nhanes2, aes(x=Household_Size, y=Income_to_Pov, color=Household_Size)) + 
  geom_boxplot()
b3


b4 = ggplot(nhanes2, aes(x=Race, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()
b4

#Scatter plots to visualise come of the correlations

s1 = ggplot(nhanes2, aes(x=BMXWAIST, y=BMXBMI, color=Gender)) + geom_point(alpha=0.5)
s1
corr('BMXWAIST', 'BMXBMI')
grp(nhanes2, 'Gender', 'BMXBMI')

#Filter dataframe to remove below values
df1 = filter(nhanes2, Smoked_100 !="Don't know" & Smoked_100 !='Refused')

s2 + scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
s2 = ggplot(df1, aes(x=Systolic_BP1, y=Systolic_BP2, color = Smoked_100)) + geom_point(alpha=0.5)
corr('Systolic_BP1', 'Systolic_BP2')

s3 = ggplot(df1, aes(x=Systolic_BP1, y=BMXBMI, color = Smoked_100)) + geom_point(alpha=0.5)
s3 + scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
corr('Systolic_BP1', 'BMXBMI')

s4 = ggplot(df1, aes(x=Age, y=Systolic_BP1, color = Smoked_100)) + geom_point(alpha=0.5)
s4 + scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
corr('Age', 'Systolic_BP1')

grid.arrange(
  s1, s2, s3, s4, nrow=2, ncol = 2, top = 'title'
)


#Hypothesis testing
#T-test to see if male who has smoked 100 cigarettes in his live and had at least 12 
#alcoholic drinks in 1 year has a higher Systolic blood pressure than a male who has not.
#H0 != H1

t1 = filter(nhanes2, Gender=='Male' & Smoked_100=='Yes' & Alcohol_Year=='Yes')
t2 = filter(nhanes2, Gender=='Male' & Smoked_100=='No' & Alcohol_Year=='No')

#Test using the t.test function
t.test(t1$Systolic_BP1, t2$Systolic_BP1, alternative = "two.sided", 
       var.equal = FALSE, paired = FALSE)
#Test manually. As the std_dev on each sample are not similar, I have used the 
#unpooled approach
n1 = nrow(t1)
n2 = nrow(t2)
u1 = mean(t1$Systolic_BP1, na.rm = TRUE)
u2 = mean(t2$Systolic_BP1, na.rm = TRUE)
sig1 = sd(t1$Systolic_BP1, na.rm = TRUE)
sig2 = sd(t2$Systolic_BP1, na.rm = TRUE)

t_test = (u1-u2)/sqrt(((sig1^2)/n1)+((sig2^2)/n2))
t_test

#t-score for inbuilt_funtion = 4.6317, t-score worked out manually is 4.73. Both give a 
#p-value of <0.00001 which provides us with sufficient evidence to reject the null hypothesis 
#and say that a male who has drank and smoked does have a higher systolic blood pressure than
#a male who has not. We can futher back this up by looking at the confidence intervals 
#(2.7, 6.67) and seeing that 0 does not fall within this.

#Hypothesis test 2.1 - Do men of all ages have higher Systolic BP than women.

#Hypothesis test 3
#Do females have a higher BMI than men



#Hypoth testing
#CHECK ASSUMPTIONS
#UNPOOLED - Variance is not equal
#1. Person in 50s has higher systolic BP to person in 20s
#2. Somone with degree likely to earn more than someone with high school education
#3. Someone with larger household size will typically earn less than somone in a small 
#4. People who have smoked 100 in their life have higher SBP than someone who doesn't

##Findings
#People in larger houserholds have a lower income to poverty ratio on average
#White-Americans have the highest income to poverty ratio whist Mexican-American's have the lowest.
#Systolic blood pressure increases with age. Males have on average higher BP than females
#until 70+ years where males appear to have a lower BP
#Females appear to have higher BMI than men