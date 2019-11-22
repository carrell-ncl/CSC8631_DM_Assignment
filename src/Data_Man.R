setwd('/Users/carrell/Desktop/MSc/Data Management/CSC8631_DM_Assignment')
#create.project('Data_Management_CSC8631')
library(ProjectTemplate)
load.project()
library(kableExtra)
#install.packages("kableExtra")

#Input details of the dataset.
nhanes2 = nhanes.2015.2016
colnames(nhanes2)
getwd()
#library(DT)

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
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

setDT(nhanes2)[Income_to_Pov >=0 & Income_to_Pov  <1, Income_Group := "1"]
nhanes2[Income_to_Pov  >=1 & Income_to_Pov  <2, Income_Group := "2"]
nhanes2[Income_to_Pov >=2 & Income_to_Pov <3, Income_Group := "3"]
nhanes2[Income_to_Pov >=3 & Income_to_Pov <4, Income_Group := "4"]
nhanes2[Income_to_Pov >=4 & Income_to_Pov <5, Income_Group := "5"]
nhanes2$Income_Group = as.factor(nhanes2$Income_Group)

nhanes2$agegroup = as.factor(nhanes2$agegroup) #Change to factor
nhanes2$Household_Size = as.factor((nhanes2$Household_Size))

colnames(nhanes2)

#*****End of wrangling*****

#Find the numeric columns
numeric_vars = unlist(lapply(nhanes2, is.numeric))
numeric_vars

numeric_data = nhanes2[, ..numeric_vars]
class(numeric_data)

#Work out correlations between each of these variables
correlations = cor(numeric_data,use = "complete.obs")
correlations

#Plot the correlations to clearly visualise how strongly correlated each comparison is
corrplot(correlations)

#Simple function to quickly work out correlation between 2 variables
corr_func = function(var1, var2){
  v1 = nhanes2[[var1]]
  v2 = nhanes2[[var2]]
  cor(v1, v2, use = 'complete.obs')
}



#Function to take dataset, group_by and analyse variable to output mean, min and max
grp_func = function(data, group_var, analyse_var){
  grouped = data %>% group_by(data[[group_var]])
  detail= grouped %>% summarise(Mean=mean(.data[[analyse_var]], na.rm = TRUE), Max=max(.data[[analyse_var]], na.rm = TRUE), Min=min(.data[[analyse_var]], na.rm = TRUE))
  total = data %>% summarise(Mean=mean(.data[[analyse_var]], na.rm = TRUE), Max=max(.data[[analyse_var]], na.rm = TRUE), Min=min(.data[[analyse_var]], na.rm = TRUE))
  names(detail)[1]=group_var
  return(detail)
  #print(total)
}


#library(scales) #Import library to convert to percentage

#Fuction that takes the following aruguments: dataframe, catagorical group variable, and binary variable to analyse. Then outputs 
#a new dataframe of  proportions based on these arguments (split by gender)
prop_func = function(data, group_var, analyse_var, percentage=TRUE){
  data = filter(data, .data[[analyse_var]]!="Don't know" & .data[[analyse_var]]!= 'Refused' & .data[[analyse_var]]!='9')
  data2 = data %>% filter(!is.na(.data[[group_var]])) %>% filter(.data[[group_var]]!='77' & .data[[group_var]]!='9')
  pro_tab0 = data2 %>%
    count(.data[[group_var]], na.rm = TRUE, .data[[analyse_var]], na.rm = TRUE) %>%
    mutate(Proportion = prop.table(n))
  
  male = filter(data2, Gender=='Male')
  pro_tab1 = male %>%
    count(.data[[group_var]], na.rm = TRUE, .data[[analyse_var]], na.rm = TRUE) %>%
    mutate(Proportion = prop.table(n))
  
  if (percentage==TRUE){
    pro1 = pro_tab1 %>% group_by(pro_tab1[[group_var]]) %>% transmute(.data[[analyse_var]], Male_Proportion = percent(n/sum(n)))
    pro0 = pro_tab0 %>% group_by(pro_tab1[[group_var]]) %>% transmute(.data[[analyse_var]], Total_Proportion = percent(n/sum(n)))
  }
  else{
    pro1 = pro_tab1 %>% group_by(pro_tab1[[group_var]]) %>% transmute(.data[[analyse_var]], Male_Proportion = n/sum(n))
    pro0 = pro_tab0 %>% group_by(pro_tab1[[group_var]]) %>% transmute(.data[[analyse_var]], Total_Proportion = n/sum(n))
  }
  female = filter(data2, Gender=='Female')
  pro_tab2 = female %>%
    count(.data[[group_var]],na.rm = TRUE, .data[[analyse_var]], na.rm = TRUE) %>%
    mutate(Proportion = prop.table(n))
  if (percentage==TRUE){
    pro2 = pro_tab2 %>% group_by(pro_tab2[[group_var]]) %>% transmute(.data[[analyse_var]], Female_Proportion = percent(n/sum(n)))
  }
  else {
    pro2 = pro_tab2 %>% group_by(pro_tab2[[group_var]]) %>% transmute(.data[[analyse_var]], Female_Proportion = n/sum(n))
  }
  
  pro3 = cbind(pro1, Female_Proportion = pro2$Female_Proportion)
  pro3 = cbind(pro3, Total_Proportion = pro0$Total_Proportion)
  names(pro3)[1]=group_var
  return(pro3)
}
  

dat = prop_func(nhanes2, 'agegroup', 'Smoked_100')
datatable(dat)

smoked = prop_func(nhanes2, 'agegroup', 'Smoked_100', percentage = FALSE)


p1 = smoked[1:2,]
p2 = smoked[3:4,]
p3 = smoked[5:6,]
p4 = smoked[7:8,]
p5 = smoked[9:10,]
p6 = smoked[11:12,]

pie1 = ggplot(p1, aes(x='', y=Total_Proportion, fill =Smoked_100 )) + 
  geom_bar(stat='identity', width=1) + labs(title = 'Age - 18-29') + 
  coord_polar('y', start=0) + 
  geom_text(aes(label = percent(Total_Proportion)), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_void()

pie1

pie2 = ggplot(p2, aes(x='', y=Total_Proportion, fill =Smoked_100 )) + 
  geom_bar(stat='identity', width=1) + labs(title = 'Age - 30-39') + 
  coord_polar('y', start=0) + 
  geom_text(aes(label = percent(Total_Proportion)), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_void()

pie2

pie3 = ggplot(p3, aes(x='', y=Total_Proportion, fill =Smoked_100)) + 
  geom_bar(stat='identity', width=1) + labs(title = 'Age - 40-49') + 
  coord_polar('y', start=0) + 
  geom_text(aes(label = percent(Total_Proportion)), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_void()

pie3

pie4 = ggplot(p4, aes(x='', y=Total_Proportion, fill =Smoked_100)) + 
  geom_bar(stat='identity', width=1) + labs(title = 'Age - 50-59') + 
  coord_polar('y', start=0) + 
  geom_text(aes(label = percent(Total_Proportion)), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_void()

pie5 = ggplot(p5, aes(x='', y=Total_Proportion, fill =Smoked_100)) + 
  geom_bar(stat='identity', width=1) + labs(title = 'Age - 60-69') + 
  coord_polar('y', start=0) + 
  geom_text(aes(label = percent(Total_Proportion)), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_void()

pie6 = ggplot(p6, aes(x='', y=Total_Proportion, fill =Smoked_100)) + 
  geom_bar(stat='identity', width=1) + labs(title = 'Age - 70-80+') + 
  coord_polar('y', start=0) + 
  geom_text(aes(label = percent(Total_Proportion)), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_void()


grid.arrange(
  pie1, pie2, pie3, pie4, pie5, pie6, nrow=2, ncol = 3, top = 'Proportion of people who have smoked 100 cigarettes'
)

ed_smoke = prop_func(nhanes2, "EducationX", "Smoked_100")
ed_smoke

#testing = filter(nhanes2, agegroup == '18-29' & Smoked_100 !="Don't know" & Smoked_100 !='Refused')
summary(nhanes2)
#Box plots
b1 = ggplot(nhanes2, aes(x=agegroup, y=Systolic_BP1, color=Gender)) + 
  geom_boxplot()
b1
b1_test = filter(nhanes2, Age>69)
b1_fun = grp_func(b1_test, 'Gender', 'Systolic_BP1')
b1_fun
#Hypothesis test. 1 to see if men of all ages have higher Systolic BP than females, and 
#another to test if females aged 70+ have a higher Systolic BP than men. (test below)

#Filter dataframe to remove below value
df2 = filter(nhanes2, EducationX !='9')
b2 = ggplot(df2, aes(x=EducationX, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()
b2

nhanes2$Household_Size = as.factor(nhanes2$Household_Size)
b3 = ggplot(nhanes2, aes(x=Household_Size, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()
b3
#Hypothesis test to see if females living alone earn more than men living on their own 
#(test below)


b4 = ggplot(nhanes2, aes(x=Race, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()
b4

#Scatter plots to visualise some of the correlations

#Filter dataframe to remove below values
df1 = filter(nhanes2, Smoked_100 !="Don't know" & Smoked_100 !='Refused')

s1 = ggplot(df1, aes(x=Systolic_BP1, y=Systolic_BP2, color = Smoked_100)) + 
  geom_point(alpha=0.2) + 
  scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
s1
helper.corr_func('Systolic_BP1', 'Systolic_BP2')

s2 = ggplot(nhanes2, aes(x=BMXWT, y=Systolic_BP1, color=Gender)) + 
  geom_point(alpha=0.2) +
  scale_color_manual(breaks = c('Male', 'Female'), values = c('blue', 'red'))
s2
corr_func('BMXWT', 'Systolic_BP1')
grp_func(nhanes2, 'Gender', 'BMXBMI')


s3 = ggplot(df1, aes(x=BMXBMI, y=Systolic_BP1, color = Smoked_100)) + 
  geom_point(alpha=0.2) + 
  scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
s3
corr_func('Systolic_BP1', 'BMXBMI')

s4 = ggplot(df1, aes(x=Age, y=Systolic_BP1, color = Smoked_100)) + 
  geom_point(alpha=0.2) + 
  scale_color_manual(breaks = c('Yes', 'No'), values = c('blue', 'red'))
s4
corr_func('Age', 'Systolic_BP1')

grid.arrange(
  s1, s2, s3, s4, nrow=2, ncol = 2, top = 'Testing some correlations'
)


#Hypothesis testing - all below tests based on significance level of 0.05

#T-test to see if male who has smoked 100 cigarettes in his live and had at least 12 
#alcoholic drinks in 1 year has a higher Systolic blood pressure than a male who has not.
#H0 != H1

test1_1 = filter(nhanes2, Gender=='Male' & Smoked_100=='Yes' & Alcohol_Year=='Yes')
test1_2 = filter(nhanes2, Gender=='Male' & Smoked_100=='No' & Alcohol_Year=='No')

#Test using the t.test function
t.test(test1_1$Systolic_BP1, test1_2$Systolic_BP1, alternative = "two.sided", 
       var.equal = FALSE, paired = FALSE)
#Test manually. As the std_dev on each sample are not similar, I have used the 
#unpooled approach
t1_n1 = nrow(test1_1)
t1_n2 = nrow(test1_2)
t1_u1 = mean(test1_1$Systolic_BP1, na.rm = TRUE)
t1_u2 = mean(test1_2$Systolic_BP1, na.rm = TRUE)
t1_sig1 = sd(test1_1$Systolic_BP1, na.rm = TRUE)
t1_sig2 = sd(test1_2$Systolic_BP1, na.rm = TRUE)

t_test = (t1_u1-t1_u2)/sqrt(((t1_sig1^2)/t1_n1)+((t1_sig2^2)/t1_n2))
t_test

#t-score for inbuilt_funtion = 4.6317, t-score worked out manually is 4.73. Both give a 
#p-value of <0.00001 which provides us with sufficient evidence to reject the null hypothesis 
#and say that a male who has drank and smoked does have a higher systolic blood pressure than
#a male who has not. We can futher back this up by looking at the confidence intervals 
#(2.7, 6.67) and seeing that 0 does not fall within this. Will run similar test for only the 
#alcohol part of this (below)

#T-test to see if somoen who has at least 12 alcoholic drinks in a year has a higher Systolic 
#BP than someone who has not.

test2_1 = filter(nhanes2, Gender=='Male' & Alcohol_Year=='Yes')
test2_2 = filter(nhanes2, Gender=='Male' & Alcohol_Year=='No')

#As the variance is similar (testing below), we will use thr pooled approach
sd(test2_1$Systolic_BP1, na.rm = TRUE)
sd(test2_2$Systolic_BP1, na.rm = TRUE)


t.test(test2_1$Systolic_BP1, test2_2$Systolic_BP1, alternative = 'two.sided', var.equal = TRUE)

#P-value of 0.02299 still provides us with enough evidence to reject the null and conclude that
#'on average' people who drink 12 alcoholic drinkls per year have a higher systolic BP than
#those tho don't

#Hypothesis test 3 - Do men of all ages have higher Systolic BP than women.
test3_1 = filter(nhanes2, Gender == 'Male')
test3_2 = filter(nhanes2, Gender == 'Female')

#Testing variancwe below. As the variance differs, we will use the unpooled approach
sd(test3_1$Systolic_BP1, na.rm = TRUE)
sd(test3_2$Systolic_BP1, na.rm = TRUE)

t.test(test3_1$Systolic_BP1, test3_2$Systolic_BP1, alternative = 'two.sided', var.equal = FALSE)

#Hypothesis test to see if women >69 have a higher Systolic BP than men of the similar age
test4_1 = filter(nhanes2, Gender == 'Female' & Age > 69)
test4_2 = filter(nhanes2, Gender == 'Male' & Age > 69)

#Testing variancwe below. As the variance is similar, the pooled approach will be used
sd(test4_1$Systolic_BP1, na.rm = TRUE)
sd(test4_2$Systolic_BP1, na.rm = TRUE)

t.test(test4_1$Systolic_BP1, test4_2$Systolic_BP1, alternative = 'two.sided', var.equal = TRUE)

#Hypothesis test to see if females living alone earn more than males living alone.
test5_1 = filter(nhanes2, Gender == 'Female' & Household_Size == '1')
test5_2 = filter(nhanes2, Gender == 'Male' & Household_Size == '1')

#Signifincant difference in variance, so we will used the unpooled approach
sd(test5_1$Income_to_Pov, na.rm = TRUE)
sd(test5_2$Income_to_Pov, na.rm = TRUE)

t.test(test5_1$Income_to_Pov, test5_2$Income_to_Pov, alternative = 'two.sided', var.equal = FALSE)

#With a p-value of 0.9063, we fail to reject the null hypothesis based on a significance 
#level of 0.05. Confidence level also includes 0.


# After some further analysis using the created Shiny dashboard, something interesting was 
# found. Using the interactive box plots we can see that someone who has smoked 100 cigarettes 
# in their life will on average earn less than someone who hasn't. Even more interesting, 
# someone who drinks at least 12 alcoholic drinks per year will earn more on average than 
# somoene who has not (illistrated by the first 2 plots below). Finally the 3rd box plot 
# shows that someone who drinks 12 alcoholic beverages in a year and who has never smoked 100 has a significantly higher income to poverty ratio (on average) than someone who smoked and does not drink.
# Based on these finding a hypothesis test will be carried out based on the first 2 plots 
# to see if this is significant enough.

df1 = filter(nhanes2, Smoked_100 !="Don't know" & Smoked_100 !='Refused' & Alcohol_Year!="Don't know" & Smoked_100 !='Refused')
b5 = ggplot(df1, aes(x=Smoked_100, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()

b6 = ggplot(df1, aes(x=Alcohol_Year, y=Income_to_Pov, color=Gender)) + 
  geom_boxplot()

b7 = ggplot(df1, aes(x=Alcohol_Year, y=Income_to_Pov, color=Smoked_100)) + 
  geom_boxplot()
b7

# 2 hypothesis tests. First to see if someone who has smoked 100 earns less than someone who 
# has not, and second to see if someone who drinks 12 in a year earns more than somone who 
# does not. If the results fail to reject the null, a further test will be carried out to see 
# is someone who has not smoked 100 but who does drink at least 12 alcoholic beverages in a 
# year earns more than somoen who smoked 100 and does not drink.

test6_1 = filter(nhanes2, Smoked_100 == 'Yes')
test6_2 = filter(nhanes2, Smoked_100 == 'No')

#Difference in variance, so we will used the unpooled approach
sd(test6_1$Income_to_Pov, na.rm = TRUE)
sd(test6_2$Income_to_Pov, na.rm = TRUE)

t.test(test6_1$Income_to_Pov, test6_2$Income_to_Pov, alternative = 'two.sided', var.equal = FALSE)

test7_1 = filter(nhanes2, Alcohol_Year == 'Yes')
test7_2 = filter(nhanes2, Alcohol_Year == 'No')

#Difference in variance, so we will used the unpooled approach
sd(test7_1$Income_to_Pov, na.rm = TRUE)
sd(test7_2$Income_to_Pov, na.rm = TRUE)

t.test(test7_1$Income_to_Pov, test7_2$Income_to_Pov, alternative = 'two.sided', var.equal = FALSE)



b8 = ggplot(nhanes2, aes(x=Income_Group, y=Systolic_BP1, color=Gender)) + 
  geom_boxplot()
b8

test8_1 = filter(nhanes2, EducationX == '< 9th grade')
test8_2 = filter(nhanes2, EducationX == 'College/Uni graduate or above')

#Difference in variance, so we will used the unpooled approach
sd(test8_1$Systolic_BP1, na.rm = TRUE)
sd(test8_2$Systolic_BP1, na.rm = TRUE)

t.test(test8_1$Income_to_Pov, test8_2$Income_to_Pov, alternative = 'two.sided', var.equal = FALSE)


test9_1 = filter(nhanes2, Household_Size == '1')
test9_2 = filter(nhanes2, Household_Size == '2')

#Variancxe very similar, so we will used the pooled approach
sd(test9_1$Systolic_BP1, na.rm = TRUE)
sd(test9_2$Systolic_BP1, na.rm = TRUE)

t.test(test9_1$Income_to_Pov, test9_2$Income_to_Pov, alternative = 'two.sided', var.equal = TRUE)
