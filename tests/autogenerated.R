#Testing corr_func
expected = cor(nhanes2$Systolic_BP1, nhanes2$Systolic_BP2, "complete.obs")

expect_that(corr_func('Systolic_BP1', 'Systolic_BP2'), equals(expected ))

#Creating test dataframe
Gender = c('Male', 'Female', 'Male', 'Female')
Drives = c('Yes', 'Yes', 'No', 'No')
Age_Group = c('Adult', 'Adult', 'Teen', 'Teen')
Nums = c(1,3,5,7)
test_df = data.frame(Gender, Drives, Age_Group, Nums)

#Testing grp_func
Gender = c('Female', 'Male')
Mean = c(5, 3)
Max = c(7, 5)
Min = c(3, 1)
expected1 = data.frame(Gender, Mean, Max, Min)
expected1 = as_tibble(expected1)
expected1

expect_that(grp_func(test_df, 'Gender', 'Nums'), equals(expected1))


#Testing prop_func
Age_Group = c('Adult', 'Teen')
Drives = c('Yes', 'No')
Male_Proportion = c('100%', '100%')
Female_Proportion = c('100%', '100%')
Total_Proportion = c('100%', '100%')
expected2 = data.frame(Age_Group, Drives, Male_Proportion, Female_Proportion, Total_Proportion)
expected2 = as_tibble(expected2)
expected2$Male_Proportion = as.character(expected2$Male_Proportion)
expected2$Female_Proportion = as.character(expected2$Female_Proportion)
expected2$Total_Proportion = as.character(expected2$Total_Proportion)
expected2

expect_that(prop_func(data = test_df, group_var = 'Age_Group', analyse_var = 'Drives', percentage = TRUE), equals(expected2))

