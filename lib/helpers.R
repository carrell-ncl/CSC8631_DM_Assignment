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
  #print(total)
}


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
  
