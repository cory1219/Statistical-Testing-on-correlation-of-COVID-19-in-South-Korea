### 1.Import packages

library(data.table)
library(bit64)
library(ggplot2)
library(magrittr)
library(car) #Levene’s test



### 2.Import dataset

patient <- fread("PatientInfo.csv", na.strings = "")


### 3.Data exploration

summary(patient)
str(patient)


#It's obvious that the data types of age and contact_number are wrong, so we have to change them. Also, they both have many missing values, so we will temporarily remove them for the sake of upcoming data visualization.



#make age a data type of factor
patient[,age:= factor(age, levels = c('0s', '10s', '20s', '30s', '40s', '50s', '60s', '70s', '80s', '90s', '100s'))]

#make contact_number a data type of integer
patient[,contact_number := as.integer(contact_number)]

#remove missing values in the columns of age and contact_number
patient_rm_na <- patient[!is.na(age) & !is.na(contact_number)]


### 4.Data visualization

#create a boxplot to obeserve the association between age and contact_number
ggplot(patient_rm_na, aes(age, contact_number))+
  geom_boxplot()+
  ylim(0,100)


#On the plot, we notice that age could play a role in the number of contact. Also, it appears that young people, which include college students, have a greater number of contact than the elderly. So we further claim that young people have a greater number of contact than the elderly. Therefore, we will statistically test:
#1. the dependence between age and contact_number.
#2. people aged at 20s have significantly higher number of contact than the rest.

### 5.Statistical testing - Kruskal-Wallis rank sum test

#Because age is organized into various groups, we can use one-way ANOVA test to examine if there is any significant difference between the average contact_numbers in the various age groups.  


res.aov <- aov(contact_number ~ factor(age), data = patient)
summary(res.aov)


#Nevertheless, we did not check ANOVA assumptions for our data,which are:
#1. The variance across groups are homogeneous
#2. The data of each factor level are normally distributed
#So let's check the homogenity of variances first.


leveneTest(contact_number ~ age, data = patient)


#From the output above we can see that the p-value is not less than the significance level of 0.05. This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

#Then check the assumption of normality.
#We can check it by plotting Q-Q plot, whose y-xais is the quantiles of the residuals against x-axis of the quantiles of the normal distribution.


plot(res.aov, 2)


#The normal probability plot of residuals is used to check the assumption that the residuals are normally distributed. It should approximately follow a dotted line.

#To double validate the normality of our data, we will run hapiro-Wilk test.


# Extract the residuals
aov_residuals <- residuals(object = res.aov )

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


#From the output above we can see that the p-value is less than the significance level of 0.05. This means that the null hypothesis that each age group is normally distributed is rejected. Therefore, we can confirm that the normality of each factor level does not exist.

#In the case of data without normality, we can turn to another statistical test with less strict assumption. A non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test, which can be used when ANOVA assumptions are not met.


kruskal.test(contact_number ~ age, data = patient)


#The Kruskal test suggests that there is a difference in the number of contact among the age groups since the P-value is lower than 0.05.

#Now we will statistically test if people aged at 20s have significantly higher number of contact than the rest. 


# the distribution of contact_number of patients aged 20s 
ggplot(patient[age == "20s"], aes(contact_number))+
  geom_histogram()
# the distribution of contact_number of patients aged 20s
ggplot(patient[age != "20s"], aes(contact_number))+
  geom_histogram()


#We notice that the two groups do not follow Gaussian distribution, so we will use permutation to test dependence.


#create a function of difference in median
median_dif_age <- function(dt, alt){
  dt[age == alt, median(contact_number, na.rm = T)]-
    dt[age != alt, median(contact_number, na.rm = T)]
}

#test statistics of data 
T_obs <- median_dif_age(patient, "20s")

#create a vector of test statistics of 1000 permutated data
dt_permuted <- copy(patient)
set.seed(0)
T_permuted <- rep(NA, 1000)
for(i in 1:1000){
  # permute the genotype column in place 
  dt_permuted[ , age:=sample(age)]
  # store the difference of medians in the i-th entry of T_permuted
  T_permuted[i] <- median_dif_age(dt_permuted, "20s")
} 

#plot the test statistics of permutated data
ggplot( data.table(T_permuted), aes(x = T_permuted) ) + 
  geom_histogram() + 
  geom_vline( aes(xintercept=T_obs, color = "T_obs") )

#P-value
p_val_20 <- (sum(T_permuted>=T_obs)+1)/1001
p_val_20


#In the permutation test, the P-value is more than 0.05, so we can't claim patients aged at 20s are more likely to have a greater number of contact than the rest groups.

#In conclusion, we prove our claim that age plays a role in the difference in contact_number. However, we cannot validate the fact that young patients(aged at 20s tested) have a greater number of contact than the rest.

### 6.Controlling for the effect of a third variable - infection_case to support a previously stated claim

#Infection_case is like patients behavior indicating how they contact others. So we assume this is an indicator affecting the distribution of contact_number among age groups. Let's observe the categories within infection_case first.


g <- patient[, .(count = .N), by=infection_case]
g$infection_case <- factor(g$infection_case, levels = g$infection_case[order(g$count)])
ggplot(g, aes(infection_case, count))+
  geom_bar(stat = "identity")+
  coord_flip()


#If the purpose of the analysis is to see where patients were infected, it would make sense to consider the use of all values. However, the purpose of this analysis is to find out what behavior of infection was at risk of death. Then, rather than labeling specific places, it is necessary to use this variable to know how infection happened and use it in the model. The values,'contact with patient','overseas inflow' remain the same, while 'unknown(etc)' & NA will be removed. The rest represents detailed cases and will be grouped together, labeling the value of 'group'.


#remove infection case = etc or NA
patient_rm_na <- patient_rm_na[infection_case != "etc" | !is.na(infection_case)]

#create a function of categorizing infection_case
categorize <- function(X){
  if(X == 'overseas inflow'){
    X = "overseas_inflow"
  }else if(X == "contact with patient"){
    X = "contact_with_patient"
  }else{
    X = "contact_in_public_places"
  }
}

#categorize the data
patient_categorized <- copy(patient_rm_na)[, infection_case := sapply(infection_case, categorize)]

#the distribution of infection_case
table(patient_categorized$infection_case)


#Observe the distribution of contact_number of various age groups in the three scenarios of infection_case on the plots

ggplot(patient_categorized[infection_case != "overseas_inflow"], aes(age, contact_number))+
  geom_boxplot()+
  ylim(0,100)+
  facet_wrap(~infection_case)

#The plot seems to show age is associated with contact_number when patients have contacted others in the public places or contacted with patients. So we will test both associations, with infection_case as a third variable.


#check ANOVA assumptions to see if we can use ANOVA or not
leveneTest(contact_number ~ age, data = patient_categorized[infection_case == "contact_in_public_places"])
res.aov.public <- aov(contact_number ~ factor(age), data = patient_categorized[infection_case == "contact_in_public_places"])
aov_residuals.public <- residuals(object = res.aov.public )
shapiro.test(x = aov_residuals.public)
#it turns out that the categorized data does not meet the requirement of Gaussian distribution, so we will use Kruskal-Wallis rank sum test.

kruskal.test(contact_number ~ age, data = patient_categorized[infection_case == "contact_in_public_places"])


#Since P-value is less than 0.05, we can claim that age is associated with contact_number when patients have contacted others in the public places. 

#Then test the association between age and contact_number in the case that patients have ever contacted with patients before. 


#check ANOVA assumptions to see if we can use ANOVA or not
leveneTest(contact_number ~ age, data = patient_categorized[infection_case == "contact_with_patient"])
res.aov.contact.patient <- aov(contact_number ~ factor(age), data = patient_categorized[infection_case == "contact_with_patient"])
aov_residuals.contact.patient <- residuals(object = res.aov.contact.patient )
shapiro.test(x = aov_residuals.contact.patient )
#it turns out that the categorized data does not meet the requirement of Gaussian distribution, so we will use Kruskal-Wallis rank sum test.

kruskal.test(contact_number ~ age, data = patient_categorized[infection_case == "contact_with_patient"])


#Since P-value is less than 0.05, we can claim that age is associated with contact_number when patients have contacted with patients before.

#So we conclude that infection_case as a third variable can support the association between age & contact_number.