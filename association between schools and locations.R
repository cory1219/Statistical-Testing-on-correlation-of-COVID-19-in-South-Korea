library(data.table)
install.packages('bit64')
library(bit64)
install.packages("ggplot2")
library(ggplot2)
library(ggrepel)
library(magrittr)

patient <- fread("PatientInfo.csv", na.strings = "")
region <- fread("Region.csv")

#Analysis of corrleation between the number of schools in a province and patients 

##### 1. Group by the column of province from patient dataset and count the number 
#of patients in each province. Also, group by the column of province from region 
#dataset and count the number of schools, namely kindergardens, elementary schools, 
#and universities, in each province.

province_patient <- patient[, .(patient_count = .N), by = province]
province_region <- region[province == city, .(province, elementary_school_count, kindergarten_count, university_count)]

###### 2. Merge the above transformed data tables into one table and melt it, 
#creating a variable of school and a value of school count.

merge_province_patient <- merge(province_patient, province_region)
melt_province_patient <- melt(merge_province_patient, id.vars = c("province", "patient_count"), variable.name = "school", value.name = "school_count")

###### 3. Create a scatter plot and then it turns out a correlation between schools 
#in each province and the number of patients exists.

ggplot(melt_province_patient, aes(school_count, patient_count, label = province))+
  geom_point()+
  facet_grid(~school, scales= c("free_x"))+
  geom_text_repel()


###### 4. Confirm the correlation by cor.test. The correlation between the number 
#of universites and the number of patients is approximately 0.83, which is stronger 
#than all the others.


cor.test(merge_province_patient$elementary_school_count, merge_province_patient$patient_count)
cor.test(merge_province_patient$kindergarten_count, merge_province_patient$patient_count)
cor.test(merge_province_patient$university_count, merge_province_patient$patient_count)

cor.test(merge_province_patient$elementary_school_count, merge_province_patient$patient_count, method = "spearman")
cor.test(merge_province_patient$kindergarten_count, merge_province_patient$patient_count, method = "spearman")
cor.test(merge_province_patient$university_count, merge_province_patient$patient_count, method = "spearman")

#Analysis of corrleation between the number of schools in a city and patients 

##### 1. Group by the column of city from patient dataset and count the number of patients in each city. Also, group by the column of city from region dataset and count the number of schools, namely kindergardens, elementary schools, and universities, in each city.


city_patient <- patient[, .(patient_count = .N), by = city]
city_region <- region[, .(sum_elementary_school = sum(elementary_school_count), sum_kindergarden = sum(kindergarten_count), sum_university = sum(university_count)), by = city]

##### 2. Merge the above transformed data tables into one table and melt it, creating a variable of school and a value of school count.


merge_city_patient <- merge(city_patient, city_region)
melt_city_patient <- melt(merge_city_patient, id.vars = c("city", "patient_count"), variable.name = "school", value.name = "school_count")


##### 3. Create a scatter plot and then it seems that a correlation between schools in each city and the number of patients does not exist.


ggplot(melt_city_patient, aes(school_count, patient_count))+
  geom_point()+
  facet_grid(~school, scales= c("free_x"))


##### 4. Show the correlation by cor.test. The correlations between the numbers of schools and the number of patients are all below 0.4.


cor.test(merge_city_patient$elementary_school_count, merge_city_patient$patient_count)
cor.test(merge_city_patient$kindergarten_count, merge_city_patient$patient_count)
cor.test(merge_city_patient$university_count, merge_city_patient$patient_count)


