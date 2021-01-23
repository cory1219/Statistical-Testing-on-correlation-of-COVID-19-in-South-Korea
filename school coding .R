library(data.table)
install.packages('bit64')
library(bit64)
install.packages("ggplot2")
library(ggplot2)
library(ggrepel)
library(magrittr)

patient <- fread("PatientInfo.csv", na.strings = "")
region <- fread("Region.csv")

province_patient <- patient[, .(patient_count = .N), by = province]
province_region <- region[province == city, .(province, elementary_school_count, kindergarten_count, university_count)]
city_patient <- patient[, .(patient_count = .N), by = city]
city_region <- region[province != city, .(city, elementary_school_count, kindergarten_count, university_count)]

merge_province_patient <- merge(province_patient, province_region)
melt_province_patient <- melt(merge_province_patient, id.vars = c("province", "patient_count"), variable.name = "school", value.name = "school_count")
ggplot(melt_province_patient, aes(school_count, patient_count, label = province))+
  geom_point()+
  facet_grid(~school, scales= c("free_x"))+
  geom_text_repel()

merge_city_patient <- merge(city_patient, city_region)
melt_city_patient <- melt(merge_city_patient, id.vars = c("city", "patient_count"), variable.name = "school", value.name = "school_count")
ggplot(melt_city_patient, aes(school_count, patient_count))+
  geom_point()+
  facet_grid(~school, scales= c("free_x"))

cor.test(merge_province_patient$elementary_school_count, merge_province_patient$patient_count)
cor.test(merge_province_patient$kindergarten_count, merge_province_patient$patient_count)
cor.test(merge_province_patient$university_count, merge_province_patient$patient_count)

cor.test(merge_province_patient$elementary_school_count, merge_province_patient$patient_count, method = "spearman")
cor.test(merge_province_patient$kindergarten_count, merge_province_patient$patient_count, method = "spearman")
cor.test(merge_province_patient$university_count, merge_province_patient$patient_count, method = "spearman")


cor.test(merge_city_patient$elementary_school_count, merge_city_patient$patient_count)
cor.test(merge_city_patient$kindergarten_count, merge_city_patient$patient_count)
cor.test(merge_city_patient$university_count, merge_city_patient$patient_count)

##################


pop <- fread("SeoulFloating.csv")


city_region <- region[province == "Seoul", .(city, sum_school = elementary_school_count+kindergarten_count+university_count)]
# total schools in seoul

pop_last <- pop[date == "2020-05-31" & hour == 23, .(pop = sum(fp_num)), by=city][city == "Dongjag-gu", city := "Dongjak-gu"]
# 05/31 23 -> fixed

city_patient <- patient[confirmed_date < "2020-06-01" & province == "Seoul", .(patient_count = .N), by = city]

merge_city_patient <- merge(pop_last, city_region, all.x = T) %>% merge(city_patient, all.x = T) 

ggplot(merge_city_patient, aes(sum_school, patient_count))+
  geom_point()

ggplot(merge_city_patient, aes(pop, patient_count))+
  geom_point()

ggplot(merge_city_patient, aes(pop, sum_school))+
  geom_point()

ggplot(merge_city_patient, aes(sum_school, patient_count, fill=pop))+
  geom_point()


cor.test(merge_city_patient$sum_school, merge_city_patient$patient_count)
cor.test(merge_city_patient$pop, merge_city_patient$patient_count)
cor.test(merge_city_patient$pop, merge_city_patient$sum_school)




