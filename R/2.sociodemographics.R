#------------------------------------------------------------------------------#
# 1.1 Demographics
# This script extracts the demographics from the data
#------------------------------------------------------------------------------#


gender_part<-single_part %>%
  group_by(  agegroup, gender ) %>%
  tally()

# gender fot the participants
kable(gender_part, caption = "Gender by Agegroup")

part_demo<-df_wide %>%
  group_by(participant) %>%
  slice(1)%>%
  group_by( agegroup ) %>%
  dplyr::summarise(Age = mean(age, na.rm=T),
                   Age_sd = sd(age, na.rm=T),
                   max_Age=max(age, na.rm=T),
                   min_age = min(age, na.rm=T),
                   Crystallized_intelligence_mean = mean(Mill_Hill, na.rm = T) , 
                   Crystallized_intelligence_sd = sd(Mill_Hill, na.rm = T),
                   Processing_speed_mean = mean(DS, na.rm = T), 
                   Processing_speed_sd = sd(DS, na.rm = T), 
                   Health = mean(health, na.rm=T), 
                   Health_df = sd(health, na.rm=T),
                   Years_education = mean(education_y, na.rm=T), 
                   Years_education_sd = sd(education_y, na.rm=T), 
                   General_intrusive_thoughts = mean(tcaq_g, na.rm=T),
                   General_intrusive_thoughts_sd = sd(tcaq_g, na.rm=T), 
                   Task_related_intrusive_thoughts = mean(c_tcaq, na.rm=T),
                   Task_related_intrusive_thoughts_sd = sd(c_tcaq, na.rm=T))



# View the summary statistics
print(kable(part_demo, caption = "Sociodemographics"))




