# This script constructs mortality tables for the LAFPP model based on RP2000 tables and Scale BB from SOA 

# Mortality tables:

# Healthy, members:       RP2000 Combined Healthy Mortality Table(separate for males and females), projected to 2022 with scale BB set BACK one year.
# Healthy, beneficiaries: RP2000 Combined Healthy Mortality Table(separate for males and females), projected to 2022 with scale BB set FORWARD one year.

# DisabledHealthy, members: RP2000 Combined Healthy Mortality Table(separate for males and females), projected to 2022 with scale BB set FORWARD one year.



#*********************************************************************************************************
#                      ## Import Data  ####
#*********************************************************************************************************

# Import mortality data

data_raw_healthyMale   <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/RP2000-male-CombinedHealthy.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b144", colTypes="character") %>% 
                          mutate_each(funs(as.numeric))
data_raw_healthyFemale <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/RP2000-female-CombinedHealthy.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b144", colTypes="character") %>% 
                          mutate_each(funs(as.numeric))

names(data_raw_healthyMale)   <- c("age", "qxm.male")
names(data_raw_healthyFemale) <- c("age", "qxm.female")



# Import Scale BB
scaleBB_male   <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/ScaleBB-male.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b125", colTypes="character") %>% 
                  mutate_each(funs(as.numeric))
names(scaleBB_male)   <- c("age", "scale.male")


scaleBB_female <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/ScaleBB-female.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b125", colTypes="character") %>% 
                  mutate_each(funs(as.numeric))
names(scaleBB_female)   <- c("age", "scale.female")


#*********************************************************************************************************
#                      ## Compute RP2000 mortality projected to 2022  ####
#*********************************************************************************************************

mortality_LAFPP <- data.frame(age = 20:120) %>% 
  left_join(data_raw_healthyMale) %>%
  left_join(data_raw_healthyFemale) %>% 
  left_join(scaleBB_male) %>% 
  left_join(scaleBB_female) %>% 
  
  mutate(# project mortality into 2022 using scale BB
         qxm.male.proj2022   = qxm.male * (1 - scale.male)^22,
         qxm.female.proj2022 = qxm.female * (1 - scale.female)^22,

         # Move back one year to get mortality for active members (eg. age 60 uses mortality rate for 59)
         qxm.pre.male     = ifelse(age == min(age), qxm.male.proj2022,
                                   ifelse(age == max(age), 1, lag(qxm.male.proj2022))),
         qxm.pre.female   = ifelse(age == min(age), qxm.female.proj2022,
                                 ifelse(age == max(age), 1, lag(qxm.female.proj2022))),
         
         # Move forward one year to get mortality for beneficiaries (eg. age 60 uses mortality rate for 61)
         qxm.post.male       =  ifelse(age == max(age), 1, lead(qxm.male.proj2022)),
         qxm.post.female     =  ifelse(age == max(age), 1, lead(qxm.female.proj2022)),
         
         # Mortality for disabled is the same as for beneficiaries, for now. 
         qxm.d.male   =  qxm.post.male,
         qxm.d.female =  qxm.post.female
         )
  
  
# check results against sample mortality provided in AV2015 and experience study 2013
mortality_LAFPP %>% filter(age %in% seq(20, 60, 5)) %>% select(age, qxm.pre.male, qxm.pre.female) %>% kable(digits = 4)
# All match

save(mortality_LAFPP, file = "Data_inputs/LAFPP_mortality.RData")










