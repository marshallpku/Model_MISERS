# This script imports plan information from PlanInfo-MISERS.xlsx

# This script performs necessary transformations (eg. truncation, expansion, imputation) to the original data, 
# but do not create new variables (eg. computing weighted average) All new variables, which might be run-specific are 
# created in Model_PrepData.R.

# Data sources:
  # Data_inputs/MISERS_mortality.RData
  # Data_inputs/MISERS_PlanInfo.xlsx

# List of outputs
  # mortality_MISERS, 
  # retRates, 
  # termRates, 
  # disbRates, 
  # salgrowth
  # tier.param
  # init_amort_raw

# Output file:
  # Data_inputs/MISERS_PlanInfo.RData


#### To do list
# 1. More smoothed imputation of decrements



# plan information file:
file_planInfo <- "Data_inputs/MISERS_PlanInfo_2015.xlsx"


#*********************************************************************************************************
#                      ## Tools ####
#*********************************************************************************************************

# read_planInfoTable <- function(file, sheet, cellStart, cellEnd, ...){
#   require(XLConnect)
#   range <- xlrange(file, sheet, cellStart, cellEnd)
#   readWorksheetFromFile(file, sheet = sheet, header=TRUE, region=range, ...)
# }




#*********************************************************************************************************
#                      ## Mortality tables ####
#*********************************************************************************************************
# Read mortality table created by MISERS_Data_RP2000.R.
load("Data_inputs/MISERS_mortality.RData") # data frame "mortality_MISERS" loaded
mortality_MISERS %<>%  select(age,
                            qxm.pre.male, qxm.pre.female,   # mortality for active members
                            qxm.post.male, qxm.post.female, # mortality for retirees and beneficiaries 
                            qxm.d.male, qxm.d.female)       # mortality for disabled

#*********************************************************************************************************
#                      ## Retirement rates  ####
#*********************************************************************************************************
retRates <- read_ExcelRange(file_planInfo, sheet="Ret_dec", "B2", "B3", colTypes="numeric")


#*********************************************************************************************************
#                      ## benefit factors  ####
#*********************************************************************************************************
# bfactor <- read_ExcelRange(file_planInfo, sheet="Ret_bfactor", "B2", "B3", colTypes="numeric")




#*********************************************************************************************************
#                      ## Termination rates  ####
#*********************************************************************************************************
# Term rates for yos < 5
termRates1 <- read_ExcelRange(file_planInfo, sheet="Term_dec1", "B2", "B3", colTypes="numeric")
# Term rates for yos >=5 (given every 5 years, need to expand to all ages)
termRates2 <- read_ExcelRange(file_planInfo, sheet="Term_dec2", "B2", "B3", colTypes="numeric") %>% 
              rename(age.match = age)

termRates2 <- data.frame(age = 20:74) %>% 
              mutate(age.match = floor(2*age/10)*10/2) %>% 
              left_join(termRates2) %>% 
              select(-age.match) %>% 
              mutate(qxt.age = ifelse(!is.na(qxt.age), qxt.age,
                                      qxt.age[age == max(age[!is.na(qxt.age)])]
                                      )
              )


termRates <- expand.grid(ea = 20:74, age = 20:74) %>% 
  mutate(yos = age - ea) %>% 
  filter(age >= ea) %>% 
  arrange(ea, age) %>% 
  left_join(termRates1) %>% 
  left_join(termRates2) %>% 
  mutate(qxt = ifelse(yos < 5, qxt.yos, qxt.age)) %>% 
  select(ea, age, yos, qxt)

#termRates %>% arrange(ea, age)


#*********************************************************************************************************
#                      ## disability rates  ####
#*********************************************************************************************************
# Assume disability rates are 0 after age 74. (when all actives are assumed retired)
disbRates <- read_ExcelRange(file_planInfo, sheet="Disb_dec", "B2", "B3", colTypes="numeric") %>% 
             rename(age.match = age)

disbRates <- data.frame(age = 20:74) %>% 
  mutate(age.match = floor(2*age/10)*10/2) %>% 
  left_join(disbRates) %>% 
  select(-age.match) %>% 
  mutate(qxd.nonduty = ifelse(age >  max(age[!is.na(qxd.nonduty)]), 
                                     qxd.nonduty[age == max(age[!is.na(qxd.nonduty)])],
                                     qxd.nonduty),
         qxd.nonduty = ifelse(age <  min(age[!is.na(qxd.nonduty)]), 
                                     qxd.nonduty[age == min(age[!is.na(qxd.nonduty)])],
                                     qxd.nonduty),
 
         qxd.duty    = ifelse(age > max(age[!is.na(qxd.duty)]), 
                                    qxd.duty[age == max(age[!is.na(qxd.duty)])],
                                    qxd.duty),
         qxd.duty    = ifelse(age < min(age[!is.na(qxd.duty)]), 
                                    qxd.duty[age == min(age[!is.na(qxd.duty)])],
                                    qxd.duty)
         
         )


#*********************************************************************************************************
#                      ## Salary growth rates  ####
#*********************************************************************************************************

salgrowth <- read_ExcelRange(file_planInfo, sheet="SalaryGrowth", "B2", "B3", colTypes="numeric") %>% 
             rename(age.match = age)

salgrowth <- data.frame(age = 20:74) %>% 
  mutate(age.match = ifelse(age > 65, 65, floor(2*age/10)*10/2)) %>% 
  left_join(salgrowth) %>% 
  select(-age.match)



#*********************************************************************************************************
#                      ## Tier specific parameters ####
#*********************************************************************************************************

tier.param <- read_ExcelRange(file_planInfo, sheet="Tier.param", colTypes="character") %>% 
  mutate_each(funs(as.numeric), -tier)

row.names(tier.param) <- tier.param$tier



#*********************************************************************************************************
#                      ## Initial Amortization Basis  ####
#*********************************************************************************************************

init_amort_raw <-  read_ExcelRange(file_planInfo, sheet = "Init_amort", colTypes="character")
  
init_amort_raw %<>%
  mutate(year.est = year(year.est)) %>%
  mutate_each(funs(as.numeric), -tier,  -type, -amort.method)

# init_amort_raw #%>% str


#*********************************************************************************************************
#                      ## Initial unrecognized return  ####
#*********************************************************************************************************

init_unrecReturns.unadj <- read_ExcelRange(file_planInfo, sheet = "Init_unrecReturn", colTypes="numeric") 



#*********************************************************************************************************
#                      ## External fund   ####
#*********************************************************************************************************
extFund.unadj <- read_ExcelRange(file_planInfo, sheet = "External_Fund", colTypes="numeric") 


save(mortality_MISERS, retRates, termRates, disbRates, 
     salgrowth, 
     tier.param, 
     init_amort_raw, 
     init_unrecReturns.unadj, 
     extFund.unadj,
     file  = "Data_inputs/MISERS_PlanInfo.RData")







