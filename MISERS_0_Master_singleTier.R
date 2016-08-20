
gc()

 Tier_select <- "t1"

 
#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************

# Plan information
# source("MISERS_Data_RP2000.R")
 source("MISERS_Data_PlanInfo.R")
 source("MISERS_Data_ImportMemberData.R")

load("Data_inputs/MISERS_PlanInfo.RData")    # for all tiers
load("Data_inputs/MISERS_MemberData.RData")  # for all tiers

# salgrowth %<>% mutate(salgrowth = salgrowth * 1.1)

pct.init.ret.la <-  1
pct.init.disb.la <-  1

pct.init.disb.ca  <- 1 - pct.init.disb.la
pct.init.ret.ca  <- 1 - pct.init.ret.la

init_retirees.la_all <- init_retirees_all %>%
  mutate(nretirees.la = nretirees * pct.init.ret.la) %>%
  select(-nretirees)

init_retirees.ca_all <- init_retirees_all %>%
  mutate(nretirees.ca = nretirees * pct.init.ret.ca) %>%
  select(-nretirees)

init_disb.la_all <- init_disb_all %>%
  mutate(ndisb.la = ndisb * pct.init.disb.la) %>%
  select(-ndisb)

init_disb.ca_all <- init_disb_all %>%
  mutate(ndisb.ca = ndisb * pct.init.disb.ca) %>%
  select(-ndisb)




#*********************************************************************************************************
# 1.2 Create decrement tables ####
#*********************************************************************************************************

# Decrement tables
source("MISERS_Model_Decrements.R")

list.decrements <- get_decrements(Tier_select)
decrement.model      <- list.decrements$decrement.model
mortality.post.model <- list.decrements$mortality.post.model


#**********************************************
##   Modify initial data ####
#**********************************************

## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees.la_all %<>% mutate(nretirees.la = 0)
 # init_retirees.ca_all %<>% mutate(nretirees.ca = 0)
 # init_beneficiaries_all %<>% mutate(nbeneficiaries = 0)
 # init_terminated_all %<>% mutate(nterm = 0)


## Exclude initial terms with ea < 20: Data_population, line 504 
 # init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
 #                                 ea >= Global_paramlist$min.ea)


# ## Exclude the initial amortization basis when testing the program.
# if(!paramlist$useAVamort) init_amort_raw %<>% mutate(amount.annual = 0) 
# 
# ## Exclude the external fund. (currently only STIP borrowing)
# if(!paramlist$useExtFund) extFund %<>% mutate_each(funs(. * 0), -year)


#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("MISERS_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************

source("MISERS_Model_PrepData.R")

salary       <- get_salary_proc(Tier_select)
benefit      <- get_benefit_tier(Tier_select)
benefit.disb <- get_benefit.disb_tier(Tier_select)
init_pop     <- get_initPop_tier(Tier_select)
entrants_dist  <- get_entrantsDist_tier("t1")

 



#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("MISERS_Model_Demographics.R")
gc()
pop <- get_Population()


#*********************************************************************************************************
# 3. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("MISERS_Model_ContingentAnnuity.R")

# For service retirement
liab.ca <- get_contingentAnnuity(Tier_select, 
                                 paramlist$factor.ca,
                                 min(paramlist$range_age.r):100, 
                                 apply_reduction = TRUE)

# For disability benefit
range_age.disb <-  min(paramlist$range_age):100   # max(paramlist$range_age.r)
liab.disb.ca <- get_contingentAnnuity(Tier_select, 
                                      paramlist$factor.ca.disb,
                                      range_age.disb, 
                                      apply_reduction = TRUE) %>% 
                rename(age.disb = age.r)




#*********************************************************************************************************
# 4. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("MISERS_Model_IndivLiab.R")
gc()


liab <- get_indivLab(Tier_select)


liab$active %>% select(year, ea, age, COLA.scale) %>% arrange(ea, age) %>% head(100)


#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("MISERS_Model_AggLiab.R")
gc()

AggLiab <- get_AggLiab(Tier_select,
                       liab,
                       liab.ca,
                       liab.disb.ca,
                       pop) 


#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("MISERS_Model_Sim.R")
penSim_results <- run_sim(Tier_select, AggLiab)




#*********************************************************************************************************
# 7  Showing results ####
#*********************************************************************************************************


var_display1 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", 
                  "AL.act", "AL.act.laca", "AL.act.disb", "AL.act.death", "AL.act.v", "AL.la", "AL.ca", "AL.term", "PVFB", "B",
                  # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                  # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca", 
                  "PR", "NC_PR", "NC","ERC")


var_display2 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", "EEC","ERC","ERC_PR","B", "B.v", "SC", "C", 
                  "nactives", "nretirees", "nla", "n.ca.R1", "n.ca.R0S1", "nterms", 
                  "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )



penSim_results %>% filter(sim == -1) %>% select(one_of(var_display1)) %>% print
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display2)) %>% print
#penSim_results %>% filter(sim == -1) %>% data.frame



penSim_results %>% names

# load("Check_allTiers.RData")
# 
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display1)) %>% print
# penSim_results.t6 %>% filter(sim == -1) %>% select(one_of(var_display1)) %>% print
# 
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display2)) %>% print
# penSim_results.t6 %>% filter(sim == -1) %>% select(one_of(var_display2)) %>% print



#*********************************************************************************************************
# Detecitve work: term rates ####
#*********************************************************************************************************
# The AL of actives becomes even higher when higher term rates are used. 

# detective.t13 <- penSim_results
# save(detective.t13, file= "detective.t13.RData")
# 
# load("detective.t13.RData")
# detective.t13 %>% filter(sim == -1) %>% select(Tier,year, FR, MA, AL, AL.act,AL.act.laca, AL.act.v,AL.act.LSC, AL.la, AL.ca, AL.term, AL, PVFB.laca, PVFB.LSC, PVFB.v, PVFB, 
#                         B, B.la, B.ca, B.LSC,B.v, nactives, nterms, PR, NC_PR) %>% data.frame

# AL.act: 3.12b
# AL.nonact: 15.64 - 3.12=12.52  
# NC rate 5.88%




PVPR <- penSim_results %>% filter(sim == -1) %>%
  mutate(PV.PR = PR * (1/(1 + 0.075))^(year - 2015)) %>% 
  summarise(PV.PR = sum(PV.PR))

(11123171373 - 1222881091 + 635685729) * 1.08



