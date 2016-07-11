# This master file is for simulating demographic distributions of non-actives in LAFPP.
# The simulated distributions will be used to infer the demographic distributions of initial non-actives

# Simulation period.
  # 80 years

# Distribution of new entrants:
  # Based on actives in Tier 6 with low yos

# All other plan provisions, assumptions are based on tier 2
  # Most current retirees should be from Tier 2(effective in 1967-1980)



gc()

Tier_select <- "t2"
Global_paramlist$nyear <- 60

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************

# Plan information
# source("LAFPP_Data_RP2000.R")
# source("LAFPP_Data_PlanInfo.R")
# source("LAFPP_Data_ImportMemberData.R")

load("Data_inputs/LAFPP_PlanInfo.RData")    # for all tiers
load("Data_inputs/LAFPP_MemberData.RData")  # for all tiers

pct.init.ret.la <-  1
pct.init.ret.ca  <- 1 - pct.init.ret.la

pct.init.disb.la <-  1
pct.init.disb.ca  <- 1 - pct.init.disb.la

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
source("LAFPP_Model_Decrements.R")

list.decrements <- get_decrements(Tier_select)
decrement.model      <- list.decrements$decrement.model
mortality.post.model <- list.decrements$mortality.post.model



#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("LAFPP_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************

source("LAFPP_Model_PrepData.R")

salary       <- get_salary_proc(Tier_select)
benefit      <- get_benefit_tier(Tier_select)
benefit.disb <- get_benefit.disb_tier(Tier_select)

entrants_dist  <- get_entrantsDist_tier("t6")

bfactor %<>% select(yos, matches(Tier_select)) %>% 
             rename_("bfactor" = paste0("bf.", Tier_select))



## Exclude selected type(s) of initial members
#init_actives_all %<>% mutate(nactives = 0) 
init_retirees_all %<>% mutate(nretirees = 0)
init_beneficiaries_all %<>% mutate(nbeneficiaries = 0)
init_terms_all %<>% mutate(nterm = 0)
init_disb_all %<>% mutate(ndisb = 0)

init_pop     <- get_initPop_tier(Tier_select)




#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("LAFPP_Model_Demographics.R")
gc()
pop <- get_Population()




#*********************************************************************************************************
# 3. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("LAFPP_Model_ContingentAnnuity.R")

# For service retirement
liab.ca <- get_contingentAnnuity(Tier_select, 
                                 tier.param[Tier_select, "factor.ca"],
                                 min(paramlist$range_age.r):100, 
                                 apply_reduction = FALSE)

# For disability benefit
range_age.disb <-  min(paramlist$range_age):100
liab.disb.ca <- get_contingentAnnuity(Tier_select, 
                                      tier.param[Tier_select, "factor.ca.disb"],
                                      range_age.disb, 
                                      apply_reduction = FALSE) %>% 
                rename(age.disb = age.r)


#*********************************************************************************************************
# 4. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("LAFPP_Model_IndivLiab.R")
gc()


liab <- get_indivLab(Tier_select)



#*********************************************************************************************************
# 5.   Examine demographic data ####
#*********************************************************************************************************

##  life annuitants
demo.la <- left_join(pop$la, liab$la) %>% filter(year %in% 2015:max(year))
demo.la %>% head


# Average age by year
demo.la %>% group_by(year) %>% 
  summarize(avg.age = sum(age  * number.la, na.rm = T)/sum(number.la, na.rm = T),
            nret    = 1000 * sum(number.la, na.rm = T))

# distribution of age and benefit by year
demo.la.dist <-  demo.la %>% group_by(year, age) %>% 
  summarize(num.la = sum(number.la, na.rm = T),
            avg.ben   = sum(B.la * number.la, na.rm = T)/sum(number.la, na.rm = T) )  %>% 
  mutate(dist.num.la  = 100 * num.la / sum(num.la),
         dist.ben.la =  100 * avg.ben / sum(avg.ben, na.rm = T)) 



# Plotting age and benefit distribution
demo.la.dist %>% filter(year == 2064) %>%  
                 qplot(x = age, y = dist.num.la, data =., geom = "line") +
                 scale_x_continuous(breaks = seq(0,120,5))

demo.la.dist %>% filter(year == 2064) %>%  
                 qplot(x = age, y = dist.ben.la, data =., geom = "line") +
                 scale_x_continuous(breaks = seq(0,120,5))






##  Disability life annuitants
demo.disb.la <- left_join(pop$disb.la, liab$disb.la) %>% filter(year %in% 2015:max(year))
demo.disb.la %>% head


# Average age by year
demo.disb.la %>% group_by(year) %>% 
  summarize(avg.age = sum(age  * number.disb.la, na.rm = T)/sum(number.disb.la, na.rm = T),
            n.disb.la    = 1000 * sum(number.disb.la, na.rm = T))

# distribution of age and benefit by year
demo.disb.la.dist <-  demo.disb.la %>% group_by(year, age) %>% 
  summarize(num.disb.la = sum(number.disb.la, na.rm = T),
            avg.ben     = sum(B.disb.la * number.disb.la, na.rm = T)/sum(number.disb.la, na.rm = T) )  %>% 
  mutate(dist.num.disb.la  = 100 * num.disb.la / sum(num.disb.la),
         dist.ben.disb.la =  100 * avg.ben / sum(avg.ben, na.rm = T)) 


# Plotting age and benefit distribution
demo.disb.la.dist %>% filter(year == 2064) %>%  
  qplot(x = age, y = dist.num.disb.la, data =., geom = "line") +
  scale_x_continuous(breaks = seq(0,120,5))

demo.disb.la.dist %>% filter(year == 2064) %>%  
  qplot(x = age, y = dist.ben.disb.la, data =., geom = "line") +
  scale_x_continuous(breaks = seq(0,120,5))



## Service retireeBeneficiaries





#*********************************************************************************************************
# 5.   Construct distribution for initial LAFPP non-actives ####
#*********************************************************************************************************


## Re-organize age and benefit distribution

dist_init.retirees <- 
demo.la.dist %>% ungroup %>% 
  filter(year == 2064, age %in% 41:100) %>% 
  select(age, dist.num.la, dist.ben.la) %>% 
  mutate(dist.num.la = dist.num.la / sum(dist.num.la),
         dist.ben.la = dist.ben.la / dist.ben.la[1])

dist_init.disb <- 
demo.disb.la.dist %>% ungroup %>% 
  filter(year == 2064, age %in% 21:100) %>% 
  select(age, dist.num.disb.la, dist.ben.disb.la) %>% 
  mutate(dist.num.disb.la = dist.num.disb.la / sum(dist.num.disb.la),
         dist.ben.disb.la = dist.ben.disb.la / dist.ben.disb.la[1])


## Missing steps:
 # Calibrate the age distribution to match the average ages in AV 2015
 # Age and benenfit distribution of benficiaries.


save(dist_init.retirees, dist_init.disb, file = "Data_inputs/dist_init.nonActives.RData")












# Target measures

# Retirees 
 # average age of retirees in all tiers 
 (4559*74.5 + 3046 * 63.7 + 227*60.3 + 202*54.3 + 77*86.1 + 11*59.6)/(4559 + 3046 + 227 + 202 + 77 + 11)
 # 69.64
 # average benefit 
 (77*2349 + 4559*5025 + 227*2888 + 202*4745 + 3046*7391 + 11*6397)/(77 + 4559 + 227 + 202 + 3046 + 11)
 # 5822.1

# Disabled
 # average age
(75*82   + 1540*73.2 + 249*56.1 + 45*53.4 + 120*50.9 + 2*50.1)/(75 + 1540 + 249 + 45 + 120 + 2)
 #69.65
 # average benefit
(75*3108 + 1540*4875 + 249*3522 + 45*4525 + 120*4745 + 2*4914)/(75 + 1540 + 249 + 45 + 120 + 2)
 #4628.5


# Beneficiaries
 # average age
(292*84   + 1876*78.6 + 83*53   + 202*35.2 + 185*54.5 )/(292 + 1876 + 83 + 202 + 185)
 #73.37
 # average benefit
(292*2584 + 1876*4288 + 83*3880 + 202*6803 + 185*5504 )/(292 + 1876 + 83 + 202 + 185)
 #4364.4





demo.active <- pop$active %>% filter(year == max(year))

demo.active %>% spread(age, number.a)



demo.active %>% group_by(year, age) %>% 
  summarize(nret = sum(number.a, na.rm = T))

pop$active %>% filter(year == 2085) %>% summarize(n = sum(number.a))




# #*********************************************************************************************************
# # 5. Aggregate actuarial liabilities, normal costs and benenfits ####
# #*********************************************************************************************************
# source("LAFPP_Model_AggLiab.R")
# gc()
# 
# AggLiab <- get_AggLiab(Tier_select,
#                        liab,
#                        liab.ca,
#                        pop) 
# 
# 























# #*********************************************************************************************************
# # 6.  Simulation ####
# #*********************************************************************************************************
# source("LAFPP_Model_Sim.R")
# penSim_results <- run_sim(Tier_select, AggLiab)
# 
# 
# 
# 
# #*********************************************************************************************************
# # 7  Showing results ####
# #*********************************************************************************************************
# 
# 
# var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", "AL.la", "AL.ca", 
#                  #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
#                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
#                  "B", "B.la", "B.ca", "B.death", 
#                  "nactives", "nterms", "PR", "NC_PR", "NC")
# 
# 
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display)) %>% print
# #penSim_results %>% filter(sim == -1) %>% data.frame
# 












