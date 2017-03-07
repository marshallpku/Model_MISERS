
gc()

Tier_select <- "t1"








#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************

# Plan information
 #source("MISERS_Data_RP2000.R")
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


# Calibration ####
paramlist$bfactor <- 0.0165

# For model starting 2017
load("save2017_closed.RData")

if (paramlist$init.year.override == 2017){

init_actives_all     <- init_actives_all_2017  
init_retirees.la_all <- init_retirees.la_all_2017
init_disb.la_all     <- init_disb.la_all_2017
init_terms_all       <- init_terms_all_2017

}


# init_retirees.la_all
# init_disb.la_all 

#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("MISERS_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************

# initial value needed for sim starting in 2017

# init_actives_all
# init_retirees.la_all
# init_retirees.ca_all
# 
# init_disb.la_all
# init_disb.ca_all
# 
# init_terms_all


source("MISERS_Model_PrepData.R")

salary         <- get_salary_proc(Tier_select)
benefit        <- get_benefit_tier(Tier_select)
benefit.disb   <- get_benefit.disb_tier(Tier_select)
init_pop       <- get_initPop_tier(Tier_select)
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
# 4.1 Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("MISERS_Model_IndivLiab.R")
gc()

liab <- get_indivLab(Tier_select)

# liab$active %>% select(year, ea, age, COLA.scale) %>% arrange(ea, age) %>% head(100)


#*********************************************************************************************************
# 4.2 Estiamted AL and cash flow for DC disability benefits ####
#*********************************************************************************************************
source("MISERS_Model_DC.R")

liab.DC <- get_DCBen(NC.DC0, NC_B.1, NC_AL.1, NC.growth, B.growth)



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
penSim_results <- run_sim(Tier_select, AggLiab, liab.DC)

# paramlist$MA_0 <- 11397362049
# paramlist$AA_0 <- 11422640330



#*********************************************************************************************************
# 7  Showing results ####
#*********************************************************************************************************

var_display1 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", 
                  "AL.act", "AL.disb.la", "AL.act.disb", "AL.act.death", "AL.act.v", "AL.la", "AL.ca", "AL.term", "PVFB", "B",
                  # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                  # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca", 
                  "PR", "NC_PR", "NC","ERC")


var_display2 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", "EEC","ERC","ERC_PR","B", "B.v", "SC", "C", 
                  "nactives", "nretirees", "nla", "n.ca.R1", "n.ca.R0S1", "nterms", 
                  "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )


var_display.cali <- c("runname", "sim", "year", "FR","FR_MA", "MA", "AA", "AL", 
                      "AL.act", "AL.disb.la", "AL.term",
                      "PVFB", 
                      "B", # "B.la", "B.ca", "B.disb.la","B.disb.ca", 
                      # "C",   
                      "NC","SC", "ERC", "EEC",
                      "PR", "nactives", "nla",
                      "NC_PR", "ERC_PR",
                      "UAAL", "AL.DC", "NC.DC", "B.DC")


penSim_results %>% filter(sim == -1) %>% select(one_of(var_display1)) %>% print
penSim_results %>% filter(sim == 0) %>% select(one_of(var_display2))  %>% print

# Calibration
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display.cali)) %>% print
penSim_results %>% filter(sim == 0)  %>% select(one_of(var_display.cali)) %>% print



# penSim_results %>% filter(sim == 1)  %>% select(one_of(var_display.cali)) %>% print


df_all.stch <- penSim_results  %>% 
  filter(sim >= 0, year <= 2045)


df_all.stch %<>%   
  select(runname, sim, year, AL, MA, EEC, PR, ERC_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(EEC_PR = 100 * EEC/PR,
         FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 50), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10)),
         EEC_high  = cumany(ifelse(is.nan(EEC_PR), 0, EEC_PR) >= 25)) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            EEC_high = 100 * sum(EEC_high, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T),
            
            EEC_PR.q10 = quantile(EEC_PR, 0.1, na.rm = T),
            EEC_PR.q25 = quantile(EEC_PR, 0.25, na.rm = T),
            EEC_PR.q50 = quantile(EEC_PR, 0.5, na.rm = T),
            EEC_PR.q75 = quantile(EEC_PR, 0.75, na.rm = T),
            EEC_PR.q90 = quantile(EEC_PR, 0.9, na.rm = T)
  ) %>% 
  ungroup()


df_all.stch

#x <- pop$disb.la %>% filter(year == 2017)




# 
# PVPR <- penSim_results %>% filter(sim == -1) %>%
#   mutate(PV.PR = PR * (1/(1 + 0.075))^(year - 2015)) %>% 
#   summarise(PV.PR = sum(PV.PR))
# 
# (11123171373 - 1222881091 + 635685729) * 1.08
# 


