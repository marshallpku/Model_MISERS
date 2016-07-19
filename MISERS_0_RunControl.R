# Run control file of the LAFPP model

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
# library(xlsx)
library("btools")

source("Functions.R")


# Road map for model files (all with suffix "LAFPP_" and extension ".R")
 
 # Master files:
   # Master_singleTier
   # Master_allTiers

 # Data_RP2000
 # Data_PlanInfo 
 # Data_ImportMemberData

 # Model_decrements
 # Model_InvReturns
 # Model_PrepData
 # Model_Demographics
 # Model_ContingentAnnuity
 # Model_IndivLiab
 # Model_AggLiab
 # Model_Sim


# Notes on disability benefits
  # qxd is the sum of qxd.nonduty and qxd.duty
  # assume all disabilities are non-duty in the model 
  # AS of July19 2016
    # Model disability benefit as life annuity. 
    


# Notes on retirement benefit
  # retirement rates are weighted averages of conservation, correction and others. 
  # For now(July 19, 2016), use retiremetn rule for "others" for all members. 

# Notes on pre-retirement death benefits
  # Currently, the benefit is 100% of retirement benefit. In later versions, the benefit should be 
  # reduced in accordance with a 100% Joint and Survivor form of payment.  


# Notes on initial service retirees and disability retirees
  # A proportion of them are assumed to be life annuitants while the rest contingent annuitants.
  # As of 7/9/2016, the proportion of life annuitants is 20%. 






#### Model Parameters ####
#********************************************************************************
folder_run <- "."
filename_RunControl <- dir(folder_run, pattern = "^RunControl")
path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)

# Import global parameters
runList <- read_excel(path_RunControl, sheet="params", skip = 0) %>% filter(!is.na(runname), include == 1)
runList

# Import return scenarios
returnScenarios <- read_excel(path_RunControl, sheet="returns", skip = 0) %>% filter(!is.na(scenario))

# Import global parameters
Global_paramlist <- read_excel(path_RunControl, sheet="GlobalParams") %>% filter(!is.na(init.year)) %>% 
                 as.list


#### Run Models and Save  ####
#********************************************************************************

folder_save <- "Results/"



#####  Run Model ####
#*********************************************************************************************************


for(runName in runList$runname ){
  
  #runName <- "RS1.closed"
  
  paramlist <- get_parmsList(runList, runName)
  
  paramlist$simTiers <- "separate"  # "joint"(defult) or "separate"

  if(paramlist$nyear.override != 0) Global_paramlist$nyear <- paramlist$nyear.override
  
  
  paramlist$Grouping    <- "fillin"
    
  paramlist$r.min  <- 45 # this is not required age of retirement benefit. 
  paramlist$r.max  <- 75 
    
    #fasyears <- 3,
    #cola     <- 0.03,
    #i <- 0.075,
    
  paramlist$infl <- 0.035
  paramlist$prod <- 0.01
    #s.year <- 7,
  paramlist$s.lower  <- 0.7   # AVA is adjusted to be within 40% of MVA:
  paramlist$s.upper  <- 1.3
    
    #m <- 20,
    
  paramlist$r.full <- 60 # age at which vested terms are assumed to retire(Temp, should use r.vben)
  paramlist$r.vben <- 60 # age at which vested terms are assumed to retire.
    
    #r.yos  <- 5,
    #v.yos  <- 5, 
    #r.age
  
  paramlist$r.yos1 <- 30
  paramlist$r.age1 <- 55
  
  paramlist$r.yos2 <- 10
  paramlist$r.age2 <- 60
  
  paramlist$v.yos <- 10
  
  paramlist$fasyears <- 3
  
  paramlist$bfactor <- 0.015
  
  paramlist$cola <- 0.03
    
  paramlist$startingSal_growth <- 0.04
    
  paramlist$actuarial_method <- "EAN.CP"
    
  paramlist$newEnt_byTier <- c(t1 = 0, t2 = 1)

  paramlist$pct.ca.M <-  0.75 # proportion of males who opt for ca upon retirement
  paramlist$pct.ca.F <-  0.6
    
  paramlist$factor.ca <- 1

  paramlist$factor.ca.disb <- 1		
  
  
      
    # Investment returns
  paramlist$seed <- 1234
    #ir.mean <- 0.075,
    #ir.sd   <- 0,  # 0.12,
    
    
  # paramlist$init_MA <- "AL_pct"
  # paramlist$init_AA <- "AL_pct"  # Later we may want to allow it to be "MA" (equal to MA), "AL" (equal to AL), "AA0"(preset AA value)
  # 
  # paramlist$MA_0_pct <- 0.946  # AV2015 pdf page 12
  # paramlist$AA_0_pct <- 0.915  # AV2015 pdf page 12
  # 
  paramlist$init_EAA <- "MA"
    
    
  paramlist$smooth_method <- "method1"
  # paramlist$salgrowth_amort <- 0.04
  #amort_method <- "cp",
  paramlist$amort_type <- "closed"
    #nonNegC <- "FALSE",
    #EEC_fixed <- "TRUE",
    #ConPolicy <- "ADC",
    #EEC_rate <- 0.05
  
  paramlist$EEC_rate <- 0.04

  
  # Parameters derived from the parameter list above. 
  paramlist$range_ea = with(Global_paramlist, min.ea:max.ea)
  paramlist$range_age = with(Global_paramlist, min.age:max.age)
  paramlist$range_age.r = with(paramlist, r.min:r.max)
  paramlist$v     = with(paramlist, 1/(1 + i))
  

  
  # if(paramlist$tier == "sumTiers"){
  #   source("LAFPP_0_Master_allTiers.R")
  #   save(penSim_results.sumTiers, file = paste0(folder_save, "results_",  paramlist$Tier, runName, ".RData"))
  #   
  # } else {
  #   Tier_select <- paramlist$tier
  #   source("LAFPP_0_Master_singleTier.R")
  #   save(penSim_results, file = paste0(folder_save, "results_",  paramlist$Tier, runName, ".RData"))
  # }
}











   
   
# Checking the importance of death benefit

# Tier_select <- "t5"
# source("Test_0_Master_singleTier.R")


# 
# 
# 
# 166987857/16955579066
# 
# (16955579066 + 541456502 - 798249899)*0.04




