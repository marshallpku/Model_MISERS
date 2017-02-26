# This script produces estimated liability and cash flow for disability benefits of DC plan members 
# Yimeng Yin 



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





#*********************************************************************************************************
# Inputs and assumptions ####
#*********************************************************************************************************

i <- paramlist$i


# Normal cost for model year 2015 (FY2015-2016) AV2015 page A1
NC.DC0 <- 6896913

# Assumption 1: initial normal cost as % of AL
NC_AL.1 <- 0.02

# Assumtpion 2: initial normal cost as % of B
NC_B.1 <-  0.3


# Assumption 3: growth rates for NC and B
NC.growth <- 0.035
B.growth  <- 0.035


# Formula for AL

# AL_t1 <- (AL_t0 + NC_t0 - B_t0) * (1 + DC)



#*********************************************************************************************************
# AL, NC and B ####
#*********************************************************************************************************

get_DCBen <- function(NC.DC0,
                      NC_B,
                      NC_AL,
                      NC.growth,
                      B.growth,
                      .Global_paramlist = Global_paramlist,
                      .paramlist = paramlist){

# 
#   NC.DC0 <- 6896913
#   NC_B   <- 0.3
#   NC_AL  <- 0.015
#   NC.growth <- 0.35
#   B.growth  <- 0.35 
#   .Global_paramlist = Global_paramlist
#   .paramlist = paramlist

assign_parmsList(.Global_paramlist, envir = environment())
assign_parmsList(.paramlist,        envir = environment()) 

df_DCBen <- data.frame(year = init.year:(init.year + nyear - 1), NC.DC = 0, B.DC = 0, AL.DC = 0) %>% 
  mutate(NC.DC = ifelse(year == init.year, NC.DC0, NC.DC),
         B.DC  = ifelse(year == init.year, NC.DC/NC_B, B.DC),
         AL.DC = ifelse(year == init.year, NC.DC/NC_AL, AL.DC),
         
         NC.DC = NC.DC[year == init.year] * (1 + NC.growth)^(row_number() - 1),
         B.DC  = B.DC[year  == init.year] *  (1 + B.growth)^(row_number() - 1)
         )

for (j in 2:nrow(df_DCBen)){
  df_DCBen$AL.DC[j] <- with(df_DCBen, (AL.DC[j - 1] + NC.DC[j - 1] - B.DC[j - 1]) * (1 + i))
}

return(df_DCBen)
}

#get_DCBen(NC.DC0, NC_B.1, 0.015, NC.growth, 0.035)




















