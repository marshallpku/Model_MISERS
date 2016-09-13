# Risk measures for LAFPP

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


path.resultFile <- "Results/"


#********************************************************
#   Functions ####  
#********************************************************  
  calc_FR.pctless  <- function(df_results, FR.pct = c(40, 60)){
    
    # df_results <- results.stch 
    # FR.pct = c(40, 60)  
    
    runname <- paste0(df_results$Tier[1],"_", df_results$runname[1])
    
    fn <- function(df_results, FR.pct.single){
      df_results %>% 
        group_by(sim) %>% 
        mutate(FR.pctless = cumany(FR_MA <= FR.pct.single)) %>% 
        select(sim, year, FR.pctless) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("FR.pctless" = paste0("FR", FR.pct.single,"less")))
    }
    
    sapply(FR.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_FR.pctmore  <- function(df_results, FR.pct = c(80, 95), rolling = FALSE){
    
    # df_results <- penSim_results.sumTiers 
    # FR.pct = c(40, 60)  
    
    runname <- paste0(df_results$Tier[1],"_", df_results$runname[1])
    
    
    fn <- function(df_results, FR.pct.single){
      df_results %>% 
        group_by(sim) %>% 
        mutate(rolling = rolling, 
               FR.pctmore = ifelse(rolling, cumany(FR_MA >= FR.pct.single),  (FR_MA >= FR.pct.single))) %>% 
        select(sim, year, FR.pctmore) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("FR.pctmore" = paste0("FR", FR.pct.single,"more")))
    }
    
    sapply(FR.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_ERCsharpRise <- function(df_results, ERC.pct = c(5, 10)){
    
    # df_results <- results.stch 
    # FR.pct = c(40, 60)  
    
    runname <- paste0(df_results$Tier[1],"_", df_results$runname[1])
    
    
    fn <- function(df_results, ERC.pct.single){
      
      df_results %>% 
        group_by(sim) %>% 
        mutate(ERC.ChgPts5y =  ERC_PR - lag(ERC_PR, 5),  # year1-5 change in pct points 
               ERC.ChgPts5y = na2zero(ERC.ChgPts5y),
               ERC.ChgPts5y.Xmore = cumany(ERC.ChgPts5y >= ERC.pct.single)) %>% 
        select(sim, year, ERC.ChgPts5y.Xmore) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("ERC.ChgPts5y.Xmore" = paste0("ERC.ChgPts5y.", ERC.pct.single,"more")))
    }
    
    sapply(ERC.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_highERC      <- function(df_results, ERC.pct = c(30, 50)){
    
    # df_results <- results.stch 
    # FR.pct = c(40, 60)  
    
    runname <- paste0(df_results$Tier[1],"_", df_results$runname[1])
    
    fn <- function(df_results, ERC.pct.single){
      
      df_results %>% 
        group_by(sim) %>% 
        mutate(ERC_PR.Xmore = cumany(ERC_PR >= ERC.pct.single)) %>% 
        select(sim, year, ERC_PR.Xmore) %>% 
        group_by(year) %>% 
        summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
        plyr::rename(c("ERC_PR.Xmore" = paste0("ERC_PR.", ERC.pct.single,"more")))
    }
    
    sapply(ERC.pct, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  
  calc_qts  <- function(df_results, varname, qts = c(0.1, 0.25, 0.5, 0.75, 0.9)){
    
     # df_results <- penSim_results %>% filter(sim >0, year >2015) #results.stch 
     # qts = c(0.1, 0.25, 0.5, 0.75, 0.9)
     # varname = "ERC_PR"
    # qts.single = 0.1
    
    
    runname <- paste0(df_results$Tier[1],"_", df_results$runname[1])
    
    
    df_results %<>% select(runname, sim, year, one_of(varname)) %>% 
      dplyr::rename_("var" = varname)
    
    fn <- function(df_results, qts.single){
      
      df_results %>% 
        group_by(year) %>% 
        summarize(var = quantile(var, qts.single, na.rm = T)) %>% 
        plyr::rename(c("var" = paste0(varname, ".q", qts.single)))
    }
    
    
    sapply(qts, fn, df_results = df_results, simplify = F) %>% join_all %>% 
      mutate(runname = runname) %>% 
      select(runname, everything())
    
  }
  # need to check issue with NA, NaN values
  
  
  # # load("Results/results_RS1.RData")
  # # 
  # # calc_qts(penSim_results, "ERC_PR")
  # 
  # 
  # x <- penSim_results %>% select(sim, year, ERC_PR, ERC, PR) %>% filter(sim >0)
  # x$ERC_PR
  # 
  
  # Function to read result data frame given the file name and path
  
  
  get_measureList <- function(runname, path){
    
      # runname <- "sumTiers_RS1"
      # path <- "Results/"
     # 
    fileName <- paste0(path,"results_", runname, ".RData") 
    load(fileName)
    
    results.stch <- penSim_results %>% filter(sim > 0, year > 2015)
    
    prob.FR.pctless <- results.stch %>%  calc_FR.pctless
    prob.FR.pctmore <- results.stch %>%  calc_FR.pctmore(rolling = T)
    prob.ERCsharpRise <- results.stch %>%  calc_ERCsharpRise
    prob.highERC <- results.stch %>%  calc_highERC
    FR.qts <- results.stch %>%  calc_qts("FR")
    ERC_PR.qts <- results.stch %>%  calc_qts("ERC_PR")
    
    
    assign(paste0("RiskMeasures_", runname), 
           list( 
             prob.FR.pctless = prob.FR.pctless,
             prob.FR.pctmore = prob.FR.pctmore,
             prob.ERCsharpRise = prob.ERCsharpRise,
             prob.highERC = prob.highERC,
             FR.qts  = FR.qts ,
             ERC_PR.qts = ERC_PR.qts))
    
    #  return(get(paste0("RiskMeasures_", runname)))
    
    do.call(save, list(paste0("RiskMeasures_", runname), file=paste0(path, "RiskMeasures_", runname, ".RData")))
    
    #do.call( return, list(get(paste0("RiskMeasures_", runname))))
    return(get(paste0("RiskMeasures_", runname)))
    
  }
  # Inputs should be a result data frame
  
  
  # Extract a risk measure from each model run
  get_measure <- function(measureName, measureList){
    
    # measureList <- RiskMeasures_list
    # 
    # measureName <- "prob.ERCsharpRise"
    
    n <- length(measureList)
    
    out.list <- vector("list", n)
    
    for(i in 1:n){out.list[[i]] <- measureList[[i]][[measureName]]}
    
    out.df <- rbind_all(out.list)
  }
  
  
  
  #********************************************************
  #   Calculating risk measures ####  
  #*******************************************************
  
  RiskMeasures_RS1 <- get_measureList("RS1", path.resultFile)
  RiskMeasures_RS2 <- get_measureList("RS2", path.resultFile)
  RiskMeasures_RS3 <- get_measureList("RS3", path.resultFile)
  RiskMeasures_RS4 <- get_measureList("RS4", path.resultFile)
  RiskMeasures_RS5 <- get_measureList("RS5", path.resultFile)
  
  
  
  RiskMeasures_RS1$prob.FR.pctless
  RiskMeasures_RS2$prob.FR.pctless
  RiskMeasures_RS3$prob.FR.pctless
  RiskMeasures_RS4$prob.FR.pctless
  RiskMeasures_RS5$prob.FR.pctless
  
  RiskMeasures_RS1$prob.ERCsharpRise
  RiskMeasures_RS2$prob.ERCsharpRise
  RiskMeasures_RS3$prob.ERCsharpRise
  RiskMeasures_RS4$prob.ERCsharpRise
  RiskMeasures_RS5$prob.ERCsharpRise
  
  # rm(RiskMeasures_sumTiers_RS1)
  # load("Results/RiskMeasures_sumTiers_RS1.RData", ex <- new.env())
  # ls.str(ex)
  # 
  

RiskMeasures_list <- list(
  RiskMeasures_RS1,
  RiskMeasures_RS2,
  RiskMeasures_RS3,
  RiskMeasures_RS4,
  RiskMeasures_RS5
)


df_FR.pctless <- get_measure("prob.FR.pctless", RiskMeasures_list)
df_ERCsharpRise <- get_measure("prob.ERCsharpRise", RiskMeasures_list)
df_highERC <- get_measure("prob.highERC", RiskMeasures_list)
df_ERC_PR.qts <- get_measure("ERC_PR.qts", RiskMeasures_list)
df_FR.qts     <- get_measure("FR.qts", RiskMeasures_list)
df_FR.pctmore <- get_measure("prob.FR.pctmore", RiskMeasures_list)


df_FR.pctless
df_FR.qts %>% data.frame 

#********************************************************
#   Making exploratory graphs ####  
#*******************************************************

year.display <- c(2015:2044)

g.FR.pctless <- 
df_FR.pctless %>% filter(year %in% year.display) %>% gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = runname)) + theme_bw() + facet_grid(.~var) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2010, 2100, 5))
g.FR.pctless

g.ERCsharpRise <- 
df_ERCsharpRise %>% filter(year %in% year.display)%>% gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = runname)) + theme_bw() + facet_grid(.~var) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.ERCsharpRise 

g.highERC <- 
df_highERC%>% filter(year %in% year.display) %>% gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = runname)) + theme_bw() + facet_grid(.~var) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.highERC

g.FR.pctmore <- 
df_FR.pctmore %>% filter(year %in% year.display)%>% gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = runname)) + theme_bw() + facet_grid(.~var) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.pctmore

g.FR.qts <-   
df_FR.qts %>% filter(year %in% year.display) %>% gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.qts
# Why FRs are different across scenarios?

g.ERC_PR.qts <- 
df_ERC_PR.qts %>% filter(year %in% year.display) %>% gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  coord_cartesian(ylim = c(0, 500))+
  scale_x_continuous(breaks = seq(2015, 2100, 5)) +
  scale_y_continuous(breaks = seq(0, 500, 20))
g.ERC_PR.qts


ggsave(g.FR.pctless,   file = paste0(path.resultFile, "g.FR.pctless.png"), width  = 10, height = 5)
ggsave(g.ERCsharpRise, file = paste0(path.resultFile, "g.ERCsharpRise.png"), width  = 10, height = 5)
ggsave(g.highERC,      file = paste0(path.resultFile, "g.highERC.png"), width  = 10, height = 5)
ggsave(g.FR.pctmore,   file = paste0(path.resultFile, "g.FR.pctmore.png"), width  = 10, height = 5)

ggsave(g.FR.qts,     file = paste0(path.resultFile, "g.FR.qts.png"), width  = 15, height = 5)
ggsave(g.ERC_PR.qts, file = paste0(path.resultFile, "g.ERC_PR.qts.png"), width  = 15, height = 5)







