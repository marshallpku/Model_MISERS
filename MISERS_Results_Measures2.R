# Risk measures for MISERS

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
library(xlsx)
library("btools")
library("scales")

source("Functions.R")



#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "Results/"
Outputs_folder  <- "Results/Graphs_report/"


#*****************************************************
##  Loading data  ####
#*****************************************************

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    
    # if("results.t7" %in% names(outputs_list)){
    #   df_out <- bind_rows(outputs_list$results,
    #                       outputs_list$results.t7,
    #                       outputs_list$results.xt7)
    #   return(df_out)
    # } else {
    #   return(outputs_list$results)
    # }
    
    return(penSim_results)
    
  }
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}



results_all <- get_results(IO_folder, "results_") %>% select(runname, sim, year, everything())


## Complement RS5 with data of 2015 and 2016
df_temp <- results_all %>% 
  filter(runname == "RS4.closed", year %in% 2015:2016) %>% 
  mutate(runname = "RS5.closed")

df_temp %>% filter(sim == 0)


results_all %<>% bind_rows(df_temp) %>% arrange(runname, sim, year) 

## AL calculated with DC = 8% for closed plan
df_AL.closed.8pct <- results_all %>% filter(runname == "RS1.closed", sim == 1) %>% 
  select(year, AL.closed.8pct = AL)

results_all %<>% left_join(df_AL.closed.8pct) 




## Loading revenue projection data
load("GenFund_proj.RData")

results_all %<>% left_join(df_revenue %>% select(year, GenFund = GenFund.proj)) %>% 
  mutate(ERC_GF = 100 * ERC/GenFund, 
         AL_GF  = 100 * AL/GenFund)





#**********************************************************************************************
##  Defining color and theme for publication format of Rockefeller Institute of Government ####
#**********************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


# demo.shape5 <- c(16, 16, 16, 15, 17) # 16-average, 15-mature, 17-immature 


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}






#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

# Runs used in the report 
runs_RS.closed <- paste0("RS", 1:5, ".closed")
runs_RS.open   <- paste0("RS", 1:4, ".open")


runs_RS.closed_labels <- c("Assumption Achieved; closed plan",
                           "15 years of low returns; closed plan",
                           "High volatility; closed plan",
                           "7.5% returns; closed plan",
                           "Lower assumed return; closed plan")

runs_RS.open_labels <- c("Assumption Achieved, open plan",
                         "15 years of low returns, open plan",
                         "High volatility, open plan",
                         "7.5% returns, open plan")

lab_s1 <- "Scenario 1 \nAssumption Achieved: \nClosed Plan"
lab_s2 <- "Scenario 2 \nAssumption Achieved: \nOpen Plan"
lab_s3 <- "Scenario 3 \n15 Years of Low Returns"
lab_s4 <- "Scenario 4 \nHigh Volatility"
lab_s5 <- "Scenario 5 \nCurrent Return Assumption"
lab_s6 <- "Scenario 6 \nLower Return Assumption"


runs_all <- c(runs_RS.closed, runs_RS.open)
runs_all_labels <- c(runs_RS.closed_labels, runs_RS.open_labels)



df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim > 0, year %in% 2016:2046)




df_all.stch %<>%   
  select(runname, sim, year, AL, MA, ERC, EEC, PR, ERC_PR, ERC_GF, AL.closed.8pct) %>%
  group_by(runname, sim) %>% 
  mutate(AL.8pct   = ifelse(runname == "RS5.closed", AL.closed.8pct, AL),
         FR_MA     = 100 * MA / AL.8pct,
         FR40less  = cumany(FR_MA <= 40),
         ERC_high  = cumany(ERC_GF >= 10), 
         ERC_hike  = cumany(na2zero(ERC_GF - lag(ERC_GF, 5) >= 5))
         ) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC.q10 = quantile(ERC/1e6, 0.1, na.rm = T),
            ERC.q25 = quantile(ERC/1e6, 0.25, na.rm = T),
            ERC.q50 = quantile(ERC/1e6, 0.5, na.rm = T),
            ERC.q75 = quantile(ERC/1e6, 0.75, na.rm = T),
            ERC.q90 = quantile(ERC/1e6, 0.9, na.rm = T),
            
            ERC_GF.q10 = quantile(ERC_GF, 0.1, na.rm = T),
            ERC_GF.q25 = quantile(ERC_GF, 0.25, na.rm = T),
            ERC_GF.q50 = quantile(ERC_GF, 0.5, na.rm = T),
            ERC_GF.q75 = quantile(ERC_GF, 0.75, na.rm = T),
            ERC_GF.q90 = quantile(ERC_GF, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)
  ) %>% 
  ungroup() %>%
  mutate(runname.lab = factor(runname, 
                              levels = runs_all, 
                              labels = runs_all_labels))





df_all.stch %>% filter(runname == "RS1.closed")
df_all.stch %>% filter(runname == "RS1.open")

df_all.stch %>% filter(runname == "RS2.closed")
df_all.stch %>% filter(runname == "RS3.closed")


df_all.stch %>% filter(runname == "RS4.closed")
df_all.stch %>% filter(runname == "RS5.closed")


results_all %>% filter(runname == "RS1.closed", sim == 0, year %in% c(2015, 2016, 2030:2045) ) %>% select(runname, Tier, year, AL, AL.DC, PR, NC, NC.DC,B,SC, EEC,ERC, GenFund, ERC, nactives)

results_all %>% filter(runname == "RS1.closed", sim == 13 ) %>% select(runname,sim, Tier, year, AL, PR, NC,SC, EEC,ERC, GenFund, ERC, ERC_GF, i.r, UAAL, FR_MA) 
results_all %>% filter(runname == "RS1.open",   sim == 13 ) %>% select(runname,sim, Tier, year, AL, PR, NC,SC, EEC,ERC, GenFund, ERC, ERC_GF, i.r, UAAL) 

results_all %>% filter(runname == "RS4.closed", sim == 0, year %in% c(2016:2020) ) %>% select(runname, Tier, year, AL, PR, SC, ERC, GenFund, ERC_GF) %>% mutate(AL_PR = AL/PR)
results_all %>% filter(runname == "RS5.closed", sim == 0, year %in% c(2016:2020) ) %>% select(runname, Tier, year, AL, PR, SC, ERC, GenFund, ERC_GF) %>% mutate(AL_PR = AL/PR)




#*****************************************************
## 10-year and 30 year compound return  ####
#*****************************************************


results_all %>% filter(runname == "RS1.closed", sim > 0) %>% 
  group_by(sim) %>% 
  summarise(geoReturn30y = get_geoReturn(i.r),
            geoReturn5y = get_geoReturn(i.r[year<=2019])) %>% 
  summarise(negReturn30y = sum(geoReturn30y <= 0)/n(),
            negReturn5y = sum(geoReturn5y <= 0)/n())



#*****************************************************
## Deterministic run  ####
#*****************************************************

df_det <- results_all  %>% 
  filter(runname == "RS1.open", sim == 0, year <= 2045) %>% 
  select(year, AL, MA, B, C, ERC, EEC, ExF, FR_MA, ERC_PR, NC_PR, ERC_GF, ExF_MA, MA_PR) %>% 
  mutate_at(vars(-year, -FR_MA, -ERC_PR, -ExF_MA, -MA_PR, -NC_PR, -ERC_GF), funs(./1e6)) %>% 
  mutate(MA_PR = MA_PR/100) 

df_det.short <- df_det %>% filter(year %in% c(2016, seq(2020, 2045, 5)))

df_det
df_det.short


results_all  %>% 
  filter(runname == "RS1.closed", sim == 0, year <= 2045) %>% 
  select(year, AL, MA, B, C, ERC, EEC, ExF, FR_MA, ERC_PR, NC_PR, ExF_MA, MA_PR, SC, PR) %>% 
  mutate(SC_PR = 100*SC/PR)



fig_projGenFund <- 
  results_all %>% filter(runname == "RS1.closed", sim == 0) %>% 
  ggplot(aes(x = year, y = GenFund/1e6)) + 
  geom_bar(stat = "identity", fill = "skyblue2", color = "grey50", width = 0.5) + 
  theme_bw() + 
  RIG.theme() + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 40000, 2000), labels = comma(seq(0, 40000, 2000))) + 
  labs(title = "Projected General fund of the State of Michigan",
       y = "$Million",
       x = NULL)
fig_projGenFund


#*****************************************************
## Stochastic run: assumption achieved  ####
#*****************************************************

# Distribution of 30-year compound returns

fig_distReturn <- results_all %>% 
  filter(runname == "RS1.closed", sim > 0) %>% 
  group_by(sim) %>% 
  summarize(geoReturn = get_geoReturn(i.r)) %>% 
  ggplot(aes(100*geoReturn)) + theme_bw() + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 0.5, boundary = 0) + 
  geom_vline(xintercept = 0.08 * 100, color = RIG.red) + 
  scale_x_continuous(breaks = seq(0,20,1))+
  labs(title = "Distribution of 30-year compound annual return over 2,000 simulations",
       x = "%",
       y = "Simulatoin count") + 
  RIG.theme()

fig_distReturn





# Projected actuarial liability of MISERS
fig.title <- "Projected actuarial liability of MISERS"
fig.subtitle <- "Closed plan and open plan"
fig_AL <- results_all %>% filter(runname %in% c("RS1.closed", "RS1.open"), year >=2016, sim == 1) %>% 
  mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, AL) %>% 
  ggplot(aes(x = year, y = AL/1e6, color = runname, shape = runname)) + theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,20000)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 40000, 2000)) + 
  scale_color_manual(values = c("black", RIG.red, RIG.red, "black"),  name = NULL 
                     #label  = c("Closed plan", "Open plan")
                     ) + 
  scale_shape_manual(values = c(17, 16, 17, 18),  name = NULL
                     #label  = c("Closed plan", "Open plan")
                     ) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "$Million") + 
  theme(axis.text.x = element_text(size = 8)) + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()

fig_AL





# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8%"
fig_stchDet.FRdist <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  left_join(results_all  %>% 
              filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>% 
              select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75, FR_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  facet_grid(.~runname) +
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,200)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_stchDet.FRdist



# Distribution of ERC ($ value)
fig.title <- "Distribution of employer contributions ($Million) across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8%"
fig_stchDet.ERC.USDdist <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>% 
  #             select(runname, year, ERC_det = ERC) %>% 
  #             mutate(ERC_det = ERC_det/1e6)) %>% 
  select(runname, year, ERC.q25, ERC.q50, ERC.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC.q75", "ERC.q50", "ERC.q25")),
             shape = factor(type, levels = c("ERC.q75", "ERC.q50", "ERC.q25")))) + 
  facet_grid(. ~ runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,1200)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 50000, 100)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "$Million") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_stchDet.ERC.USDdist


# Median ERC ($ value)
fig.title <- "Median employer contributions ($million) across 2,000 simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8; closed plan and open plan"
fig_stchDet.ERC_Med <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>% 
  #             select(runname, year, ERC_det = ERC) %>% 
  #             mutate(ERC_det = ERC_det/1e6)) %>% 
  select(runname, year, ERC.q50) %>% 
  #gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = ERC.q50,
             color = runname,
             shape = runname)) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,500)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 50000, 100)) + 
  scale_color_manual(values = c("black", RIG.red),  name = NULL,
                     label  = c("Scenario 1\nAssumption Achieved: \nClosed Plan", 
                                "Scenario 2\nAssumption Achieved: \nOpen Plan")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("Scenario 1\nAssumption Achieved: \nClosed Plan", 
                                "Scenario 2\nAssumption Achieved: \nOpen Plan")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "$Million") + 
  theme(axis.text.x = element_text(size = 8)) + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_stchDet.ERC_Med


# Median ERC ($ value)
fig.title <- "Median employer contributions as percentage of the projected general fund \nof the State of Michigan across 2,000 simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8; closed plan and open plan"
fig_stchDet.ERC_GF_Med <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>% 
  #             select(runname, year, ERC_det = ERC) %>% 
  #             mutate(ERC_det = ERC_det/1e6)) %>% 
  select(runname, year, ERC_GF.q50) %>% 
  #gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = ERC_GF.q50,
             color = runname,
             shape = runname)) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,6)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 50000, 1)) + 
  scale_color_manual(values = c("black", RIG.red),  name = NULL,
                     label  = c("Scenario 1\nAssumption Achieved: \nClosed Plan", 
                                "Scenario 2\nAssumption Achieved: \nOpen Plan")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("Scenario 1\nAssumption Achieved: \nClosed Plan", 
                                "Scenario 2\nAssumption Achieved: \nOpen Plan")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of general fund") + 
  theme(axis.text.x = element_text(size = 8)) + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_stchDet.ERC_GF_Med



# Distribution of ERC as % general fund
fig.title <- "Distribution of employer contribution as a percentage of general fund across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8%"
fig_stchDet.ERCdist <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>% 
  #             select(runname, year, ERC_det = ERC_GF)) %>% 
  select(runname, year, ERC_GF.q25, ERC_GF.q50, ERC_GF.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_GF.q75", "ERC_GF.q50", "ERC_GF.q25")),
             shape = factor(type, levels = c("ERC_GF.q75", "ERC_GF.q50", "ERC_GF.q25")))) + 
  facet_grid(. ~ runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,10)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 1)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_stchDet.ERCdist


# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_stchDet.FR40less <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, FR40less) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,35)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black",RIG.red),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_stchDet.FR40less


# Risk of sharp increase in ERC/GF
fig.title <- "Probability of employer contribution rising more than 5% of general fund \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_stchDet.ERChike <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black", RIG.red),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_stchDet.ERChike


# Risk of high increase in ERC/GF
fig.title <- "Probability of employer contribution rising above 10% of general fund \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_stchDet.ERChigh <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.open")) %>% 
  select(runname, year, ERC_high) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.red,RIG.blue),  name = "", 
                     label  = c("Closed plan", "Open plan")) + 
  scale_shape_manual(values = c(17,16),  name = "", 
                     label  = c("Closed plan", "Open plan")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_stchDet.ERChigh




#**********************************************************************
## Impact of a period of low returns and impact of high volatility ####
#**********************************************************************


# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8%"
fig_RS.FRdist <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS2.closed", "RS3.closed")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname %in% c("RS2.closed", "RS3.closed"), sim == 0) %>% 
  #             select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -year, -runname) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,300)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_RS.FRdist



# Distribution of ERC as % general fund
fig.title <- "Distribution of employer contribution rates across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8%"
fig_RS.ERCdist <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS2.closed", "RS3.closed")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS2.closed", "RS3.closed"), sim == 0) %>% 
  #             select(runname, year, ERC_det = ERC_GF)) %>% 
  select(runname, year, ERC_GF.q25, ERC_GF.q50, ERC_GF.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_GF.q75", "ERC_GF.q50", "ERC_GF.q25", "ERC_det")),
             shape = factor(type, levels = c("ERC_GF.q75", "ERC_GF.q50", "ERC_GF.q25", "ERC_det")))) + 
  facet_grid(.~runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,10)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 1)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_RS.ERCdist



# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_RS.FR40less <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS2.closed", "RS3.closed")) %>% 
  select(runname, year, FR40less) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,30)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.green, RIG.red,RIG.blue),  name = "", 
                     label  = c("Base case: \nassumption achieved",  "15 years of low returns", "High volatility")) + 
  scale_shape_manual(values = c(15, 17,16),  name = "", 
                     label  = c("Base case: \nassumption achieved", "15 years of low returns", "High volatility")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_RS.FR40less


# Risk of sharp increase in ERC/GF
fig.title <- "Probability of employer contribution rising more than 5% of general fund \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_RS.ERChike <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS2.closed", "RS3.closed")) %>% 
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.green, RIG.red, RIG.blue),  name = "", 
                     label  = c("Base case: \nassumption achieved", "15 years of low returns", "High volatility")) + 
  scale_shape_manual(values = c(15, 17,16),  name = "", 
                     label  = c("Base case: \nassumption achieved", "15 years of low returns", "High volatility")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_RS.ERChike


# Risk of high increase in ERC/GF
fig.title <- "Probability of employer contribution rising above 10% of general fund \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_RS.ERChigh <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS2.closed", "RS3.closed")) %>% 
  select(runname, year, ERC_high) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.green, RIG.red,RIG.blue),  name = "", 
                     label  = c("Base case: \nassumption achieved", "15 years of low returns", "High volatility")) + 
  scale_shape_manual(values = c(15, 17,16),  name = "", 
                     label  = c("Base case: \nassumption achieved", "15 years of low returns", "High volatility")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_RS.ERChigh





#**********************************************************************
##  Lowering DC to 7.5%, true expected return = 7.5% ####
#**********************************************************************

# Deterministic runs

results_dc <- results_all %>%  
  filter(year %in% 2016:2046, runname %in% c("RS4.closed", "RS5.closed"), sim == 0) %>% 
  select(runname, year, FR_MA, MA, AL, AL.closed.8pct, ERC_GF, ERC) %>% 
  mutate(FR_MA.8pct = 100* MA / AL.closed.8pct)
  



fig.title <- "Employer contributions as percentage of general fund \nunder different assumed rate of returns"
fig.subtitle <- "Deterministic annual return of 7.5%"
fig_DC.DetERC <- 
results_dc %>% 
  ggplot(aes(x = year, y = ERC_GF, color = runname, shape = runname)) + 
  geom_line() + 
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,6)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 0.5)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL,
                     label = c("Assumed return  = 8%", "Assumed return = 7.5%")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL,
                     label = c("Assumed return  = 8%", "Assumed return = 7.5%")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) +
  theme_bw() +
  RIG.theme()
fig_DC.DetERC


fig.title <- "Funded ratios \nunder different assumed rate of returns"
fig.subtitle <- "Deterministic annual return of 7.5%"
fig_DC.DetFR <- 
results_dc %>% 
  ggplot(aes(x = year, y = FR_MA.8pct, color = runname, shape = runname)) + 
  geom_line() + 
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,110)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 10)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL,
                     label = c("Assumed return  = 8%", "Assumed return = 7.5%")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL,
                     label = c("Assumed return  = 8%", "Assumed return = 7.5%")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) +
  theme_bw() +
  RIG.theme()
fig_DC.DetFR




# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Expected compound return = 7.5%"
fig_DC.FRdist <- df_all.stch %>% filter(runname %in% c("RS4.closed", "RS5.closed")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname %in% c("RS2.closed", "RS3.closed"), sim == 0) %>% 
  #             select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -year, -runname) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,300)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_DC.FRdist



# Distribution of ERC as % general fund
fig.title <- "Distribution of employer contribution rates across simulations"
fig.subtitle <- "Expected compound return = 7.5%"
fig_DC.ERCdist <- df_all.stch %>% filter(runname %in% c("RS4.closed", "RS5.closed")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS2.closed", "RS3.closed"), sim == 0) %>% 
  #             select(runname, year, ERC_det = ERC_GF)) %>% 
  select(runname, year, ERC_GF.q25, ERC_GF.q50, ERC_GF.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_GF.q75", "ERC_GF.q50", "ERC_GF.q25", "ERC_det")),
             shape = factor(type, levels = c("ERC_GF.q75", "ERC_GF.q50", "ERC_GF.q25", "ERC_det")))) + 
  facet_grid(.~runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,10)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 1)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_DC.ERCdist



# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Expected compound return = 7.5%"
fig_DC.FR40less <- df_all.stch %>% filter(runname %in% c("RS4.closed", "RS5.closed")) %>% 
  select(runname, year, FR40less) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,10)) + 
  scale_y_continuous(breaks = seq(0,200, 1)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.red,RIG.blue),  name = "", 
                     label  = c("Assumed return = 8%", "Assumed return = 7.5%")) + 
  scale_shape_manual(values = c(17,16),  name = "", 
                     label  = c("Assumed return = 8%", "Assumed return = 7.5%")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_DC.FR40less


# Risk of sharp increase in ERC/GF
fig.title <- "Probability of employer contribution rising more than 5% of general fund \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Expected compound return = 7.5%"
fig_DC.ERChike <- df_all.stch %>% filter(runname %in% c("RS4.closed", "RS5.closed")) %>% 
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,40)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.red,RIG.blue),  name = "", 
                     label  = c("Assumed return = 8%", "Assumed return = 7.5%")) + 
  scale_shape_manual(values = c(17,16),  name = "", 
                     label  = c("Assumed return = 8%", "Assumed return = 7.5%")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_DC.ERChike


# Risk of high increase in ERC/GF
fig.title <- "Probability of employer contribution rising above 10% of general fund \nat any time prior to and including the given year"
fig.subtitle <- "Expected compound return = 7.5%"
fig_DC.ERChigh <- df_all.stch %>% filter(runname %in% c("RS4.closed", "RS5.closed")) %>% 
  select(runname, year, ERC_high) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,30)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.red,RIG.blue),  name = "", 
                     label  = c("Assumed return = 8%", "Assumed return = 7.5%")) + 
  scale_shape_manual(values = c(17,16),  name = "", 
                     label  = c("Assumed return = 8%", "Assumed return = 7.5%")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_DC.ERChigh





#*************************************************************************
##                     Summary tables ####
#*************************************************************************



runs_report <- c("RS1.closed", "RS1.open", "RS2.closed", "RS3.closed", "RS4.closed", "RS5.closed")
lvl_measures  <- c("FR40less", "ERC_hike")

# Summary tables for the three major risk measures
tab_summary <- 
  df_all.stch %>% filter(runname %in% c(runs_report ), year == 2046) %>% 
  select(runname, FR40less, ERC_hike) %>% 
  gather(Measure, value, -runname) %>% 
  mutate(runname = factor(runname, levels = runs_report),
         Measure = factor(Measure, levels = lvl_measures)) %>% 
  spread(runname, value)

tab_summary

write.xlsx2(tab_summary1, paste0(Outputs_folder, "tables.xlsx"), sheetName = "summary", append = TRUE)


#*************************************************************************
##                        Saving results                              ####
#*************************************************************************

g.height.1col <- 7*0.8
g.width.1col  <- 10*0.8

g.height.2col <- 6*0.8
g.width.2col  <- 13*0.8

g.height.3col <- 5*0.8
g.width.3col  <- 15*0.8



ggsave(file = paste0(Outputs_folder, "distReturn.png"),   fig_distReturn, height = 7*0.8, width = 10*0.8)




# closed and open plan

ggsave(file = paste0(Outputs_folder, "AL.png"),  fig_AL,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "stchDet.FRdist.png"),  fig_stchDet.FRdist,  height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERCdist.png"), fig_stchDet.ERCdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERC.USDdist.png"), fig_stchDet.ERC.USDdist, height = g.height.2col, width = g.width.2col)


ggsave(file = paste0(Outputs_folder, "stchDet.ERC_Med.png"), fig_stchDet.ERC_Med, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERC_GF_Med.png"), fig_stchDet.ERC_GF_Med, height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "stchDet.FR40less.png"), fig_stchDet.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERChike.png"),  fig_stchDet.ERChike, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERChigh.png"),  fig_stchDet.ERChigh, height = g.height.1col, width = g.width.1col)


# low returns and high volatility
ggsave(file = paste0(Outputs_folder, "RS.FRdist.png"),   fig_RS.FRdist,  height = g.height.3col, width = g.width.3col)
ggsave(file = paste0(Outputs_folder, "RS.ERCdist.png"),  fig_RS.ERCdist, height = g.height.3col, width = g.width.3col)

ggsave(file = paste0(Outputs_folder, "RS.FR40less.png"), fig_RS.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "RS.ERChike.png"),  fig_RS.ERChike,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "RS.ERChigh.png"),  fig_RS.ERChigh,  height = g.height.1col, width = g.width.1col)


# Lowering discount rate

ggsave(file = paste0(Outputs_folder, "DC.DetERC.png"), fig_DC.DetERC,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "DC.DetFR.png"),  fig_DC.DetFR, height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "DC.FRdist.png"),   fig_DC.FRdist,  height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "DC.ERCdist.png"),  fig_DC.ERCdist, height = g.height.2col, width = g.width.2col)

ggsave(file = paste0(Outputs_folder, "DC.FR40less.png"), fig_DC.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "DC.ERChike.png"),  fig_DC.ERChike,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "DC.ERChigh.png"),  fig_DC.ERChigh,  height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "projGenFun.png"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)








# 
# #*****************************************************
# ## Comparing funding policies: proposed ERC cap ####
# #*****************************************************
# 
# # Distribution of funded ratio 
# fig_policy.FRdist <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap"), Tier == "sumTiers") %>% 
#   mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25"))
#   )) + theme_bw() + facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point() + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(50,180)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 20)) + 
#   scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) + 
#   labs(title = "Distribution of funded ratios across simulations under different funding approaches",
#        x = NULL, y = "Percent") + 
#   theme(axis.text.x = element_text(size = 8)) + 
#   RIG.theme()
# 
# fig_policy.FRdist
# 
# 
# # Figure 11
# # Distribution of ERC rate
# fig_policy.ERCdist <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap", "RS1_cap.allTiers"), Tier == "sumTiers") %>% 
#   mutate(runname = factor(runname, levels = c("RS1", "RS1_cap", "RS1_cap.allTiers" ), 
#                           labels = c("without ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
#   select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
#              shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
#   theme_bw() + facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point(size = 2) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(0,50)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 5)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) + 
#   scale_shape_manual(values = c(17, 16, 15),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) + 
#   labs(title = "Distribution of employer contribution rates across simulations under different funding approaches",
#        x = NULL, y = "Percent of payroll(%)") + 
#   theme(axis.text.x = element_text(size = 8)) + 
#   RIG.theme()
# fig_policy.ERCdist
# 
# 
# # Risk of high ERC
# fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different funding approaches"
# fig_policy.ERChigh <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap", "RS1_cap.allTiers" ), Tier == "sumTiers") %>%
#   ggplot(aes(x = year, y = ERC_high, color = factor(runname, levels = c("RS1", "RS1_cap",  "RS1_cap.allTiers" )))) + theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 40)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 5)) +
#   scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "Contribution \napproaches", 
#                      label  = c("without ERC cap", "ERC cap for new hires", "ERC cap for all tiers")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   RIG.theme()
# fig_policy.ERChigh
# 
# 
# 
# # Figure 12
# # Risk of sharp ERC increase
# fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different funding approaches"
# fig_policy.ERChike <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap", "RS1_cap.allTiers"), Tier == "sumTiers") %>%
#   ggplot(aes(x = year, y = ERC_hike, 
#              color = factor(runname, levels = c("RS1", "RS1_cap", "RS1_cap.allTiers")),
#              shape = factor(runname, levels = c("RS1", "RS1_cap", "RS1_cap.allTiers")))) + theme_bw() + 
#   geom_line() + geom_point(size = 2) + 
#   coord_cartesian(ylim = c(0, 80)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "Contribution \nPolicies", 
#                      label  = c("without ERC cap", "ERC cap for new hires", "ERC cap for all tiers")) + 
#   scale_shape_manual(values = c(16, 15, 17),  name = "Contribution \nPolicies", 
#                      label  = c("without ERC cap", "ERC cap for new hires", "ERC cap for all tiers")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   RIG.theme()
# fig_policy.ERChike
# 
# 
# # Impact on EEC rates of new hires after Jan 2019
# 
# # Distribution of EEC rates of t7 members 
# 
# # Figure 13
# # Distribution of EEC rate: ERC cap applied on new hires
# fig.title <- "Distribution of employee contribution rates for new hires \nif the employer contribution cap is applied to new hires"
# fig_policy.EECdist.t7 <- df_t7.stch %>% filter(runname %in% c("RS1_cap"), Tier == "t7") %>% 
#   #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   select(runname, year, EEC_PR.q10, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75, EEC_PR.q90) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("EEC_PR.q90", "EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25", "EEC_PR.q10")),
#              shape = factor(type, levels = c("EEC_PR.q90", "EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25", "EEC_PR.q10")))) + 
#   theme_bw() + 
#   #facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point(size = 2) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(0,20)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 2.5)) + 
#   scale_color_manual(values = c("red", RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
#                      label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile", "10th percentile")) + 
#   scale_shape_manual(values = c(18, 17, 16, 15, 18),  name = NULL, 
#                      label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile", "10th percentile")) +
#   labs(title = fig.title,
#        x = NULL, y = "Percent of payroll (%)") + 
#   theme(axis.text.x = element_text(size = 8)) + 
#   RIG.theme()
# fig_policy.EECdist.t7
# 
# 
# 
# # Risk of high EEC rate (15%) for t7: ERC cap applied on new hires
# fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \nif the proposed ERC cap is applied to new hires"
# fig_policy.EEChigh.t7 <- df_t7.stch %>% 
#   filter(runname %in% c("RS1_cap"), Tier == "t7") %>%
#   ggplot(aes(x = year, y = EEC_high)) + 
#   theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 55)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 5)) +
#   #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
#   #                   label  = c("without ERC cap", "with ERC cap")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   RIG.theme()
# fig_policy.EEChigh.t7
# 
# 
# 
# 
# # Figure 14
# # Distribution of EEC rate: : ERC cap applied to all tiers
# fig.title <- "Distribution of employee contribution rates \nif the employer contribution cap is applied to all plan members"
# fig_policy.EECdist.allTiers <- df_all.stch %>% filter(runname %in% c("RS1_cap.allTiers")) %>% 
#   #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   select(runname, year, EEC_PR.q10, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75, EEC_PR.q90) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("EEC_PR.q90", "EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25", "EEC_PR.q10")),
#              shape = factor(type, levels = c("EEC_PR.q90", "EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25", "EEC_PR.q10")))) + 
#   theme_bw() + 
#   #facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point(size = 2) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(0,70)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 10)) + 
#   scale_color_manual(values = c("red", RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
#                      label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile", "10th percentile")) +
#   scale_shape_manual(values = c(18, 17, 16, 15, 18),  name = NULL, 
#                      label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile", "10th percentile")) +
#   labs(title = fig.title,
#        x = NULL, y = "Percent") + 
#   theme(axis.text.x = element_text(size = 8)) + 
#   RIG.theme()
# fig_policy.EECdist.allTiers
# 
# 
# 
# # Risk of high EEC rate (15%): ERC cap applied to all tiers
# fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \nif the proposed ERC cap is applied to all current tiers"
# fig_policy.EEChigh.allTiers <- df_all.stch %>% 
#   filter(runname %in% c("RS1_cap.allTiers")) %>%
#   ggplot(aes(x = year, y = EEC_high)) + 
#   theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 5)) +
#   #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
#   #                   label  = c("without ERC cap", "with ERC cap")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   RIG.theme()
# fig_policy.EEChigh.allTiers
# 
# 
# 
# 
# #*************************************************************
# ## Comparing return scenarios: low returns in early years ####
# #*************************************************************
# 
# runs.compareRS1 <- c("RS1", "RS1_cap", "RS1_cap.allTiers",
#                      "RS2", "RS2_cap", "RS2_cap.allTiers",
#                      "RS3", "RS3_cap", "RS3_cap.allTiers")
# 
# fig.labels.lowReturns <- c("Scenario 2: \nAssumption Achieved \nStochastic Base Case", 
#                            "Scenario 3: \n5 Years of Low Returns", 
#                            "Scenario 4: \n15 Years of Low Returns")
# 
# # Risk of high ERC
# fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different return scenarios"
# 
# # fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
# #                            "Scenario 3: \n5 years of low returns", 
# #                            "Scenario 4: \n15 years of low returns")
# 
# fig_compareRS1.ERChigh <- df_all.stch %>% filter(runname %in% runs.compareRS1, Tier == "sumTiers") %>% 
#   select(runname, year, ERC_high) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(value = as.numeric(value),
#          RS = str_sub(runname, 1, 3),
#          policy = str_sub(runname, 5),
#          policy = ifelse(policy == "", "no_cap", policy),
#          policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#                          labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
#   facet_grid(. ~ policy) + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 55)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 5)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.ERChigh
# 
# 
# 
# # Risk of high ERC, current policy only
# fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different return scenarios"
# fig.subtitle <- "Current LAFPP funding policy"
# # fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
# #                            "Scenario 3: \n5 years of low returns", 
# #                            "Scenario 4: \n15 years of low returns")
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 3: Expected compound return is 7.1%, standard deviation is 12%"
# n4 <- "\n   Scenario 4: Expected compound return is 6.6%, standard deviation is 12%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_compareRS1.ERChigh.noCap <- df_all.stch %>% filter(runname %in% c("RS1", "RS2", "RS3"), Tier == "sumTiers") %>% 
#   select(runname, year, ERC_high) %>% 
#   ggplot(aes(x = year, y = ERC_high, color = factor(runname, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.ERChigh.noCap
# 
# 
# 
# # Risk of sharp ERC increase
# fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different return scenarios"
# # fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
# #                            "Scenario 3: \n5 years of low returns", 
# #                            "Scenario 4: \n15 years of low returns")
# 
# fig_compareRS1.ERChike <- df_all.stch %>% filter(runname %in% runs.compareRS1, Tier == "sumTiers") %>% 
#   select(runname, year, ERC_hike) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(value = as.numeric(value),
#          RS = str_sub(runname, 1, 3),
#          policy = str_sub(runname, 5),
#          policy = ifelse(policy == "", "no_cap", policy),
#          policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#                          labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>%  
#   ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
#   facet_grid(. ~ policy) + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.ERChike
# 
# 
# #Figure 6
# # Risk of sharp ERC increase, current policy
# fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different return scenarios"
# fig.subtitle <- "Current LAFPP funding policy"
# # fig.labels.lowReturns <- c("Scenario 2: \nAssumption Achieved \nStochastic Base Case", 
# #                            "Scenario 3: \n5 Years of Low Returns", 
# #                            "Scenario 4: \n15 Years of Low Returns")
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 3: Expected compound return is 7.1%, standard deviation is 12%"
# n4 <- "\n   Scenario 4: Expected compound return is 6.6%, standard deviation is 12%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_compareRS1.ERChike.noCap <- df_all.stch %>% filter(runname %in% c("RS1", "RS2", "RS3"), Tier == "sumTiers") %>% 
#   select(runname, year, ERC_hike) %>% 
#   #gather(variable, value, -year, -runname) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "", "no_cap", policy),
#   #        policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#   #                        labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>%  
#   ggplot(aes(x = year, y = ERC_hike, 
#              color = factor(runname, levels = c("RS1", "RS2", "RS3")),
#              shape = factor(runname, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
#   geom_line() + geom_point(size = 2) + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   scale_shape_manual(values = c(17, 15, 16),  name = "", 
#                      label  = fig.labels.lowReturns) +
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   theme(plot.caption=element_text(hjust=0, size = 9)) + 
#   RIG.theme()
# fig_compareRS1.ERChike.noCap
# 
# 
# 
# 
# 
# 
# 
# 
# # Figure 5
# # Risk of low FR
# fig.title <- "Probability of funded ratio below 40% \nat any time prior to and including the given year \nunder different return scenarios"
# fig.subtitle <- "Current LAFPP funding policy"
# # fig.labels.lowReturns <- c("Scenario 2: \nAssumption Achieved \nStochastic Base Case", 
# #                            "Scenario 3: \n5 Years of Low Returns", 
# #                            "Scenario 4: \n15 Years of Low Returns")
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 3: Expected compound return is 7.1%, standard deviation is 12%"
# n4 <- "\n   Scenario 4: Expected compound return is 6.6%, standard deviation is 12%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_compareRS1.FR40less <- df_all.stch %>% filter(runname %in% runs.compareRS1[c(1,4,7)], Tier == "sumTiers") %>% 
#   select(runname, year, FR40less) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "cap", policy, "no_cap" ),
#   #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   ggplot(aes(x = year, y = value, 
#              color = factor(runname, levels = c("RS1", "RS2", "RS3")),
#              shape = factor(runname, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
#   geom_line() + geom_point(size = 2) + 
#   coord_cartesian(ylim = c(0, 10)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 1)) + 
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   scale_shape_manual(values = c(17, 15, 16),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.FR40less
# 
# 
# 
# 
# # Median ERC rate
# fig.title <- "Median employer contribution rates \nunder different return scenarios and funding approaches"
# 
# # fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
# #                            "Scenario 3: \n5 years of low returns", 
# #                            "Scenario 4: \n15 years of low returns")
# 
# fig_compareRS1.MedERC <- df_all.stch %>% filter(runname %in% runs.compareRS1, Tier == "sumTiers") %>% 
#   select(runname, year, ERC_PR.q50) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(value = as.numeric(value),
#          RS = str_sub(runname, 1, 3),
#          policy = str_sub(runname, 5),
#          policy = ifelse(policy == "", "no_cap", policy),
#          policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#                          labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
#   facet_grid(. ~ policy) + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 50)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   labs(title = fig.title,
#        x = NULL, y = "Percent of payroll (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.MedERC
# 
# 
# 
# # Median  FR
# fig.title <- "Median funded ratios \nunder different return scenarios"
# 
# # fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
# #                            "Scenario 3: \n5 years of low returns", 
# #                            "Scenario 4: \n15 years of low returns")
# 
# fig_compareRS1.MedFR <- df_all.stch %>% filter(runname %in% runs.compareRS1[c(1,4,7)], Tier == "sumTiers") %>% 
#   select(runname, year, FR.q50) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "cap", policy, "no_cap" ),
#   #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(runname, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
#   geom_line() + geom_point() + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) + 
#   coord_cartesian(ylim = c(0, 150)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,500, 20)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.lowReturns) + 
#   labs(title = fig.title,
#        x = NULL, y = "Percent of payroll (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.MedFR 
# 
# 
# 
# 
# # Impact on EEC rates of new hires 
# 
# df_t7.stch.RS1 <- df_t7.stch %>% filter(runname %in% c("RS1_cap", "RS2_cap", "RS3_cap"), Tier == "t7") %>% 
#   mutate(runname = factor(runname, labels = fig.labels.lowReturns))
# 
# # Figure 19
# # Distribution of EEC rate
# fig.title <- "Distribution of employee contribution rates for new hires \nif the proposed employer contribution cap is applied to new hires"
# fig_compareRS1.EECdist.t7 <- df_t7.stch.RS1 %>% 
#   #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")),
#              shape = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
#   theme_bw()  + 
#   facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point(size = 2) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(0,20)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 2.5)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) + 
#   scale_shape_manual(values = c(17, 16, 15),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Percent of payroll (%)") + 
#   theme(axis.text.x = element_text(size = 8)) + 
#   RIG.theme()
# fig_compareRS1.EECdist.t7
# 
# 
# 
# # Risk of high EEC rate (15%) for t7
# fig.title <-  "Probability of employee contribution above 15% of payroll for new hires \nat any time prior to and including the given year \nif the proposed ERC cap is applied to new hires"
# fig_compareRS1.EEChigh.t7 <- df_t7.stch.RS1 %>% 
#   ggplot(aes(x = year, y = EEC_high, color = runname)) + 
#   theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
#   #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
#   #                   label  = c("without ERC cap", "with ERC cap")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.EEChigh.t7
# 
# 
# 
# 
# 
# # Impact on EEC rates: applying ERC cap on all current tiers
# 
# df_allTiers.stch.RS1 <- df_all.stch %>% filter(runname %in% c("RS1_cap.allTiers", "RS2_cap.allTiers", "RS3_cap.allTiers")) %>% 
#   mutate(runname = factor(runname, labels = fig.labels.lowReturns))
# 
# # Figure 20
# # Distribution of EEC rate
# fig.title <- "Distribution of employee contribution rates \nif the proposed ERC cap is applied to all current tiers"
# fig_compareRS1.EECdist.allTiers <- df_allTiers.stch.RS1 %>% 
#   #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")),
#              shape = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
#   theme_bw()  + 
#   facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point(size = 2 ) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(0,70)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 10)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) +
#   scale_shape_manual(values = c(17, 16, 15),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) +
#   labs(title = fig.title,
#        x = NULL, y = "Percent of payroll") + 
#   theme(axis.text.x = element_text(size = 8),
#         plot.title=element_text(hjust=0.5)) + 
#   RIG.theme()
# fig_compareRS1.EECdist.allTiers
# 
# 
# 
# # Risk of high EEC rate (15%)
# fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \nif the proposed ERC cap is applied to all current tiers"
# fig_compareRS1.EEChigh.allTiers <- df_allTiers.stch.RS1 %>% 
#   ggplot(aes(x = year, y = EEC_high, color = runname)) + 
#   theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 5)) +
#   scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
#   #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
#   #                   label  = c("without ERC cap", "with ERC cap")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS1.EEChigh.allTiers
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #*************************************************************************
# ## Comparing return scenarios: alternative capital market assumptions ####
# #*************************************************************************
# runs.compareRS2 <- c("RS1", "RS1_cap", "RS1_cap.allTiers",
#                      "RS4", "RS4_cap", "RS4_cap.allTiers",
#                      "RS5", "RS5_cap", "RS5_cap.allTiers")
# 
# fig.labels.altAssumptions <- c("Scenario 2: \nAssumption Achieved \nStochastic Base Case", 
#                                "Scenario 5: \nHigh Volatility", 
#                                "Scenario 6: \nTarget Asset Allocation")
# 
# df_all.stch.RS2 <-  df_all.stch %>% filter(runname %in% runs.compareRS2, Tier == "sumTiers")
# 
# # Risk of high ERC
# fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different return scenarios"
# 
# fig_compareRS2.ERChigh <- df_all.stch.RS2 %>% 
#   select(runname, year, ERC_high) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(value = as.numeric(value),
#          RS = str_sub(runname, 1, 3),
#          policy = str_sub(runname, 5),
#          policy = ifelse(policy == "", "no_cap", policy),
#          policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#                          labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   facet_grid(. ~ policy) + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 59)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 5)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.ERChigh
# 
# 
# # Risk of high ERC, current policy only
# fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different return scenarios"
# fig.subtitle <- "Current LAFPP funding policy%"
# # fig.labels.altAssumptions <- c("Scenario 2: \nAssumption achieved \nstochastic", 
# #                                "Scenario 5: \nHigh volatility \nreflecting market forecasts", 
# #                                "Scenario 6: \nLow expected return \nbased on LAFPP target portfolio")
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 5: Expected compound return is 7.5%, standard deviation is 17.2%"
# n4 <- "\n   Scenario 6: Expected compound return is 6.1%, standard deviation is 13.4%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_compareRS2.ERChigh.noCap <- df_all.stch %>% filter(runname %in% c("RS1", "RS4", "RS5"), Tier == "sumTiers") %>% 
#   select(runname, year, ERC_high) %>% 
#   ggplot(aes(x = year, y = ERC_high, color = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        caption = fig.caption,
#        subtitle = fig.subtitle,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.ERChigh.noCap
# 
# 
# 
# 
# 
# 
# # Risk of sharp ERC increase
# fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different return scenarios"
# 
# fig_compareRS2.ERChike <- df_all.stch.RS2 %>% 
#   select(runname, year, ERC_hike) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(value = as.numeric(value),
#          RS = str_sub(runname, 1, 3),
#          policy = str_sub(runname, 5),
#          policy = ifelse(policy == "", "no_cap", policy),
#          policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#                          labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   facet_grid(. ~ policy) + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.ERChike
# 
# 
# 
# 
# 
# # Figure 10
# # Risk of sharp ERC increase, current policy
# fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different return scenarios"
# fig.subtitle <- "Current LAFPP funding policy"
# # fig.labels.altAssumptions <- c("Scenario 2: \nAssumption Achieved \nStochastic Base Case", 
# #                                "Scenario 5: \nHigh volatility", 
# #                                "Scenario 6: \nLow expected return \nbased on LAFPP target portfolio")
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 5: Expected compound return is 7.5%, standard deviation is 17.2%"
# n4 <- "\n   Scenario 6: Expected compound return is 6.1%, standard deviation is 13.4%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_compareRS2.ERChike.noCap <- df_all.stch %>% filter(runname %in% c("RS1", "RS4", "RS5"), Tier == "sumTiers") %>% 
#   select(runname, year, ERC_hike) %>% 
#   #gather(variable, value, -year, -runname) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "", "no_cap", policy),
#   #        policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#   #                        labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>%  
#   ggplot(aes(x = year, y = ERC_hike, 
#              color = factor(runname, levels = c("RS1", "RS4", "RS5")),
#              shape = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   geom_line() + geom_point(size = 2) + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   scale_shape_manual(values = c(17, 15, 16),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   theme(plot.caption=element_text(hjust=0, size = 9)) + 
#   RIG.theme()
# fig_compareRS2.ERChike.noCap
# 
# 
# 
# 
# # Figure 8
# # Risk of low FR
# fig.title <- "Probability of funded ratio below 40% \nat any time prior to and including the given year \nunder different return scenarios"
# fig.subtitle <- "Current LAFPP funding policy"
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 5: Expected compound return is 7.5%, standard deviation is 17.2%"
# n4 <- "\n   Scenario 6: Expected compound return is 6.1%, standard deviation is 13.4%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_compareRS2.FR40less <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
#   select(runname, year, FR40less) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "cap", policy, "no_cap" ),
#   #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   ggplot(aes(x = year, y = value, 
#              color = factor(runname, levels = c("RS1", "RS4", "RS5")),
#              shape = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   geom_line() + geom_point(size = 2) + 
#   coord_cartesian(ylim = c(0, 20)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 2.5)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   scale_shape_manual(values = c(17, 15, 16),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.FR40less
# 
# 
# 
# 
# # Median ERC rate
# fig.title <- "Median employer contribution rates \nunder different return scenarios and funding approaches"
# fig.subtitle <- "Current LAFPP funding policy"
# 
# fig_compareRS2.MedERC <- df_all.stch.RS2 %>% 
#   select(runname, year, ERC_PR.q50) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(value = as.numeric(value),
#          RS = str_sub(runname, 1, 3),
#          policy = str_sub(runname, 5),
#          policy = ifelse(policy == "", "no_cap", policy),
#          policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
#                          labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   facet_grid(. ~ policy) + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 50)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        x = NULL, y = "Percent of payroll (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.MedERC
# 
# 
# # Figure 9
# # Median ERC rate, current policy only
# fig.title <- "Median employer contribution rates \nunder different return scenarios and funding approaches"
# fig.subtitle <- "Current LAFPP funding policy"
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 5: Expected compound return is 7.5%, standard deviation is 17.2%"
# n4 <- "\n   Scenario 6: Expected compound return is 6.1%, standard deviation is 13.4%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_compareRS2.MedERC.noCap <- df_all.stch.RS2 %>% filter(runname %in% c("RS1", "RS4", "RS5"), Tier == "sumTiers") %>%
#   select(runname, year, ERC_PR.q50) %>% 
#   ggplot(aes(x = year, y = ERC_PR.q50, 
#              color = factor(runname, levels = c("RS1", "RS4", "RS5")),
#              shape = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   geom_line() + geom_point(size = 2) + 
#   coord_cartesian(ylim = c(0, 50)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   scale_shape_manual(values = c(17, 15, 16),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Percent of payroll (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.MedERC.noCap
# 
# 
# 
# 
# # Median ERC rate: current policy only
# fig.title <- "Median employer contribution rates \nunder different return scenarios and funding approaches"
# fig.subtitle <- "Current LAFPP funding policy"
# 
# fig_compareRS2.MedERC.noCap <- df_all.stch.RS2 %>% filter(runname %in% c("RS1", "RS4", "RS5"), Tier == "sumTiers") %>% 
#   select(runname, year, ERC_PR.q50) %>% 
#   ggplot(aes(x = year, y = ERC_PR.q50, color = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 50)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Percent of payroll (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.MedERC.noCap
# 
# 
# # Figure 7
# # Median FR
# fig.title <- "Median funded ratios \nunder different return scenarios"
# fig.subtitle <- "Current LAFPP funding policy"
# fig_compareRS2.MedFR <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
#   select(runname, year, FR.q50) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "cap", policy, "no_cap" ),
#   #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   ggplot(aes(x = year, y = value, 
#              color = factor(runname, levels = c("RS1", "RS4", "RS5")),
#              shape = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
#   geom_line() + geom_point(size = 2) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) + 
#   coord_cartesian(ylim = c(0, 150)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,500, 20)) +
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   scale_shape_manual(values = c(17, 15, 16),  name = "", 
#                      label  = fig.labels.altAssumptions) + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        caption = fig.caption,
#        x = NULL, y = "Percent") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.MedFR 
# 
# 
# 
# # distribuiton FR with current funding policy under different return scenarios
# fig.title <- "Distribution of funded ratios \nunder different return scenarios"
# fig.subtitle <- "Current funding policy, without ERC cap"
# fig.labels.pctile3 <- c("25th percentile",  "50th percentile", "75th percentile")
# fig.labels.pctile5 <- c("10th percentile",  "25th percentile", "50th percentile", "75th percentile", "90th percentile")
# fig_compareRS2.distFR <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
#   select(runname, year, FR.q10, FR.q25, FR.q50, FR.q75, FR.q90) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(runname = factor(runname, labels = fig.labels.altAssumptions)) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "cap", policy, "no_cap" ),
#   #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(variable, levels = c("FR.q90", "FR.q75", "FR.q50", "FR.q25", "FR.q10"),
#                                                  labels = rev(fig.labels.pctile5) ))) + 
#   theme_bw() + 
#   facet_grid(. ~ runname) + 
#   geom_line() + geom_point() + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) + 
#   coord_cartesian(ylim = c(0, 200)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,500, 20)) +
#   scale_color_manual(values = rev(c("red", RIG.red, RIG.blue, RIG.green, "green")),  name = "") + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        x = NULL, y = "Funded ratio (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.distFR
# 
# 
# 
# 
# # distribuiton ERC with current funding policy under different return scenarios
# fig.title <- "Distribution of funded ratios \nunder different return scenarios"
# fig.subtitle <- "Current funding policy, without ERC cap"
# fig.labels.pctile3 <- c("25th percentile",  "50th percentile", "75th percentile")
# fig.labels.pctile5 <- c("10th percentile",  "25th percentile", "50th percentile", "75th percentile", "90th percentile")
# fig_compareRS2.distERC <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
#   select(runname, year, ERC_PR.q10, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_PR.q90) %>% 
#   gather(variable, value, -year, -runname) %>% 
#   mutate(runname = factor(runname, labels = fig.labels.altAssumptions)) %>% 
#   # mutate(value = as.numeric(value),
#   #        RS = str_sub(runname, 1, 3),
#   #        policy = str_sub(runname, 5),
#   #        policy = ifelse(policy == "cap", policy, "no_cap" ),
#   #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   ggplot(aes(x = year, y = value, color = factor(variable, levels = c("ERC_PR.q90", "ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25", "ERC_PR.q10"),
#                                                  labels = rev(fig.labels.pctile5) ))) + 
#   theme_bw() + 
#   facet_grid(. ~ runname) + 
#   geom_line() + geom_point() + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) + 
#   coord_cartesian(ylim = c(0, 80)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,500, 10)) +
#   scale_color_manual(values = c("red", RIG.red, RIG.blue, RIG.green, "green"),  name = "") + 
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        x = NULL, y = "Percent of payroll (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.distERC
# 
# 
# 
# 
# 
# 
# 
# 
# # Figure 21
# # Impact on EEC rates of new hires
# 
# df_t7.stch.RS2 <- df_t7.stch %>% filter(runname %in% c("RS1_cap", "RS4_cap", "RS5_cap"), Tier == "t7") %>% 
#   mutate(runname = factor(runname, labels = fig.labels.altAssumptions))
# 
# # Distribution of EEC rate
# fig.title <- "Distribution of employee contribution rates for new hires \nif the proposed ERC cap is applied to new hires"
# fig_compareRS2.EECdist.t7 <- df_t7.stch.RS2 %>% 
#   #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")),
#              shape = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
#   theme_bw()  + 
#   facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point(size =2) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(0,20)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 2.5)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) +
#   scale_shape_manual(values = c(17, 16, 15),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) +
#   labs(title = fig.title,
#        x = NULL, y = "Percent of payroll (%)") + 
#   theme(axis.text.x = element_text(size = 8)) + 
#   RIG.theme()
# fig_compareRS2.EECdist.t7
# 
# 
# 
# # Risk of high EEC rate (15%) for t7
# fig.title <-  "Probability of employee contribution above 15% of payroll for new hires \nat any time prior to and including the given year \nif the proposed ERC cap is applied to new hires"
# fig_compareRS2.EEChigh.t7 <- df_t7.stch.RS2 %>% 
#   ggplot(aes(x = year, y = EEC_high, color = runname)) + 
#   theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
#   #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
#   #                   label  = c("without ERC cap", "with ERC cap")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.EEChigh.t7
# 
# 
# # Figure 22
# # Impact on EEC rates: applying the proposed ERC cap to all tiers
# 
# df_allTiers.stch.RS2 <- df_all.stch %>% filter(runname %in% c("RS1_cap.allTiers", "RS4_cap.allTiers", "RS5_cap.allTiers")) %>% 
#   mutate(runname = factor(runname, labels = fig.labels.altAssumptions))
# 
# # Distribution of EEC rate
# fig.title <- "Distribution of employee contribution rate for new hires \nif the proposed employer contribution cap is applied to all current tiers"
# fig_compareRS2.EECdist.allTiers <- df_allTiers.stch.RS2 %>% 
#   #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
#   select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
#   gather(type, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value,
#              color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")),
#              shape = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
#   theme_bw()  + 
#   facet_grid(.~runname) + 
#   geom_line() + 
#   geom_point(size = 2) + 
#   geom_hline(yintercept = 100, linetype = 2, size = 1) +
#   coord_cartesian(ylim = c(0,70)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 500, 10)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) + 
#   scale_shape_manual(values = c(17, 16, 15),  name = NULL, 
#                      label  = c("75th percentile", "50th percentile", "25th percentile")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Percent") + 
#   theme(axis.text.x = element_text(size = 8)) + 
#   RIG.theme()
# fig_compareRS2.EECdist.allTiers
# 
# 
# 
# # Risk of high EEC rate (15%) for t7
# fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \n\nif the proposed ERC cap is applied to all current tiers "
# fig_compareRS2.EEChigh.allTiers <- df_allTiers.stch.RS2 %>% 
#   ggplot(aes(x = year, y = EEC_high, color = runname)) + 
#   theme_bw() + 
#   geom_line() + geom_point() + 
#   coord_cartesian(ylim = c(0, 100)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0,100, 10)) +
#   scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
#   #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
#   #                   label  = c("without ERC cap", "with ERC cap")) + 
#   labs(title = fig.title,
#        x = NULL, y = "Probability (%)") + 
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   RIG.theme()
# fig_compareRS2.EEChigh.allTiers
# 
# 
# 
# 
# #***********************************************************************
# #  individual runs ####
# #***********************************************************************
# 
# results.stch <- results_all %>% filter(runname == "RS1", sim > 0)
# 
# DC <- results.stch$i[1]
# FR.MA.year1 <- results.stch$FR_MA[1]
# 
# 
# # FR% and ERC% in 4 indiv runs
# df_geoReturn <-  results.stch %>% group_by(sim) %>%
#   summarize(geoReturn = get_geoReturn(i.r)) %>% 
#   arrange(geoReturn) %>% 
#   mutate(order.ir = 1:n()) %>% 
#   mutate(dist2DC = abs(geoReturn - DC)) %>% 
#   arrange(dist2DC) %>%
#   mutate(order.dist = 1:n())
# 
# 
# # looking for runs with geo return at around 0.725%, 25th-tile and 75th-tile  
# df_geoReturn$geoReturn %>% quantile
# df_indiv_selcet <- df_geoReturn %>% 
#   filter(order.ir == round(0.25*n()) |
#            order.ir == round(0.75*n()) | 
#            order.dist %in% 1:2) %>% 
#   arrange(geoReturn)
# 
# #(geoReturn >= 0.0724& geoReturn <= 0.0726))
# df_indiv_selcet
# 
# # selecting runs
# results_indiv <- results.stch %>% filter(sim %in% df_indiv_selcet$sim) %>% 
#   left_join(df_indiv_selcet %>% select(sim, order.ir, geoReturn)) %>% 
#   mutate(plot.label = paste0(round(100*geoReturn, digits = 2), "%"))
# 
# results_indiv$plot.label
# df_indiv_selcet
# 
# # Creating graphs
# plot.label <- paste0(round(100*df_indiv_selcet$geoReturn, digits = 2), "%")
# 
# 
# g.ind.FR <- results_indiv %>% 
#   ggplot(aes(x = year, y = FR_MA, color = factor(order.ir), label = plot.label)) + theme_bw() + 
#   geom_line(linetype = 1) + geom_point() +
#   geom_hline(yintercept = c(100, FR.MA.year1), linetype = 2, color = c("red", "black") ) +
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5)) ) + 
#   scale_y_continuous(breaks = c(seq(0, 500, 10))) + 
#   scale_color_manual(values = c(RIG.red,"dodgerblue", RIG.blue, RIG.green),
#                      label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
#   labs(title = "Funded ratio of selected individual simulations",
#        x = "Year", y = "Funded ratio (%, based on market value asset)") + 
#   RIG.theme()
# g.ind.FR  
# 
# 
# g.ind.ERC <-  results_indiv %>% 
#   ggplot(aes(x = year, y = ERC_PR, color = factor(order.ir), label = plot.label)) + theme_bw() + 
#   geom_line(linetype = 1) + geom_point() + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_color_manual(values = c(RIG.red,"dodgerblue", RIG.blue, RIG.green),
#                      label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
#   labs(title = "Employer contribution rate of \nselected individual simulations",
#        x = "Year", y = "Contribution as % of payroll") + 
#   RIG.theme()
# g.ind.ERC
# 
# 
# # Rolling compound return
# 
# roundedReturn <- round(100*df_indiv_selcet$geoReturn,2)
# 
# hlineNotes <- c(min(roundedReturn), DC*100, max(roundedReturn))
# 
# 
# results_indiv %<>% group_by(sim) %>%  
#   mutate(rollgeoReturn   = get_rollingReturns(i.r, "moving", window = 5),
#          expandgeoReturn = get_rollingReturns(i.r, "expanding")) 
# 
# 
# 
# g.ind.rollgeoReturn <-  results_indiv %>% 
#   ggplot(aes(x = year, y = rollgeoReturn*100, color = factor(order.ir))) + theme_bw() + 
#   geom_line(linetype = 1) + geom_point() + 
#   geom_hline(yintercept = unique(round(100*df_indiv_selcet$geoReturn,2)), linetype = 3)+
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   # scale_y_continuous(breaks = c(seq(-20,30,5), unique(round(100*df_indiv_selcet$geoReturn,2)))) + 
#   annotate("text", label = hlineNotes, x = 2016 , y = hlineNotes, size = 4, colour = c(RIG.red, RIG.blue, RIG.green)) + 
#   scale_color_manual(values = c(RIG.red,"dodgerblue", RIG.blue, RIG.green),
#                      label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
#   labs(title = "Rolling 5-year geometric returns for \nselected individual simulations",
#        x = "Year", y = "Percent") + 
#   RIG.theme()
# g.ind.rollgeoReturn
# 
# 
# 
# g.ind.expandgeoReturn <-  results_indiv %>% 
#   ggplot(aes(x = year, y = expandgeoReturn*100, color = factor(order.ir))) + theme_bw() + 
#   geom_line(linetype = 1) + geom_point() + 
#   geom_hline(yintercept = unique(round(100*df_indiv_selcet$geoReturn,2)), linetype = 3)+
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   # scale_y_continuous(breaks = c(seq(-20,30,5), unique(round(100*df_indiv_selcet$geoReturn,2)))) + 
#   annotate("text", label = hlineNotes, x = 2016 , y = hlineNotes, size = 4, colour = c(RIG.red, RIG.blue, RIG.green)) + 
#   scale_color_manual(values = c(RIG.red,"dodgerblue", RIG.blue, RIG.green),
#                      label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
#   labs(title = "Rolling geometric returns up to a given year for \nselected individual simulations",
#        x = "Year", y = "Percent") + 
#   RIG.theme()
# g.ind.expandgeoReturn
# 
# 
# g.ind.annualReturn <-  results_indiv %>% 
#   ggplot(aes(x = year, y = i.r*100, color = factor(order.ir))) + theme_bw() + 
#   geom_line(linetype = 1) + geom_point() + 
#   geom_hline(yintercept = unique(round(100*df_indiv_selcet$geoReturn,2)), linetype = 3)+
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   # scale_y_continuous(breaks = c(seq(-20,30,5), unique(round(100*df_indiv_selcet$geoReturn,2)))) + 
#   #annotate("text", label = hlineNotes, x = 2014 , y = hlineNotes, size = 3, colour = c(RIG.red, RIG.blue, RIG.green)) + 
#   annotate("text", label = hlineNotes[1], x = 2015 , y = hlineNotes[1], size = 3, colour = c(RIG.red), vjust = 1.2) + 
#   annotate("text", label = hlineNotes[2], x = 2015 , y = hlineNotes[2], size = 3, colour = c(RIG.blue)) + 
#   annotate("text", label = hlineNotes[3], x = 2015 , y = hlineNotes[3], size = 3, colour = c(RIG.green), vjust = -0.4) + 
#   
#   scale_color_manual(values = c(RIG.red,"dodgerblue", RIG.blue, RIG.green),
#                      label = plot.label, name = "Compound \nreturn of \nindiv. sim") +
#   labs(title = " Annual returns of \nselected individual simulations",
#        x = "Year", y = "Percent") + 
#   RIG.theme()
# 
# g.ind.annualReturn
# 
# 
# results.stch %>% filter(sim == 1977) %>% summarise(Nneg = sum(i.r < 0), minR = min(i.r))
# results.stch %>% filter(sim == 1977) %>% select(year, sim, FR_MA, ERC_PR)
# 
# 
# #*************************************************************************
# ##                     Summary tables ####
# #*************************************************************************
# 
# 
# lvl_policies  <- c("", "_cap", "_cap.allTiers")
# lvl_scenarios <- paste0(paste0("RS", rep(1:5, 3)), rep(lvl_policies, each = 5))
# lvl_measures  <- c("FR40less", "ERC_hike", "ERC_high") 
# 
# # Summary tables for the three major risk measures
# tab_summary1 <- 
#   df_all.stch %>% filter(runname %in% c(runs_all[1:15]), year == 2045, Tier == "sumTiers") %>% 
#   select(runname, FR40less, ERC_hike, ERC_high) %>% 
#   gather(Measure, value, -runname) %>% 
#   mutate(runname = factor(runname, levels = lvl_scenarios),
#          Measure = factor(Measure, levels = lvl_measures)) %>% 
#   spread(runname, value)
# 
# 
# # Summary tables for EEC risks
# tab_summary2 <- 
#   df_all.stch %>% filter(runname %in% c(runs_all[1:15]), year == 2045) %>% 
#   mutate(policy = str_sub(runname, 5)) %>% 
#   filter(!(policy == "cap" & Tier == "sumTiers"), !(policy == "cap" & Tier == "xt7")) %>% 
#   select(runname, EEC_high, EEC_PR.q75) %>% 
#   gather(Measure, value, -runname) %>% 
#   mutate(runname = factor(runname, levels = lvl_scenarios)) %>% 
#   spread(runname, value)
# 
# 
# 
# 
# #*************************************************************************
# ##                        Saving results                              ####
# #*************************************************************************
# 
# g.height <- 5
# g.width <- 12
# 
# # g.height.3col <- 
# # g.width.3col <- 
# 
# ggsave(file = paste0(Outputs_folder, "fig1_stchDet.FRdist.pdf"), fig_stchDet.FRdist, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig2_stchDet.FR40less.pdf"), fig_stchDet.FR40less, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig3_stchDet.ERCdist.pdf"), fig_stchDet.ERCdist, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig4_stchDet.ERChike.pdf"), fig_stchDet.ERChike, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig5_compareRS1.FR40less.pdf"), fig_compareRS1.FR40less, height = 7*0.8, width = 9*0.8)
# ggsave(file = paste0(Outputs_folder, "fig6_compareRS1.ERChike.noCap.pdf"), fig_compareRS1.ERChike.noCap, height = 0.8*7, width = 0.8*9)
# ggsave(file = paste0(Outputs_folder, "fig7_compareRS2.MedFR.pdf"), fig_compareRS2.MedFR, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig8_compareRS2.FR40less.pdf"), fig_compareRS2.FR40less, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig9_compareRS2.MedERC.noCap.pdf"), fig_compareRS2.MedERC.noCap, height = 0.8*7, width = 0.8*10)
# ggsave(file = paste0(Outputs_folder, "fig10_compareRS2.ERChike.noCap.pdf"), fig_compareRS2.ERChike.noCap, height = 0.8*7, width = 0.8*10)
# ggsave(file = paste0(Outputs_folder, "fig11_policy.ERCdist.pdf"), fig_policy.ERCdist, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig12_policy.ERChike.pdf"), fig_policy.ERChike, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig13_policy.EECdist.t7.pdf"), fig_policy.EECdist.t7, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig14_policy.EECdist.allTiers.pdf"), fig_policy.EECdist.allTiers, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig18_distReturn.pdf"), fig_distReturn, height = 7, width = 10)
# ggsave(file = paste0(Outputs_folder, "fig19_compareRS1.EECdist.t7.pdf"), fig_compareRS1.EECdist.t7, height = 6*0.8, width = 15*0.8)
# ggsave(file = paste0(Outputs_folder, "fig20_compareRS1.EECdist.allTiers.pdf"), fig_compareRS1.EECdist.allTiers, height = 6*0.8, width = 15*0.8)
# ggsave(file = paste0(Outputs_folder, "fig21_compareRS2.EECdist.t7.pdf"), fig_compareRS2.EECdist.t7, height = 6*0.7, width = 15*0.7)
# ggsave(file = paste0(Outputs_folder, "fig22_compareRS2.EECdist.allTiers.pdf"), fig_compareRS2.EECdist.allTiers, height = 6*0.7, width = 15*0.7)
# 
# 
# ggsave(file = paste0(Outputs_folder, "fig18_distReturn.png"), fig_distReturn, height = 7, width = 10)
# ggsave(file = paste0(Outputs_folder, "fig_distFR30.png"), fig_distFR30, height = 7, width = 10)
# ggsave(file = paste0(Outputs_folder, "fig2_stchDet.FR40less.png"), fig_stchDet.FR40less, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig_stchDet.ERChigh.png"), fig_stchDet.ERChigh, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig4_stchDet.ERChike.png"), fig_stchDet.ERChike, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig_stchDet.3measures.png"), fig_stchDet.3measures, height = g.height, width = g.width)
# ggsave(file = paste0(Outputs_folder, "fig_stchDet.2measures.png"), fig_stchDet.2measures, height = 1.1*g.height, width = g.width)
# ggsave(file = paste0(Outputs_folder, "fig3_stchDet.ERCdist.png"), fig_stchDet.ERCdist, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig1_stchDet.FRdist.png"), fig_stchDet.FRdist, height = 7*0.8, width = 10*0.8)
# 
# 
# 
# ggsave(file = paste0(Outputs_folder, "fig_policy.FRdist.png"), fig_policy.FRdist, height = 7, width = 13)
# ggsave(file = paste0(Outputs_folder, "fig11_policy.ERCdist.png"), fig_policy.ERCdist, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig_policy.ERChigh.png"), fig_policy.ERChigh, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig12_policy.ERChike.png"), fig_policy.ERChike, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig13_policy.EECdist.t7.png"), fig_policy.EECdist.t7, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig_policy.EEChigh.t7.png"), fig_policy.EEChigh.t7, height = 7*0.8, width = 9*0.8)
# ggsave(file = paste0(Outputs_folder, "fig14_policy.EECdist.allTiers.png"), fig_policy.EECdist.allTiers, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig_policy.EEChigh.allTiers.png"), fig_policy.EEChigh.allTiers, height = 7*0.8, width = 9*0.8)
# 
# 
# ggsave(file = paste0(Outputs_folder, "fig_compareRS1.MedFR.png"), fig_compareRS1.MedFR, height = 7*0.9, width = 10*0.9)
# ggsave(file = paste0(Outputs_folder, "fig5_compareRS1.FR40less.png"), fig_compareRS1.FR40less, height = 7*0.8, width = 9*0.8)
# ggsave(file = paste0(Outputs_folder, "fig6_compareRS1.ERChike.noCap.png"), fig_compareRS1.ERChike.noCap, height = 0.8*7, width = 0.8*9)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS1.ERChigh.noCap.png"), fig_compareRS1.ERChigh.noCap, height = 0.8*7, width = 0.8*9)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS1.MedERC.png"), fig_compareRS1.MedERC, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS1.ERChigh.png"), fig_compareRS1.ERChigh, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS1.ERChike.png"), fig_compareRS1.ERChike, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig19_compareRS1.EECdist.t7.png"), fig_compareRS1.EECdist.t7, height = 6*0.8, width = 15*0.8)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS1.EEChigh.t7.png"), fig_compareRS1.EEChigh.t7, height = 7, width = 10)
# ggsave(file = paste0(Outputs_folder, "fig20_compareRS1.EECdist.allTiers.png"), fig_compareRS1.EECdist.allTiers, height = 6*0.8, width = 15*0.8)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS1.EEChigh.allTiers.png"), fig_compareRS1.EEChigh.allTiers, height = 7, width = 10)
# 
# 
# ggsave(file = paste0(Outputs_folder, "fig7_compareRS2.MedFR.png"), fig_compareRS2.MedFR, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig8_compareRS2.FR40less.png"), fig_compareRS2.FR40less, height = 7*0.8, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig9_compareRS2.MedERC.noCap.png"), fig_compareRS2.MedERC.noCap, height = 0.8*7, width = 0.8*10)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.ERChigh.noCap.png"), fig_compareRS2.ERChigh.noCap, height = 0.8*7, width = 0.8*10)
# ggsave(file = paste0(Outputs_folder, "fig10_compareRS2.ERChike.noCap.png"), fig_compareRS2.ERChike.noCap, height = 0.8*7, width = 0.8*10)
# ggsave(file = paste0(Outputs_folder, "fig21_compareRS2.EECdist.t7.png"), fig_compareRS2.EECdist.t7, height = 6*0.7, width = 15*0.7)
# ggsave(file = paste0(Outputs_folder, "fig22_compareRS2.EECdist.allTiers.png"), fig_compareRS2.EECdist.allTiers, height = 6*0.7, width = 15*0.7)
# 
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.MedERC.png"), fig_compareRS2.MedERC, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.distFR.png"), fig_compareRS2.distFR, height = 5*0.9, width = 15*0.9)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.distERC.png"), fig_compareRS2.distERC, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.ERChigh.png"), fig_compareRS2.ERChigh, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.ERChike.png"), fig_compareRS2.ERChike, height = 0.8*5, width = 0.8*15)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.EEChigh.t7.png"), fig_compareRS2.EEChigh.t7, height = 7, width = 10)
# ggsave(file = paste0(Outputs_folder, "fig_compareRS2.EEChigh.allTiers.png"), fig_compareRS2.EEChigh.allTiers, height = 7, width = 10)
# 
# 
# # Appendix
# g.height <- 5.5
# g.width <- 11
# 
# ggsave(file = paste0(Outputs_folder, "fig_appx.indivRuns.annualReturn.png"), g.ind.annualReturn, height = g.height, width = g.width)
# ggsave(file = paste0(Outputs_folder, "fig_appx.indivRuns.expandReturn.png"), g.ind.expandgeoReturn, height = g.height, width = g.width)
# ggsave(file = paste0(Outputs_folder, "fig_appx.indivRuns.FRpath.png"),  g.ind.FR, height = g.height, width = g.width)
# ggsave(file = paste0(Outputs_folder, "fig_appx.indivRuns.ERCpath.png"), g.ind.ERC, height = g.height, width = g.width)
# 
# 
# write.xlsx2(df_det, paste0(Outputs_folder, "tables.xlsx"), sheetName = "det_full")
# write.xlsx2(df_det.short, paste0(Outputs_folder, "tables.xlsx"), sheetName = "det_short", append = TRUE)
# write.xlsx2(tab_summary1, paste0(Outputs_folder, "tables.xlsx"), sheetName = "summary1", append = TRUE)
# write.xlsx2(tab_summary2, paste0(Outputs_folder, "tables.xlsx"), sheetName = "summary2", append = TRUE)
# 
# 
# 
# 
# 
# #**************************************************************************
# ## Checking results for in-text numbers                              ####
# #**************************************************************************
# 
# 
# # Stochastic results: 
# df_all.stch %>% filter(runname == "RS1")
# df_all.stch %>% filter(runname == "RS1_FR075")
# 
# # comparing policies:
# df_all.stch %>% filter(runname == "RS1")
# df_all.stch %>% filter(runname == "RS1_cap")
# df_all.stch %>% filter(runname == "RS1_cap.allTiers")
# 
# df_all.stch %>% filter(runname == "RS1_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS1_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# 
# 
# # comparing return scenarios 1:
# df_all.stch %>% filter(runname == "RS1")
# df_all.stch %>% filter(runname == "RS2")
# df_all.stch %>% filter(runname == "RS3")
# 
# df_all.stch %>% filter(runname == "RS1_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS2_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS3_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# 
# results_all %>% filter(runname == "RS1_cap", Tier == "t7", sim == 0, year %in% c(2016, 2030, 2045) ) %>% select(runname, Tier, year, AL, PR, SC, B) %>% mutate(AL_PR = AL/PR)
# results_all %>% filter(runname == "RS1_cap", Tier == "xt7", sim == 0, year %in% c(2016, 2030, 2045) ) %>% select(runname, Tier, year, AL, PR, SC, B) %>% mutate(AL_PR = AL/PR)
# 
# df_all.stch %>% filter(runname == "RS1_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS2_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS3_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# 
# 
# # comparing return scenarios 2:
# df_all.stch %>% filter(runname == "RS1")
# df_all.stch %>% filter(runname == "RS4")
# df_all.stch %>% filter(runname == "RS5")
# 
# df_all.stch %>% filter(runname == "RS1_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS4_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS5_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# 
# df_all.stch %>% filter(runname == "RS1_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS4_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS5_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# 
# 
# 
# 
# 
# 
# results_all %>% filter(runname == "RS1_cap", sim == 0, Tier %in% c("t7", "sumTiers")) %>% 
#   select(Tier, year, nactives, AL, NC, PR) %>% 
#   gather(key, value, -Tier, -year) %>% 
#   mutate(var = paste(key, Tier, sep=".")) %>% 
#   select(-key, -Tier) %>% 
#   spread(var, value) %>% 
#   mutate(AL.t7pct = 100*AL.t7 / AL.sumTiers,
#          nact.t7pct = 100 * nactives.t7 / nactives.sumTiers,
#          PR.t7pct = 100 * PR.t7/ PR.sumTiers) %>% 
#   filter(year %in% c(2019, 2029, 2045))
# 
# 
# df_all.stch %>% filter(runname == "RS1_cap.allTiers", year == 2045)
# 
# df_all.stch %>% filter(runname == "RS5_cap",Tier == "t7") %>% select(runname, Tier, year, starts_with("EEC"))
# df_all.stch %>% filter(runname == "RS5_cap.allTiers") %>% select(runname, Tier, year, starts_with("EEC"))
# # df_all.stch %>% filter(runname == "RS1_cap.allTiers") %>% select(runname, year, starts_with("EEC"))
# 
# results_all %>% filter(runname == "RS1", sim == 0, year %in% c(2016, 2030, 2045) ) %>% select(runname, Tier, year, AL, PR, SC, B) %>% mutate(AL_PR = AL/PR)
# 
# 
# 
# 
# df_all.stch %>% filter(runname == "RS1_FR075")
# 
# 
# 
# 
# 
# #**************************************************************************
# ## #Explore the impact of modeling Tier 7 separately on funded status  ####
# #**************************************************************************
# 
# # Runs involved: RS1 and RS1_cap
# # Variables involved: 
# # NC, SC, ADC, C, ERC, EEC
# 
# # Look for sims under RS1 with a extended period of zero ERC
# 
# results_all %>% filter(runname == "RS1", sim == 2, year <=2040) %>% 
#   select(runname, sim, year, NC, NC_PR, SC, ADC, B, C, EEC, ERC, AL, MA, AA, UAAL, EUAAL, LG, FR_MA, I.r)
# # %>% 
# mutate(ADC_unadj = NC + SC
# )
# 
# 
# results_all %>% filter(runname == "RS1_cap", sim == 1, Tier == "sumTiers", year <=2045) %>% 
#   select(runname, sim, year, NC, SC, ADC, B, C, EEC, ERC, AL, MA, AA, UAAL, EUAAL, LG, FR_MA, I.r, ERC_cap, NC_PR)
# # %>% 
# mutate(ADC_unadj = NC + SC,
#        ERC_cap_PR = 100 * ERC_cap / PR )
# 
# results_all %>% filter(runname == "RS1_cap", sim == 1, Tier == "t7", year <=2045) %>% 
#   select(runname, sim, year, NC, NC_PR, SC, ADC, B, C, EEC, ERC, AL, MA, AA, FR_MA, C_PR, ERC_cap, PR, ERC_PR, i.r, MA_PR, nactives) %>% 
#   # %>% 
#   mutate(ADC_unadj = NC + SC,
#          ERC_cap_PR = 100 * ERC_cap / PR,
#          AL_PR = AL/PR)
# 
# results_all %>% filter(runname == "RS1_cap", sim == 1, Tier == "xt7", year <=2045) %>% 
#   select(runname, sim, year, NC, NC_PR, SC, ADC, B, C, EEC, ERC, AL, MA, AA, UAAL, EUAAL, LG, FR_MA, C_PR, ERC_cap, PR, ERC_PR, MA_PR, nactives) %>% 
#   # %>% 
#   mutate(ADC_unadj = NC + SC,
#          ERC_cap_PR = 100 * ERC_cap / PR,
#          AL_PR = AL/PR)
# 
# 
# 
# results_all %>% filter(runname == "RS1_cap", sim == 2) %>% 
#   select(runname, sim, Tier, NC, SC, ADC,B, C, year, ERC, ERC_PR, AL, MA, AA) %>% 
#   mutate(ADC_unadj = NC + SC) %>% 
#   gather(variable, value, -runname, -sim, -Tier, -year) %>% 
#   mutate(variable = paste(variable, Tier, sep = ".")) %>% 
#   select(-Tier) %>% 
#   spread(variable, value) %>% 
#   select(runname, sim, year, ends_with("sumTiers"), ends_with(".t7"))
# 
# 
# 
# 
# results_all %>% filter(runname == "RS1", sim == 0) %>% 
#   select(runname, sim, year, NC, NC_PR, SC, ADC, C, EEC, ERC, ERC_PR, AL, MA, AA, FR_MA, LG, UAAL, Amort_basis, PR) %>% 
#   mutate(ADC_unadj = NC + SC,
#          SC_PR = 100 * SC/PR)
# 
# results_all %>% filter(runname == "RS1_cap", sim == 0) %>% 
#   select(runname, sim, Tier, NC, SC, ADC, C, year, ERC, ERC_PR, AL, MA, FR_MA) %>% 
#   mutate(ADC_unadj = NC + SC) %>% 
#   gather(variable, value, -runname, -sim, -Tier, -year) %>% 
#   mutate(variable = paste(variable, Tier, sep = ".")) %>% 
#   select(-Tier) %>% 
#   spread(variable, value) %>% 
#   select(runname, sim, year, ends_with("sumTiers"), ends_with(".t7"))
# 
# 
# 
# results_all %>% filter(runname == "RS1_cap", sim == 0, Tier %in% c("t7", "sumTiers")) %>% 
#   select(Tier, year, nactives, AL, NC, PR) %>% 
#   gather(key, value, -Tier, -year) %>% 
#   mutate(var = paste(key, Tier, sep=".")) %>% 
#   select(-key, -Tier) %>% 
#   spread(var, value) %>% 
#   mutate(AL.t7pct = 100*AL.t7 / AL.sumTiers,
#          nact.t7pct = 100 * nactives.t7 / nactives.sumTiers,
#          PR.t7pct = 100 * PR.t7/ PR.sumTiers) %>% 
#   filter(year %in% c(2019, 2029, 2045))
# 
# 
# #**************************************************************************
# # Explore issues in the report ####
# #**************************************************************************
# 
# ## Check supplemental costs in the deterministic run
# 
# load("SC_amort0.RData")  
# SC_amort0
# 
# colSums(SC_amort0)[1:30]
# 
# x <- results_all %>% filter(runname == "RS1", sim == 0) %>% 
#   select(runname, sim, year, SC, LG, ADC, C, FR_MA, PR, NC) %>% 
#   mutate(SC_original = colSums(SC_amort0)[1:30]) %>% 
#   mutate_each(funs(./1e6), -runname, -sim, -year, -FR_MA) %>%
#   mutate(SC_diffpct = 100 * ((SC - SC_original)/SC_original),
#          SC_diff    = SC - SC_original,
#          PR.growth = 100*PR/lag(PR) - 100) %>% 
#   select(runname, sim,year, SC, SC_original, SC_diff, SC_diffpct, everything())
# x
# 
# 
# results_all %>% filter(runname == "RS1", sim == -1) %>% 
#   select(runname, sim, year, SC, LG, ADC, C, FR_MA) %>% 
#   mutate(SC_original = colSums(SC_amort0)[1:30]) %>%
#   mutate_each(funs(./1e6), -runname, -sim, -year, -FR_MA) %>% 
#   mutate(SC_diff = 100 * SC/SC_original -100 )
# 
# 
# 
# G2A <- function(G, V){
#   ArithMean <- (1 + G)*(0.5 + 0.5*(1 + 4*V/(1 + G)^2 )^0.5 )^0.5 - 1
# }
# 
# (G2A(0.075, 0.12))
# 
# 
# 
# 
# # %>% 
# #   ggplot(aes(x = geoReturn_RS1, y = Diff_inc)) + geom_point()
# 
# 
# 
# 
# 
# library(pdata)
# 
# 
# names(ppd)
# 
# x <- ppd %>% select(PlanName, fy, ReqContRate_ER) %>% 
#   filter(fy == 2014) %>% 
#   arrange(ReqContRate_ER)
# 
# 
# #**************************************************************************
# # Lower discount rate: 7%     ####
# #**************************************************************************
# 
# df.DC7 <- results_all %>% filter(runname %in% c("RS1", "RS1_DC7","RS1_DC7a", "RS1_DC7b"), sim == 0, year == 2016) %>% 
#   select(runname, year, FR_MA, AL, MA,AA,UAAL, SC, NC,  C, ERC, EEC, PR)
# df.DC7
# 
# 628253575/565444323 # C
# 493605962/430796709 # ERC
# 
# 19943067948/18808249455 # 
# 
# 
# amort_cp(2282121710-1147303217, 0.07, 20, 0.04) #  73350356 year - 1 amort payment for change in DC
# 
# 
# (628253575 + 73350356) /565444323 # C
# (493605962 + 73350356) /430796709 # ERC
# 
# 
# write.xlsx2(df.DC7, file = paste0(Outputs_folder, "df.DC7.xlsx"))
# 
# 
# # DC = 7.25%, salary growth lowered by 0.25%
# amort_cp(1606067056-1147303217, 0.0725, 20, 0.04)
# 
# (441302839 + 30248344)/1397264823 # 33.75% , 2.9 pct points higher 
# 
# 
# # DC = 7%, salary growth lowered by 0.5%
# amort_cp(2085255919-1147303217, 0.07, 20, 0.04)
# 
# 
# (452417767 + 60625699)/1397264823 # 36.72% , 5.9 pct points higher 
# 
# # DC = 7%, salary growth lowered by 0.5%, cola lowered by 0.25%
# amort_cp(1704374607-1147303217, 0.07, 20, 0.04)
# 
# # DC
# (442379906 + 36006978)/1397264823 # 34.24% , 3.4 pct points higher 
# 
# 
# 
# 
# 
# x <- results_all %>% filter(runname == "RS1", sim > 0) %>% 
#   group_by(sim) %>% 
#   summarise(FR30 = FR_MA[year == 2045],
#             ERC30 = ERC_PR[year == 2045],
#             geoR = get_geoReturn(i.r)) %>% 
#   arrange(geoR)
# 
# 
# df_all.stch %>% filter(runname == "RS1_FR075")
# results_all %>% filter(runname == "RS1_FR075", sim == 0) %>% select(runname, sim, year, FR_MA, ERC_PR)
# 
# df_all.stch %>% filter(runname == "RS1")
# results_all %>% filter(runname == "RS1", sim == 0) %>% select(runname, sim, year, FR_MA, ERC_PR)
# 
# (1.075)^30
# (1.0753)^30
# 
# 
# # Examine disability benefits (for MISERS DC disability benefits) #####
# 
# results_all %>% filter(runname == "RS1", sim == 1) %>% select(year, NC.disb, B.disb.la, B.disb.ca, AL.act.disb, AL.disb.la, AL.disb.ca) %>% 
#   mutate(B.disb = B.disb.la + B.disb.ca,
#          AL.disb = AL.act.disb + AL.disb.la+ AL.disb.ca,
#          NC_AL.disb = NC.disb/AL.disb,
#          NC_B.disb  = NC.disb/B.disb, 
#          B_AL.disb =  B.disb/AL.disb,
#          gAL = 100*(AL.disb/lag(AL.disb) - 1 ),
#          gNC = 100*(NC.disb/lag(NC.disb) - 1 ),
#          gB  = 100*(B.disb/lag(B.disb) - 1 ))
# 
# 
# 
# # MA in AV: 10416577282 
# # AA in AV: 10731762400
# 
# 












