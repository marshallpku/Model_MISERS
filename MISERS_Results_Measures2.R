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
runs_RS.alt   <- c("RS1.closed.min5")


runs_RS.closed_labels <- c("Assumption Achieved; closed plan",
                           "15 years of low returns; closed plan",
                           "High volatility; closed plan",
                           "7.5% returns; closed plan",
                           "Lower assumed return; closed plan")

runs_RS.open_labels <- c("Assumption Achieved, open plan",
                         "15 years of low returns, open plan",
                         "High volatility, open plan",
                         "7.5% returns, open plan")

runs_RS.alt_labels <- c("Min amort = 5 years")


lab_s1 <- "Scenario 1 \nAssumption Achieved: \nClosed Plan"
lab_s2 <- "Scenario 2 \nAssumption Achieved: \nOpen Plan"
lab_s3 <- "Scenario 3 \n15 Years of Low Returns"
lab_s4 <- "Scenario 4 \nHigh Volatility"
lab_s5 <- "Scenario 5 \nCurrent Return Assumption"
lab_s6 <- "Scenario 6 \nLower Return Assumption"

lab.s1.1 <- "Assumption Achieved:\nClosed Plan \nCurrent policy"
lab.s7   <- "Assumption Achieved:\nClosed Plan \nAmortization period \nno less than 5 years"

runs_all <- c(runs_RS.closed, runs_RS.open, runs_RS.alt)
runs_all_labels <- c(runs_RS.closed_labels, runs_RS.open_labels, runs_RS.alt_labels)



df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim > 0, year %in% 2016:2046)




df_all.stch %<>%   
  select(runname, sim, year, AL, MA, ERC, EEC, PR, ERC_PR, ERC_GF, AL.closed.8pct) %>%
  group_by(runname, sim) %>% 
  mutate(AL.8pct   = ifelse(runname == "RS5.closed", AL.closed.8pct, AL),
         FR_MA     = 100 * MA / AL.8pct,
         FR40less  = cumany(FR_MA <= 40),
         FR100more = FR_MA >= 100,
         ERC_high  = cumany(ERC_GF >= 10), 
         ERC_hike  = cumany(na2zero(ERC_GF - lag(ERC_GF, 5) >= 5))
         ) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
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

df_all.stch %>% filter(runname == "RS1.closed.min5")

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
       x = NULL, y = "Percent of general fund") + 
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
fig.subtitle <- NULL

n1 <- "Notes:"
n2 <- "\n   Scenario 1: Expected compound return is 8%, standard deviation is 12%"
n3 <- "\n   Scenario 3: Expected compound return is 6.8%, standard deviation is 12%"
n4 <- "\n   Scenario 4: Expected compound return is 8%, standard deviation is 18%"
fig.caption <- paste0(n1, n2, n3, n4)

fig_RS.FR40less <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS2.closed", "RS3.closed")) %>% 
  select(runname, year, FR40less) %>% 
  mutate(runname = factor(runname, labels = c(lab_s1, lab_s3, lab_s4))) %>%  
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,30)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.green, RIG.red,RIG.blue),  name = "") + 
  scale_shape_manual(values = c(15, 17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       caption = fig.caption,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_RS.FR40less


# Risk of sharp increase in ERC/GF
fig.title <- "Probability of employer contribution rising more than 5% of general fund \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- NULL

n1 <- "Notes:"
n2 <- "\n   Scenario 1: Expected compound return is 8%, standard deviation is 12%"
n3 <- "\n   Scenario 3: Expected compound return is 6.8%, standard deviation is 12%"
n4 <- "\n   Scenario 4: Expected compound return is 8%, standard deviation is 18%"
fig.caption <- paste0(n1, n2, n3, n4)

fig_RS.ERChike <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS2.closed", "RS3.closed")) %>% 
  select(runname, year, ERC_hike) %>% 
  mutate(runname = factor(runname, labels = c(lab_s1, lab_s3, lab_s4))) %>%  
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.green, RIG.red, RIG.blue),  name = "") + 
  scale_shape_manual(values = c(15, 17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       caption = fig.caption,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
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
  mutate(runname = factor(runname, labels = c("Scenario 5\nCurrent Return Assumption\nAssumed Return = 8%",
                                              "Scenario 6\nLower Return Assumption\nAssumed Return = 7.5%"))) %>% 
  ggplot(aes(x = year, y = ERC_GF, color = runname, shape = runname)) + 
  geom_line() + 
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,6)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 0.5)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of general fund") + 
  theme(axis.text.x = element_text(size = 8)) +
  theme_bw() +
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
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
fig.subtitle <- "Stochastic returns; expected compound return = 7.5%"
fig_DC.FR40less <- df_all.stch %>% filter(runname %in% c("RS4.closed", "RS5.closed")) %>% 
  select(runname, year, FR40less) %>% 
  mutate(runname = factor(runname, labels = c("Scenario 5\nCurrent Return Assumption\nAssumed Return = 8%",
                                              "Scenario 6\nLower Return Assumption\nAssumed Return = 7.5%"))) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,10)) + 
  scale_y_continuous(breaks = seq(0,200, 1)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.red,RIG.blue),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_DC.FR40less


# Risk of sharp increase in ERC/GF
fig.title <- "Probability of employer contribution rising more than 5% of general fund \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Stochastic returns; expected compound return = 7.5%"
fig_DC.ERChike <- df_all.stch %>% filter(runname %in% c("RS4.closed", "RS5.closed")) %>% 
  select(runname, year, ERC_hike) %>% 
  mutate(runname = factor(runname, labels = c("Scenario 5\nCurrent Return Assumption\nAssumed Return = 8%",
                                              "Scenario 6\nLower Return Assumption\nAssumed Return = 7.5%"))) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,40)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.red,RIG.blue),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
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
##                     5-year min amortization period   ####
#*************************************************************************



# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8%"
fig_min5.FRdist <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.closed.min5")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>% 
  #             select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(runname = factor(runname, labels = c(lab.s1.1, lab.s7))) %>% 
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

fig_min5.FRdist



# Distribution of ERC as % general fund
fig.title <- "Distribution of employer contribution as a percentage of general fund across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 8%"
fig_min5.ERCdist <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.closed.min5")) %>% 
  # left_join(results_all  %>% 
  #             filter(runname  %in% c("RS1.closed", "RS1.open"), sim == 0) %>% 
  #             select(runname, year, ERC_det = ERC_GF)) %>% 
  select(runname, year, ERC_GF.q25, ERC_GF.q50, ERC_GF.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(runname = factor(runname, labels = c(lab.s1.1, lab.s7))) %>%  
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
       x = NULL, y = "Percent of general fund") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_min5.ERCdist



# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_min5.FR40less <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.closed.min5")) %>% 
  mutate(runname = factor(runname, labels = c(lab.s1.1, lab.s7))) %>%  
  select(runname, year, FR40less) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,10)) + 
  scale_y_continuous(breaks = seq(0,200, 2)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black",RIG.red),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_min5.FR40less


# Risk of sharp increase in ERC/GF
fig.title <- "Probability of employer contribution rising more than 5% of general fund \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_min5.ERChike <- df_all.stch %>% filter(runname %in% c("RS1.closed", "RS1.closed.min5")) %>% 
  mutate(runname = factor(runname, labels = c(lab.s1.1, lab.s7))) %>%  
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
fig_min5.ERChike


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


ggsave(file = paste0(Outputs_folder, "min5.FR40less.png"), fig_min5.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "min5.ERChike.png"),  fig_min5.ERChike, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "min5.FRdist.png"),  fig_min5.FRdist,  height = g.height.2col, width = g.width.2col)



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



ggsave(file = paste0(Outputs_folder, "fig1_AL.pdf"),  fig_AL,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig2_stchDet.FR40less.pdf"), fig_stchDet.FR40less, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig3_stchDet.ERC.USDdist.pdf"), fig_stchDet.ERC.USDdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig4_stchDet.ERCdist.pdf"), fig_stchDet.ERCdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig5_stchDet.ERChike.pdf"),  fig_stchDet.ERChike, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig6_RS.FR40less.pdf"), fig_RS.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig7_RS.ERChike.pdf"),  fig_RS.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig8_DC.DetERC.pdf"), fig_DC.DetERC,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig9_DC.FR40less.pdf"), fig_DC.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig10_DC.ERChike.pdf"),  fig_DC.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig11_projGenFun.pdf"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "fig1_AL.png"),  fig_AL,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig2_stchDet.FR40less.png"), fig_stchDet.FR40less, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig3_stchDet.ERC.USDdist.png"), fig_stchDet.ERC.USDdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig4_stchDet.ERCdist.png"), fig_stchDet.ERCdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig5_stchDet.ERChike.png"),  fig_stchDet.ERChike, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig6_RS.FR40less.png"), fig_RS.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig7_RS.ERChike.png"),  fig_RS.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig8_DC.DetERC.png"), fig_DC.DetERC,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig9_DC.FR40less.png"), fig_DC.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig10_DC.ERChike.png"),  fig_DC.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig11_projGenFun.png"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "fig12_min5.FR40less.pdf"), fig_min5.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig13_min5.ERChike.pdf"),  fig_min5.ERChike, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig14_min5.FRdist.pdf"),  fig_min5.FRdist,  height = g.height.2col, width = g.width.2col)














