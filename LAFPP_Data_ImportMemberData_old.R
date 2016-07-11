# This script import demographic data of LAFPP

# Data source: Data_inputs/LAFPP_MemberData.xlsx


# Output list
  # init_actives_all
  # init_retirees_all     
  # init_beneficiaries_all
  # init_disb_all         
  # init_terms_all

  # prop.occupation
  # pct.male, pct.female

# Output file
  # Data_inputs/LAFPP_MemberData.RData


load("Data_inputs/dist_init.nonActives.RData")              

simDist_retirees <- TRUE
simDist_disb     <- TRUE
simDist_benenficiaries <- TRUE


#****************************************************************************************************
#                    Global constants ####
#****************************************************************************************************

file_memberData <- "Data_inputs/LAFPP_MemberData.xlsx"

#****************************************************************************************************
#                                       Tools                                                   #####                  
#****************************************************************************************************

# Utility functions
getcell <- function(file, sheet, cell) {
  require(XLConnect)
  value <- readWorksheetFromFile(file, sheet=sheet, header=FALSE, region=cell, colTypes="character")
  return(as.character(value))
}


xlrange <- function(file, sheet, cell1, cell2) {
  startcell <- getcell(file, sheet, cell1)
  endcell   <- getcell(file, sheet, cell2)
  range     <- paste0(startcell, ":", endcell)
  return(range)
}


get_bound <- function(range, bound = c("l","u")){
  # range must be in the form of "XX-YY", "X" is a single digit, and "XX" <= "YY".
  switch(bound,
         l =  str_extract(range, "\\d+-") %>% gsub("\\D+", "",.) %>% as.integer,
         u =  str_extract(range, "-\\d+") %>% gsub("\\D+", "",.) %>% as.integer)
}


# Load actives
import_actives <- function(file, sheet, planname){
#   

# file <- file_memberData
# sheet <- "Actives_t3"
# planname <- "Actives_t3"
# 
# getcell("Data_inputs/LAFPP_MemberData1.xlsx", "sheet1", "B2")
# read_excel(file, sheet = "TOC")



  range <- xlrange(file, sheet, "B2", "B3")
  
  df <- readWorksheetFromFile(file, sheet=sheet, header=TRUE, region=range, colTypes="character")
  
  yoscuts <- df %>% filter(type == "yosgrp") %>%
    select(starts_with("X")) %>%
    gather(yos.cell, yosgrp) %>%
    mutate(yos.cell=as.integer(gsub("[^0-9]", "", yos.cell)),
           yoslb = get_bound(yosgrp, "l"),
           yosub = get_bound(yosgrp, "u")) %>% 
    select(-yosgrp)
  yoscuts
  
  agecuts <- df %>% filter(type != "yosgrp") %>% 
             filter(type == unique(type)[1]) %>% 
             select(age.cell, agegrp) %>% 
             mutate(agelb = get_bound(agegrp, "l"),
                    ageub = get_bound(agegrp, "u")) %>% 
             select(-agegrp)
  agecuts
  
  
  df %<>% filter(type != "yosgrp") %>% 
         select(type, age.cell, starts_with("X")) %>%
         gather(yos.cell, value, -type, -age.cell) %>%
         mutate(yos.cell = as.integer(gsub("[^0-9]", "", yos.cell)),
                age.cell = as.integer(age.cell),
                value    = as.numeric(value),
                age = age.cell,
                yos = yos.cell,
                planname = planname) %>%
         filter(!is.na(value)) %>% 
         spread(type, value) %>%
         arrange(age.cell, yos.cell)
  
  
  lactives <- list()
  lactives$agecuts <- agecuts
  lactives$yoscuts <- yoscuts
  lactives$actives.yos <- df
  
  return(lactives)
}


# Load retirees, also can be used to load initial beneficiaries and initial disabled
import_retirees_byAge <- function(file, sheet, planname){

# file <- paste0(path, fileName)
# sheet <- "Beneficiaries"
# planname <- Tier_select


range <- xlrange(file, sheet, "B2", "B3")
benperiod <- getcell(file, sheet, "B4")
benmult <- ifelse(benperiod=="month", 12, 1)
name_N  <- getcell(file, sheet, "B5")
name_V  <- getcell(file, sheet, "B6")


df <- readWorksheetFromFile(file, sheet=sheet, header=TRUE, region=range, colTypes="character")

agecuts <- df %>%  
  select(age.cell, agegrp) %>% 
  mutate(agelb = get_bound(agegrp, "l"),
         ageub = get_bound(agegrp, "u")) %>% 
  select(-agegrp)

df %<>%  
  select(-agegrp) %>%
  colwise(as.numeric)() %>% 
  mutate(age.cell = as.integer(age.cell),
         age = age.cell,
         V = V * benmult,
         planname = planname)


list_out <- list()
list_out$data <- df
list_out$agecuts <- agecuts
list_out$varNames <- c(name_N = name_N, name_V = name_V)

return(list_out)
}


# Interpolation of actives
fillin.actives.spreadyos.splineage <- function(lactives) {
  # salary:
  #   first spread uniformly within age.cell-yos.cell group (same salary for all)
  #   then for every yos, estimate salary for each age using a spline - adjust endpoints first for plausibility
  #   finally, adjust resulting salary within each age.cell-yos.cell proportionately to hit total payroll values from grouped data
  #   then add ea to the data
  # nactives: spread uniformly within age.cell-yos.cell group (same nactives for all), then add ea to the data
  
  lactives
  
  adf <- lactives$actives.yos
  agecuts <- lactives$agecuts
  yoscuts <- lactives$yoscuts
  #eacuts <- lactives$eacuts
  minage <- min(agecuts$agelb)
  maxage <- max(agecuts$ageub)
  minyos <- min(yoscuts$yoslb)
  maxyos <- max(yoscuts$yosub)
  
  planname <- paste0(adf$planname[1])
  
  # adf %>% select(age, ea, salary) %>% spread(ea, salary)
  # adf %>% select(age, ea, nactives) %>% spread(ea, nactives)
  
  # create a master grouped data frame
  adf.g <- adf %>% select(-planname, -age, -yos, nactives.cell=nactives, salary.cell=salary) %>%
    mutate(pay.cell=nactives.cell * salary.cell) %>%
    mutate(ageidx = findInterval(age.cell, agecuts$agelb),
           age.lb = agecuts$agelb[ageidx],
           age.ub = agecuts$ageub[ageidx],
           yosidx = findInterval(yos.cell, yoscuts$yoslb),
           yos.lb = yoscuts$yoslb[yosidx],
           yos.ub = yoscuts$yosub[yosidx]) %>%
    select(age.cell, yos.cell, age.lb, age.ub, yos.lb, yos.ub, nactives.cell, salary.cell, pay.cell)
  
  # expand the grouped data frame to all allowable age-yos combinations ####
  xpnd <- function(df) {
    # expand to all age-yos combinations but only keep those where ea>=15 or, if there are no such records,
    # keep the recrods with max ea
    df2 <- expand.grid(age=df$age.lb:df$age.ub, yos=df$yos.lb:df$yos.ub) %>%
      mutate(ea=age - yos) %>%
      filter((ea >= 20) | (ea<20 & ea==max(ea))) %>%
      select(-ea)
    return(df2)
  }
  
  adf.x <- adf.g %>% rowwise() %>%
    do(cbind(., xpnd(.))) %>%
    ungroup %>%  # get rid of rowwise
    group_by(age.cell, yos.cell) %>%
    mutate(n.cell=n()) %>%
    select(age, yos, everything()) %>%
    arrange(age, yos)
  
  
  # work with the expanded data ####
  
  # we have to anchor the endpoints with reasonable values BEFORE computing the spline
  adjustends <- function(age, salary) {
    # the basic idea is that if an endpoint is NA, insert a plausible value
    
    # simple rule: if spline first or last value falls within +/ 50% of the nearest nonNA value, use spline estimate
    # otherwise use the capped value
    firstnonna <- salary[which.min(is.na(salary))]
    lastnonna <- rev(salary)[which.min(is.na(rev(salary)))]
    bound <- .5
    firstrange <- c(firstnonna * bound, firstnonna * (1 + bound))
    lastrange <- c(lastnonna * bound, lastnonna * (1 + bound))
    cap <- function(sal, range) {
      cappedval <- max(sal, range[1])
      cappedval <- min(cappedval, range[2])
      return(cappedval)
    }
    
    salary.est <- spline(age, salary, xout=age)$y # what does spline think naively?
    salary.adjusted <- salary
    
    if(is.na(salary[1])) salary.adjusted[1] <- cap(salary.est[1], firstrange)
    ilast <- length(salary)
    if(is.na(salary[ilast])) salary.adjusted[ilast] <- cap(salary.est[ilast], firstrange)
    
    return(salary.adjusted)
  }
  
  # test out adjustends
  # fs <- function(age, sal) return(spline(age, sal, xout=age)$y) # spline doesn't seem to work with dplyr if not in function
  # # various salaries to try out
  # salary <- seq(20, 50, length.out = 10)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 80)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, NA)
  # salary <- c(NA, 10, 30, NA, 40, NA, 50, 80, NA, NA)
  # age <- 21:30
  # d <- data_frame(age, salary, saladj=adjustends(age, salary)) %>%
  #   mutate(sal.spline=fs(age, salary),
  #          saladj.spline=fs(age, saladj))
  # d
  # qplot(age, value, data=gather(d, variable, value, -age), colour=variable, geom=c("point", "line")) + scale_x_continuous(breaks=0:100) + geom_hline(y=0)
  
  
  spline.y2 <- function(age, salary, safesalary) {
    # safesalary is what we use if salary has no data
    if(all(is.na(salary))) {
      print("AllNA")
      salary <- safesalary
    }
    salary.adjusted <- adjustends(age, salary)
    
    sp.out <- spline(age, salary.adjusted, xout=age)
    salout <- sp.out$y
    return(salout)
  }
  
  adf.x3 <- adf.x %>% ungroup %>% # MUST be ungrouped or ifelse won't work if there is only one rec in a group
    mutate(nactives=nactives.cell / n.cell, # always spread nactives uniformly
           salary.group=ifelse(age==age.cell & yos==yos.cell, salary.cell, NA),
           salary.group=ifelse(salary.group==0, NA, salary.group),
           salary.agecell=ifelse(age==age.cell, salary.cell, NA)) %>% # Yimeng's first step
    group_by(yos) %>%
    arrange(age) %>%
    mutate(salary.spline.adjep=spline.y2(age, salary.agecell, salary.cell)) %>% # Yimeng's 2nd step with endpoint adjustment
    group_by(age.cell, yos.cell) %>%
    mutate(planname=planname,
           pay.unadj=sum(salary.spline.adjep * nactives),
           adjust=pay.cell / pay.unadj,
           salary.final=salary.spline.adjep * adjust,
           pay.adj=sum(salary.final * nactives),
           ea=age - yos
           #ea.cell=eacuts$stub[findInterval(ea, eacuts$lb)]
    )
  
  return(adf.x3)
}


# Interpolation of retirees
fillin.retirees <- function(list_data) {
  
  rdf <- select(list_data$data, planname, age, N, V) # keep only the vars we want
  agecuts <- list_data$agecuts
  
  planname <- paste0(rdf$planname[1], ".fillin")
  name_N <- list_data$varNames["name_N"]
  name_V <- list_data$varNames["name_V"]
  
  # add group ranges to the retirees data frame
  combo <- rdf %>%
    mutate(totben=N * V) %>%
    mutate(ageidx=findInterval(age, agecuts$agelb),
           age.lb=agecuts$agelb[ageidx],
           age.ub=agecuts$ageub[ageidx]) %>%
    arrange(age)
  
  # get avg benefits by age, via spline
  avgben <- splong(select(combo, age, V), "age", min(combo$age.lb):max(combo$age.ub))
  # force benefit to be non-negative DJB added 10/30/2015
  avgben <- avgben %>% mutate(V=ifelse(V<0, 0, V))
  
  guessdf <- data.frame(age=min(combo$age.lb):max(combo$age.ub)) %>%
    mutate(ageidx=findInterval(age, agecuts$agelb),
           age.cell=combo$age[match(ageidx, combo$ageidx)],
           N.cell=combo$N[match(ageidx, combo$ageidx)],
           V.cell=combo$V[match(ageidx, combo$ageidx)]) %>%
    group_by(age.cell) %>%
    mutate(n.cell=n(),
           N=N.cell / n.cell, # spread nretirees evenly
           adjV=avgben$V[match(age, avgben$age)], # get the spline-based avg benefit
           adjtotben=N * adjV)
  
  # refine the guess by adjusting ensure that we hit the right total benefits in each group
  guessdf2 <- guessdf %>% group_by(age.cell) %>%
    mutate(adjust=mean(N.cell * V.cell) / sum(adjtotben),
           V=adjV*adjust,
           totben=N * V)
  
  rdf.fillin <- guessdf2 %>% mutate(planname=planname) %>%
    select(planname, age.cell, age, N, V) %>%
    ungroup
  #plyr::rename(c("N" = list_data$varNames["name_N"])))
  
  names(rdf.fillin)[names(rdf.fillin) == "N"] <- name_N
  names(rdf.fillin)[names(rdf.fillin) == "V"] <- name_V
  
  return(rdf.fillin)
}



#*************************************************************************************************************
#                     Importing summary measures of initial non-actives                                  #####                  
#*************************************************************************************************************
get_init.nonActives.info <- function(file, sheet, cellStart = "B2", cellEnd = "B3"){
  
  # file <- file_memberData
  # sheet <-   "Other_t2"
  # planname <- "t2"
  # cellStart <- "B2"
  # cellEnd <- "B3"
  
  range <- xlrange(file, sheet, cellStart, cellEnd)
  
  df <- readWorksheetFromFile(file, sheet = sheet, header=TRUE, region= range, colTypes="character")
}

init.nonActives.info <- get_init.nonActives.info(file_memberData, "nonActives_allTiers") %>%
  mutate_each(funs(as.numeric),  -tier)

init.nonActives.info
glimpse(init.nonActives.info)

init.nonActives.info.singleTier <-
  init.nonActives.info %>%
  summarize(retirees.n   = sum(retirees.n.tot),
            retirees.avg.age = sum(retirees.avg.age * retirees.n.tot)/sum(retirees.n.tot),
            retirees.ben.mon = sum(retirees.ben.mon * retirees.n.tot)/sum(retirees.n.tot),
            
            disb.n   = sum(disb.n.tot),
            disb.avg.age = sum(disb.avg.age * disb.n.tot)/sum(disb.n.tot),
            disb.ben.mon = sum(disb.ben.mon * disb.n.tot)/sum(disb.n.tot),
            
            beneficiaries.n   = sum(beneficiaries.n.tot),
            beneficiaries.avg.age = sum(beneficiaries.avg.age * beneficiaries.n.tot)/sum(beneficiaries.n.tot),
            beneficiaries.ben.mon = sum(beneficiaries.ben.mon * beneficiaries.n.tot)/sum(beneficiaries.n.tot),
            
            terms.n   = sum(terms.n.tot),
            terms.avg.age = sum(terms.avg.age * terms.n.tot)/sum(terms.n.tot),
            terms.ben50.mon = sum(terms.ben50.mon * terms.n.tot)/sum(terms.n.tot))




#*************************************************************************************************************
#                                       Importing Data for initial actives                               #####                  
#*************************************************************************************************************

fn_actives <- function(sheet, file_ = file_memberData){
  
  # path_ = path
  # fileName_ = fileName
  # Tier_select = "t76"
  
  lactives <- import_actives(file_, sheet, sheet)
  lactives$actives.yos %<>% filter(age - yos >= 20) # Will drop a small number of members, need to figure out how to add memebers with ea<20 back   
  
  actives_grouped <- lactives$actives.yos %>% select(planname, age, yos, nactives, salary) %>% 
    mutate(planname = paste0(planname, "_grouped"),
           ea = age - yos)
  
  actives_fillin  <- fillin.actives.spreadyos.splineage(lactives) %>% ungroup %>% 
    select(planname, age, yos, ea,
           #age.cell, yos.cell, 
           nactives, salary=salary.final) %>% 
    mutate(planname = paste0(planname, "_fillin"))
  
  actives_out <- bind_rows(actives_fillin, actives_grouped)
} 


# import_actives(file_memberData, "Actives_t6_HPP", "Actives_t2")
# 
# x <- fn_actives("Actives_t2") %>% print
# fn_actives("Actives_t1") %>% print
# fn_actives("Actives_t2") %>% print
# fn_actives("Actives_t3") %>% print
# fn_actives("Actives_t4") %>% print
# fn_actives("Actives_t5_noHPP") %>% print
# fn_actives("Actives_t5_HPP") %>% print
# fn_actives("Actives_t6_noHPP") %>% print
# fn_actives("Actives_t6_HPP") %>% print


# T1 has no actives, create a all-0 data frame for it.  
df_t1 <- fn_actives("Actives_t2") %>% 
         mutate(planname = sub("t2", "t1", planname),
         nactives = 0,
         salary = 0)

init_actives_all <- bind_rows(df_t1,
                              fn_actives("Actives_t2"),
                              fn_actives("Actives_t3"),
                              fn_actives("Actives_t4"),
                              fn_actives("Actives_t5_noHPP"), 
                              fn_actives("Actives_t5_HPP"), 
                              fn_actives("Actives_t6_noHPP"), 
                              fn_actives("Actives_t6_HPP") )



#*************************************************************************************************************
#                             Importing initial retirees (Temporary)                                     #####                  
#*************************************************************************************************************

# There is no breakdown of initial retirees by age in the AV. For now, the total number of retirees are evenly spread
# over age 41 - 80.

if(!simDist_retirees){
get_init.ret.temp <- function(file, sheet, planname, cellStart = "B2", cellEnd = "B3"){

# file <- file_memberData
# sheet <-   "Other_t2"
# planname <- "t2"
# cellStart <- "B2"
# cellEnd <- "B3"

range <- xlrange(file, sheet, cellStart, cellEnd)

df <- readWorksheetFromFile(file, sheet = sheet, header=TRUE, region= range, colTypes="character") %>%
      mutate(value = suppressWarnings(as.numeric(value)))
df
init_retirees <- data.frame(age = 41:80) %>%
  mutate(nretirees = df[df$variable == "retirees.n.tot", "value"]/n(),
         benefit   = 12 * df[df$variable == "retirees.ben.mon", "value"],
         planname = planname) %>%
  mutate_each(funs(na2zero), -planname, -age) %>%
  select(planname, everything())
}


init_retirees_all <- bind_rows(
  get_init.ret.temp(file_memberData, "Other_t1", "Retirees_t1"),
  get_init.ret.temp(file_memberData, "Other_t2", "Retirees_t2"),
  get_init.ret.temp(file_memberData, "Other_t3", "Retirees_t3"),
  get_init.ret.temp(file_memberData, "Other_t4", "Retirees_t4"),
  get_init.ret.temp(file_memberData, "Other_t5_noHPP", "Retirees_t5_noHPP"),
  get_init.ret.temp(file_memberData, "Other_t5_HPP",   "Retirees_t5_HPP"),
  get_init.ret.temp(file_memberData, "Other_t6_noHPP", "Retirees_t6_noHPP"),
  get_init.ret.temp(file_memberData, "Other_t6_HPP",   "Retirees_t6_HPP")
)
  # init_retirees_all


} else {


# Assume all initial retirees are in Tier 5
init_retirees_t5 <-  expand.grid(planname = "Retirees_t5", age = 41:100) %>%
  left_join(dist_init.retirees) %>%
  mutate(nretirees = dist.num.la * init.nonActives.info.singleTier[1,"retirees.n"])


ben.factor.retirees <- as.numeric(init.nonActives.info.singleTier[1,"retirees.ben.mon"] * 12) /
  as.numeric(init_retirees_t5 %>% summarise(ben.tot = sum(nretirees * dist.ben.la)/sum(nretirees)))


init_retirees_t5 %<>% mutate(benefit = dist.ben.la * ben.factor.retirees) %>%
  select(planname, age, nretirees, benefit)


# double check
init.nonActives.info.singleTier
init_retirees_t5 %>% summarize(avg.ben = sum(benefit * nretirees)/sum(nretirees)/12,
                               avg.age = sum(age * nretirees)/sum(nretirees),
                               n.tot = sum(nretirees))


init_retirees_all <-  expand.grid(age = 41:100, planname = paste0("Retirees_", paste0("t", 1:6))) %>%
  left_join(init_retirees_t5) %>%
  mutate_each(funs(na2zero), -age, -planname) %>%
  mutate(planname = paste0(planname, "_fillin"))
}


#*************************************************************************************************************
#                             Importing initial beneficiaries (Temporary)                                #####                  
#*************************************************************************************************************

# There is no breakdown of initial retirees by age in the AV. For now, the total number of retirees are evenly spread 
# over age 41 - 80. 

if(!simDist_benenficiaries){

get_init.beneficiaries.temp <- function(file, sheet, planname, cellStart = "B2", cellEnd = "B3"){
  
  # file <- file_memberData
  # sheet <-   "Other_t2"
  # planname <- "t2"
  # cellStart <- "B2"
  # cellEnd <- "B3"
  
  range <- xlrange(file, sheet, cellStart, cellEnd)
  
  df <- readWorksheetFromFile(file, sheet = sheet, header=TRUE, region= range, colTypes="character") %>% 
    mutate(value = suppressWarnings(as.numeric(value)))
  df
  init_retirees <- data.frame(age = 41:80) %>% 
    mutate(nbeneficiaries = df[df$variable == "beneficiaries.n.tot", "value"]/n(),
           benefit   = 12 * df[df$variable == "beneficiaries.ben.mon", "value"],
           planname = planname) %>%
    mutate_each(funs(na2zero), -planname, -age) %>% 
    select(planname, everything())
  
}

init_beneficiaries_all <- bind_rows(
  get_init.beneficiaries.temp(file_memberData, "Other_t1", "Beneficiaries_t1"),
  get_init.beneficiaries.temp(file_memberData, "Other_t2", "Beneficiaries_t2"),
  get_init.beneficiaries.temp(file_memberData, "Other_t3", "Beneficiaries_t3"),
  get_init.beneficiaries.temp(file_memberData, "Other_t4", "Beneficiaries_t4"),
  get_init.beneficiaries.temp(file_memberData, "Other_t5_noHPP", "Beneficiaries_t5_noHPP"),
  get_init.beneficiaries.temp(file_memberData, "Other_t5_HPP",   "Beneficiaries_t5_HPP"),
  get_init.beneficiaries.temp(file_memberData, "Other_t6_noHPP", "Beneficiaries_t6_noHPP"),
  get_init.beneficiaries.temp(file_memberData, "Other_t6_HPP",   "Beneficiaries_t6_HPP")
)

 init_beneficiaries_all

} else {
  
  # Assume all initial retirees are in Tier 5
  # For now, use the simulated distribution for service retirement life annuitants. 
  
  init_beneficiaries_t5 <-  expand.grid(planname = "Beneficiaries_t5", age = 41:100) %>%
    left_join(dist_init.retirees) %>%
    mutate(nbeneficiaries = dist.num.la * init.nonActives.info.singleTier[1,"beneficiaries.n"])
  
  
  ben.factor.beneficiaries <- as.numeric(init.nonActives.info.singleTier[1,"beneficiaries.ben.mon"] * 12) /
    as.numeric(init_beneficiaries_t5 %>% summarise(ben.tot = sum(nbeneficiaries * dist.ben.la)/sum(nbeneficiaries)))
  
  
  init_beneficiaries_t5 %<>% mutate(benefit = dist.ben.la * ben.factor.beneficiaries) %>%
    select(planname, age, nbeneficiaries, benefit)
  
  
  # double check
  init.nonActives.info.singleTier
  init_beneficiaries_t5 %>% summarize(avg.ben = sum(benefit * nbeneficiaries)/sum(nbeneficiaries)/12,
                                 avg.age = sum(age * nbeneficiaries)/sum(nbeneficiaries),
                                 n.tot = sum(nbeneficiaries))
  
  
  init_beneficiaries_all <-  expand.grid(age = 41:100, planname = paste0("Beneficiaries_", paste0("t", 1:6))) %>%
    left_join(init_beneficiaries_t5) %>%
    mutate_each(funs(na2zero), -age, -planname) %>%
    mutate(planname = paste0(planname, "_fillin"))
}



#*************************************************************************************************************
#                             Importing initial disabled (Temporary)                                #####                  
#*************************************************************************************************************

if(!simDist_disb){

get_init.disb.temp <- function(file, sheet, planname, age_spread, cellStart = "B2", cellEnd = "B3"){
  
  # file <- file_memberData
  # sheet <-   "Other_t2"
  # planname <- "t2"
  # cellStart <- "B2"
  # cellEnd <- "B3"
  
  range <- xlrange(file, sheet, cellStart, cellEnd)
  
  df <- readWorksheetFromFile(file, sheet = sheet, header=TRUE, region= range, colTypes="character") %>% 
    mutate(value = suppressWarnings(as.numeric(value)))
  df
  init_retirees <- data.frame(age = age_spread) %>% 
    mutate(ndisb = df[df$variable == "disb.n.tot", "value"]/n(),
           benefit   = 12 * df[df$variable == "disb.ben.mon", "value"],
           planname = planname) %>%
    mutate_each(funs(na2zero), -planname, -age) %>% 
    select(planname, everything())
  
}

init_disb_all <- bind_rows(
  get_init.disb.temp(file_memberData, "Other_t1", "Disb_t1", 80:85),
  get_init.disb.temp(file_memberData, "Other_t2", "Disb_t2", 63:83),
  get_init.disb.temp(file_memberData, "Other_t3", "Disb_t3", 46:66),
  get_init.disb.temp(file_memberData, "Other_t4", "Disb_t4", 43:64),
  get_init.disb.temp(file_memberData, "Other_t5_noHPP", "Disb_t5_noHPP", 41:61),
  get_init.disb.temp(file_memberData, "Other_t5_HPP",   "Disb_t5_HPP", 41:61),
  get_init.disb.temp(file_memberData, "Other_t6_noHPP", "Disb_t6_noHPP", 41:61),
  get_init.disb.temp(file_memberData, "Other_t6_HPP",   "Disb_t6_HPP", 41:61)
)

init_disb_all

} else {

# Assume all initial retirees are in Tier 5
init_disb_t5 <-  expand.grid(planname = "Disb_t5", age = 21:100) %>%
  left_join(dist_init.disb) %>%
  mutate(ndisb = dist.num.disb.la * init.nonActives.info.singleTier[1,"disb.n"])


ben.factor.disb <- as.numeric(init.nonActives.info.singleTier[1,"disb.ben.mon"] * 12) /
  as.numeric(init_disb_t5 %>% summarise(ben.tot = sum(ndisb * dist.ben.disb.la)/sum(ndisb)))


init_disb_t5 %<>% mutate(benefit = dist.ben.disb.la * ben.factor.disb) %>%
  select(planname, age, ndisb, benefit)


# double check
init.nonActives.info.singleTier
init_disb_t5 %>% summarize(avg.ben = sum(benefit * ndisb)/sum(ndisb)/12,
                               avg.age = sum(age * ndisb)/sum(ndisb),
                               n.tot = sum(ndisb))


init_disb_all <-  expand.grid(age = 21:100, planname = paste0("Disb_", paste0("t", 1:6))) %>%
  left_join(init_disb_t5) %>%
  mutate_each(funs(na2zero), -age, -planname) %>%
  mutate(planname = paste0(planname, "_fillin"))

}



#*************************************************************************************************************
#                             Importing initial vested terms (Temporary)                                #####                  
#*************************************************************************************************************

get_init.terms.temp <- function(file, sheet, planname, cellStart = "B2", cellEnd = "B3"){
  
  # file <- file_memberData
  # sheet <-   "Other_t2"
  # planname <- "t2"
  # cellStart <- "B2"
  # cellEnd <- "B3"
  
  range <- xlrange(file, sheet, cellStart, cellEnd)
  
  df <- readWorksheetFromFile(file, sheet = sheet, header=TRUE, region= range, colTypes="character") %>% 
    mutate(value = suppressWarnings(as.numeric(value)))
  df
  init_retirees <- data.frame(age = 30:49) %>% 
    mutate(nterms = df[df$variable == "terms.n.tot", "value"]/n(),
           benefit.50   = 12 * df[df$variable == "terms.ben50.mon", "value"],
           planname = planname) %>%
    mutate_each(funs(na2zero), -planname, -age) %>% 
    select(planname, everything())
  
}

init_terms_all <- bind_rows(
  get_init.terms.temp(file_memberData, "Other_t1", "Terms_t1"),
  get_init.terms.temp(file_memberData, "Other_t2", "Terms_t2"),
  get_init.terms.temp(file_memberData, "Other_t3", "Terms_t3"),
  get_init.terms.temp(file_memberData, "Other_t4", "Terms_t4"),
  get_init.terms.temp(file_memberData, "Other_t5_noHPP", "Terms_t5_noHPP"),
  get_init.terms.temp(file_memberData, "Other_t5_HPP",   "Terms_t5_HPP"),
  get_init.terms.temp(file_memberData, "Other_t6_noHPP", "Terms_t6_noHPP"),
  get_init.terms.temp(file_memberData, "Other_t6_HPP",   "Terms_t6_HPP")
)

init_terms_all




#*************************************************************************************************************
#                            Aggregate HPP and non-HPP members members(Temporary)                        #####                  
#*************************************************************************************************************

integrate_HPP <- function(df){

df_nonHPP <- df %>% filter(!grepl("HPP", planname)) 
df_HPP <- df %>% filter(grepl("HPP", planname)) 
df_HPP

suffix <- df_HPP$planname[1] %>% str_extract("^[A-z]*_")
var.n  <- names(df_HPP)[3] 
var.v  <- names(df_HPP)[4]


df_HPP %<>% rename_("nmember" = var.n, "value" = var.v) %>% 
           mutate(tier = str_extract(planname, "t\\d"),
                  HPP  = str_extract(planname, "[a-z]?[a-z]?HPP")) %>% 
           gather(variable, value, -planname, -age, -tier, -HPP) %>% 
           mutate(varname.new = paste(variable, HPP, sep = "_")) %>%
           select(-planname, -variable, -HPP) %>% 
           spread(varname.new, value) %>% 
           mutate(nmember = nmember_HPP + nmember_noHPP,
                  value = ifelse(nmember == 0, 0, (nmember_HPP * value_HPP + nmember_noHPP * value_noHPP)/nmember),
                  planname = paste0(suffix, tier)) %>% 
           select(planname, age, nmember, value) %>%
           plyr::rename(c("nmember" = var.n, "value" = var.v)) %>% 
           arrange(planname, age)

df_out <- bind_rows(df_nonHPP, df_HPP) %>% 
          mutate(planname = paste0(planname, "_fillin"))
}

 
if(!simDist_retirees)        init_retirees_all %<>% integrate_HPP()
if(!simDist_disb)            init_disb_all     %<>% integrate_HPP()
if(!simDist_benenficiaries)  init_beneficiaries_all %<>% integrate_HPP()
init_terms_all         %<>% integrate_HPP()


# for actives
df_nonHPP <- init_actives_all %>% filter(!grepl("HPP", planname)) 
df_HPP <- init_actives_all %>% filter(grepl("HPP", planname))

var.n <- "nactives"
var.v <- "salary"
df_HPP %<>% 
  rename_("nmember" = var.n, "value" = var.v) %>% 
  mutate(tier = str_extract(planname, "t\\d"),
         HPP  = str_extract(planname, "[a-z]?[a-z]?HPP"),
         type = str_extract(planname, "grouped|fillin")) %>% 
  gather(variable, value, -planname, -age, -ea, -yos, -tier, -HPP, -type) %>% 
  mutate(varname.new = paste(variable, HPP, sep = "_")) %>%
  select(-planname, -variable, -HPP, -yos) %>% 
  spread(varname.new, value) %>% 
  mutate_each(funs(na2zero)) %>% 
  mutate(nmember = nmember_HPP + nmember_noHPP,
         value   = ifelse(nmember == 0, 0, (nmember_HPP * value_HPP + nmember_noHPP * value_noHPP)/nmember),
         planname = paste("Actives", tier, type, sep = "_"),
         yos = age - ea) %>% 
  select(planname, age, ea, yos, nmember, value) %>%
  plyr::rename(c("nmember" = var.n, "value" = var.v)) %>% 
  arrange(planname, age)


init_actives_all <- bind_rows(df_nonHPP, df_HPP)



#*************************************************************************************************************
#                           Post processing to make data usable to the model                       #####                  
#*************************************************************************************************************

init_terms_all %<>% mutate(ea = 20) 

# 
#    mutate(year = Global_paramlist$init.year,
#           age.term = age - 1,   # assume all terms are terminated in init.year - 1.
#           ea   = age.term - yos,
#           start.year = year - (age - ea),
#           planname = "Terms_t76_grouped") 

# init_terms_all
# 
# get_tierData(init_terms_all, Tier_select) %>% print

#*************************************************************************************************************
#                           Gender and occuplation(fire/polic) distribuitons                        #####                  
#*************************************************************************************************************
# Gender distribution is required to calculate weighted average mortality
# occupation distribution is required to calculate weighted average retirement rates, term rates, and disability rates.  


# Occupation distribution. 
 # Since tier 1 has no actives, arbitrary numbers are assigned to it. 
 # Harbor members are ignored when calculating proportions. 

prop.occupation <- read_ExcelRange(file_memberData, "prop.occupation") %>% 
  mutate(n.fire_plc = n.fire + n.plc,
         pct.fire   = n.fire / n.fire_plc,
         pct.plc    = 1 - pct.fire)

row.names(prop.occupation) <- prop.occupation$tier



# Gender distribution
 # In p82 CAFR2015, average life expectancy for retirees are calculated based on a proportion of 95% male and 5% female. 
 # For now, the ratio is applied to the entire population. 

pct.male   <- 0.95
pct.female <-  1 - pct.male



save(init_actives_all, init_retirees_all, init_beneficiaries_all, init_disb_all, init_terms_all,
     prop.occupation, pct.male, pct.female,
     file = "Data_inputs/LAFPP_MemberData.RData")


# init_actives_all




# #*************************************************************************************************************
# #                                       Importing Data for initial retirees and beneficiaries            #####                  
# #*************************************************************************************************************
# 
# fn_ret.ben <- function(sheet, path_ = path, fileName_ = fileName){
#   
#   # path_ = path
#   # fileName_ = fileName
#   # Tier_select = "t76"
#   
#   ldata <- import_retirees_byAge(paste0(path, fileName), sheet, sheet)
#   
#   df_grouped <- ldata$data %>% select(planname, age, N, V) %>% mutate(planname = paste0(planname, "_grouped"))
#   names(df_grouped)[names(df_grouped) == "N"] <- ldata$varNames["name_N"]
#   names(df_grouped)[names(df_grouped) == "V"] <- ldata$varNames["name_V"]
#   
#   df_fillin  <- fillin.retirees(ldata) %>% ungroup %>% select(-age.cell) %>%
#     mutate(planname = paste0(planname, "_fillin"))
#   
#   df_out <- bind_rows(df_fillin, df_grouped)
# } 
# 
# init_retirees_all      <- fn_ret.ben("Retirees_t76")
# init_beneficiaries_all <- fn_ret.ben("Beneficiaries_t76")
# 
# # Assume t13 and tm13 have no initial retirees or beneficiaries
# 
# init_retirees_all <- bind_rows(init_retirees_all,
#                                init_retirees_all %>% mutate(nretirees = 0, benefit = 0,
#                                                             planname = gsub("t76", "t13", planname)),
#                                init_retirees_all %>% mutate(nretirees = 0, benefit = 0,
#                                                             planname = gsub("t76", "tm13", planname))
# )
# 
# 
# init_beneficiaries_all <- bind_rows(init_beneficiaries_all,
#                                     init_beneficiaries_all %>% mutate(n.R0S1 = 0, benefit = 0,
#                                                                       planname = gsub("t76", "t13", planname)),
#                                     init_beneficiaries_all %>% mutate(n.R0S1 = 0, benefit = 0,
#                                                                       planname = gsub("t76", "tm13", planname))
# )
# 
# 
# 
# 
# 
# 
# 
# # lretirees      <- import_retirees_byAge(paste0(path, fileName), "Retirees",      paste0("Retirees_",      Tier_select))
# # lbeneficiaries <- import_retirees_byAge(paste0(path, fileName), "Beneficiaries", paste0("Beneficiaries_", Tier_select))
# # 
# # init_retirees      <- fillin.retirees(lretirees) %>% ungroup %>% select(-age.cell)
# # init_beneficiaries <- fillin.retirees(lbeneficiaries) %>% select(-age.cell)
# 
# 
# 
# #*************************************************************************************************************
# #                                       Importing Data for initial terms                                     #####                  
# #*************************************************************************************************************
# 
# # Thoughts on term data 
# # We can only smooth the data along age, and calibrate the benefit payment by adjusting yos. 
# 
# # Notes:
# # assume all terms are terminated in init.year - 1.
# # Currently excluding all cells with yos < 20. Should manage to bring them back in later versions. 
# 
# 
# 
# terms_HAPC <- read_excel(paste0(path, fileName), sheet = "Terms_HAPC_t76_raw", skip = 3) %>% rename(age = Age) %>% 
#   gather(yos, HAPC, -age) %>% 
#   filter(yos != "non-Vested") %>% 
#   mutate(
#     age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
#     age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
#     
#     yos_l =  ifelse(grepl("\\D$",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)) %>% as.numeric,
#     yos_u =  ifelse(grepl("^\\D",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d{2}$")) %>% as.numeric,
#     
#     HAPC = (as.numeric(HAPC) * 12) %>% na2zero,
#     
#     # age_cell = ifelse(is.na(age_l), age_u - 3, age_l+3),
#     # yos_cell = ifelse(is.na(yos_u), yos_l + 4, round((yos_l + yos_u)/2 )),
#     
#     age_cell = ifelse(is.na(age_u), age_l + 3, age_u - 2),
#     yos_cell = ifelse(yos_l == 5, 7, ifelse(yos_l == 11, 12, 21)),
#     # yos_cell = yos_l,
#     
#     age = NULL,
#     yos = NULL
#   )
# 
# terms_n <- read_excel(paste0(path, fileName), sheet = "Terms_N_t76_raw", skip = 5) %>% rename(age = Age) %>% 
#   gather(yos, nterm, -age) %>% 
#   filter(yos != "non-Vested", yos != "All", age != "All") %>% 
#   mutate(
#     age_l =  ifelse(grepl("\\D$",  age),  str_extract(age, "\\d+"),  str_extract(age, "^\\d{2}")) %>% as.numeric,
#     age_u =  ifelse(grepl("^\\D",  age),  str_extract(age, "\\d+"),  str_extract(age, "\\d{2}$")) %>% as.numeric,
#     
#     yos_l =  ifelse(grepl("\\D$",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d+-") %>% gsub("\\D+", "",.)) %>% as.numeric,
#     yos_u =  ifelse(grepl("^\\D",  yos),  str_extract(yos, "\\d+"),  str_extract(yos, "\\d{2}$")) %>% as.numeric,
#     
#     nterm = as.numeric(nterm) %>% na2zero,
#     
#     # age_cell = ifelse(is.na(age_l), age_u - 3, age_l+3),
#     # yos_cell = ifelse(is.na(yos_u), yos_l + 4, round((yos_l + yos_u)/2 )),
#     
#     age_cell = ifelse(is.na(age_u), age_l + 3, age_u - 2),
#     yos_cell = ifelse(yos_l == 5, 7, ifelse(yos_l == 11, 12, 21)),
#     # yos_cell = yos_l,
#     
#     age = NULL,
#     yos = NULL
#   )
# 
# init_terminated_all <-  terms_n %>% 
#   select(age = age_cell, yos = yos_cell, nterm) %>% 
#   left_join(terms_HAPC %>% select(age = age_cell, yos = yos_cell, HAPC)) %>% 
#   mutate(year = Global_paramlist$init.year,
#          age.term = age - 1,   # assume all terms are terminated in init.year - 1.
#          ea   = age.term - yos,
#          start.year = year - (age - ea),
#          planname = "Terms_t76_grouped") 
# 
# # %>% 
# # filter(age.term >= Global_paramlist$min.ea,
# #        ea >= Global_paramlist$min.ea)
# 
# # assme age.term is age - 1, ea must be greater than 20
# 
# 
# # assume t13 and tm13 have no initial terms
# 
# 
# init_terminated_all <- bind_rows(init_terminated_all,
#                                  init_terminated_all %>% mutate(nterm = 0, HAPC = 0,
#                                                                 planname = gsub("t76", "t13", planname)),
#                                  init_terminated_all %>% mutate(nterm = 0, HAPC = 0,
#                                                                 planname = gsub("t76", "tm13", planname))
# )
# 
# 
# init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
#                                 ea >= Global_paramlist$min.ea)
# 
# 
# 
# #*************************************************************************************************************
# #                                        Choose data for selected tier                                    #####                  
# #*************************************************************************************************************
# # Tier_select <- "t76"
# # Grouping <- "fillin"
# 
# get_tierData <- function(df, tier = Tier_select, grouping = paramlist$Grouping) df %<>% filter(grepl(tier, planname), grepl(grouping, planname))
# 
# #t1976
# init_actives.t76       <- get_tierData(init_actives_all, "t76")
# init_retirees.t76      <- get_tierData(init_retirees_all, "t76")
# init_beneficiaries.t76 <- get_tierData(init_beneficiaries_all, "t76")
# init_terminated.t76    <- init_terminated_all %>%  filter(grepl("t76", planname))
# 
# 
# #t2014
# init_actives.t13       <- get_tierData(init_actives_all, "t13")
# init_retirees.t13      <- get_tierData(init_retirees_all, "t13")
# init_beneficiaries.t13 <- get_tierData(init_beneficiaries_all, "t13")
# init_terminated.t13    <- init_terminated_all %>%  filter(grepl("t13", planname))
# 
# 
# #t modified 2014
# init_actives.tm13       <- get_tierData(init_actives_all, "tm13")
# init_retirees.tm13      <- get_tierData(init_retirees_all, "tm13")
# init_beneficiaries.tm13 <- get_tierData(init_beneficiaries_all, "tm13")
# init_terminated.tm13    <- init_terminated_all %>%  filter(grepl("tm13", planname))
# 
# 
# 
# 
# 
# #*************************************************************************************************************
# #                                       Importing Summary data                                     #####                  
# #*************************************************************************************************************
# 
# summary_actives <- read_excel("Data/UCRP-MembersData-2015.xlsx", sheet = "Sum_Active", skip = 1) %>% 
#   gather(Tier, n, -Type, -Sex) %>% 
#   filter(!Tier %in% c("Tier2", "All"), Type != "Safety") %>% 
#   mutate(n    = as.numeric(n) %>% na2zero,
#          # Tier = levels(Tier)[Tier], # no longer needed with the new version of tidyr
#          Tier = ifelse(Tier == "13Tier", "t13", Tier),
#          Tier = ifelse(Tier == "76Tier", "t76", Tier),
#          Tier = ifelse(Tier == "Modi13", "tm13", Tier))
# 
# summary_actives
# 
# 
# #*************************************************************************************************************
# #                                        gender and faculty-staff ratios                                 #####                  
# #*************************************************************************************************************
# 
# 
# # Gender ratio
# ratio_gender <- summary_actives %>% 
#   # filter(Tier == Tier_select) %>% 
#   group_by(Sex) %>% 
#   summarise(n = sum(n))
# 
# pct.F.actives <- with(ratio_gender, n[Sex == "F"]/sum(n))
# pct.M.actives <- 1 - pct.F.actives
# 
# 
# 
# 
# # Employment type ratios
# ratio_type <- summary_actives %>% group_by(Type, Tier) %>% 
#   summarise(n = sum(n))
# 
# pct.fac.actives.t76 <- with(ratio_type %>% filter(Tier == "t76"), n[Type == "Faculty"]/sum(n))
# pct.stf.actives.t76 <- 1 - pct.fac.actives.t76 
# 
# pct.fac.actives.t13 <- with(ratio_type %>% filter(Tier == "t13"), n[Type == "Faculty"]/sum(n))
# pct.stf.actives.t13 <- 1 - pct.fac.actives.t13 
# 
# 
# # The relatively small number of members for tm13 may not accurately reflect the type ratio at the maturity of the plan. 
# # For now we may want to apply the ratio for t13 to it.
# # Need to check with UCRP about whether the ratio of 15/9291 reflects the nature of the plan.
# 
# # pct.fac.actives.tm13 <- with(ratio_type %>% filter(Tier == "tm13"), n[Type == "Faculty"]/sum(n))
# # pct.stf.actives.tm13 <- 1 - pct.fac.actives.tm13  
# 
# pct.fac.actives.tm13 <- pct.fac.actives.t13
# pct.stf.actives.tm13 <- 1 - pct.fac.actives.tm13 
# 
# # 
# # ratio_type.all <- ratio_type %>% group_by(Type) %>% summarise(n = sum(n))
# # pct.fac.actives.all <- with(ratio_type.all, n[Type == "Faculty"]/sum(n))
# # pct.sft.actives.all <- 1 - pct.fac.actives.all
# # 
# 
# 
# 
# 
# # Choice between life annuity and contingent annuity
# pct.ca.t76 <- paramlist$pct.ca.F * pct.F.actives + paramlist$pct.ca.M * pct.M.actives # For those opting for annuit rather than LSC, the % of choosing contingent annuity (0% for 2013 and modified 2013 tier)
# pct.la.t76 <- 1 - pct.ca.t76                                          # For those opting for annuit rather than LSC, the % of choosing life annuity (100% for 2013 and modified 2013 tier)
# 
# pct.ca.t13 <- pct.ca.tm13 <- 0
# pct.la.t13 <- pct.la.tm13 <- 1
# 
# 
# 
# 
# # Final outputs of this file
# 
# # init_actives
# # init_retirees
# # init_beneficiaries
# # init_terminated
# #
# 
# #
# # pct.F.actives
# # pct.M.actives
# # 
# # 
# 
# # pct.fac.actives.t76
# # pct.stf.actives.t76
# # 
# # pct.fac.actives.t13 
# # pct.stf.actives.t13
# # 
# # pct.fac.actives.tm13
# # pct.stf.actives.tm13
# # 
# 
# # pct.ca.t76 # For those opting for annuit rather than LSC, the % of choosing contingent annuity (0% for 2013 and modified 2013 tier)
# # pct.la.t76 # For those opting for annuit rather than LSC, the % of choosing life annuity (100% for 2013 and modified 2013 tier)
# #
# # pct.ca.t13
# # pct.la.t13
# #
# # pct.ca.tm13
# # pct.la.tm13
# 
# 
# # decrement.ucrp %>% filter(ea == 20) %>% select(age, qxm.pre) %>% mutate(qxm.pre = qxm.pre * 100)

# init_disb_all %>% filter(grepl("fillin", planname)) %>% group_by(planname) %>% 
#   summarize(avg_age = sum(age * ndisb)/sum(ndisb) )





