# This script calculates individdual liabilities and normal costs for LAFPP members. 

# Road map

# 1.  Preparation

# 2.1 AL and NC of life annuity and contingent annuity for actives
# 2.2 AL and benefit for retirees with life annuity

# 3.1 AL and NC of deferred benefits for actives
# 3.2 AL and benefits for vested terminated members

# 4.1 AL and NC of benefit for death before retirement
# 4.2 AL and benefits for vested terminated members

# 5.1 AL and NC of disability benefit
# 5.2 AL and benefits for disability benefit

# 6. Selecting variables for the chosen actuarial method







get_indivLab <- function(Tier_select_,
                         decrement.model_ = decrement.model,
                         salary_          = salary,
                         benefit_         = benefit,
                         benefit.disb_    = benefit.disb,
                         bfactor_         = bfactor,
                         mortality.post.model_ = mortality.post.model,
                         liab.ca_ = liab.ca,
                         liab.disb.ca_ = liab.disb.ca,
                         init_terms_all_  = init_terms_all,
                         paramlist_ = paramlist,
                         Global_paramlist_ = Global_paramlist){

# Inputs
  # decrement.model_ = decrement.model
  # salary_          = salary
  # benefit_         = benefit
  # benefit.disb_    = benefit.disb
  # bfactor_         = bfactor
  # init_terms_all_ = init_terms_all # get_tierData(init_terms_all, Tier_select)
  # Tier_select_     = "t1"
  # mortality.post.model_ = mortality.post.model
  # liab.ca_         = liab.ca
  # liab.disb.ca_ = liab.disb.ca
  # paramlist_       =  paramlist
  # Global_paramlist_ =  Global_paramlist


assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(paramlist_,        envir = environment())

# Choosing tier specific parameters and data
# fasyears <- tier.param[Tier_select_, "fasyears"]
# r.vben   <- tier.param[Tier_select_, "r.vben"]
# r.yos    <- tier.param[Tier_select_, "r.yos"]
# r.age    <- tier.param[Tier_select_, "r.age"]
# v.yos    <- tier.param[Tier_select_, "v.yos"]
# cola     <- tier.param[Tier_select_, "cola"]

init_terminated_ <-  get_tierData(init_terms_all_, Tier_select_)


#*************************************************************************************************************
#                               1. Preparation                        #####                  
#*************************************************************************************************************
# Starts with a simple case with only 2 entry ages: 20 and 74

min.year <- min(init.year - (max.age - (r.max - 1)), init.year - (r.max - 1 - min.ea))
## Track down to the year that is the smaller one of the two below: 
# the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
# the year a r.max year old active in year 1 enter the workforce at age min.ea 

# liab.ca %>% filter(age == age.r) %>% select(age, liab.ca.sum.1)


liab.active <- expand.grid(start.year = min.year:(init.year + nyear - 1) , 
                           ea = range_ea, age = range_age) %>%
  filter(start.year + max.age - ea >= init.year, age >= ea) %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>% 
  left_join(salary_) %>%
# left_join(.benefit) %>% 
  left_join(decrement.model_) %>% 
  #left_join(bfactor_) %>%
  left_join(mortality.post.model_ %>% filter(age == age.r) %>% select(age, ax.r.W)) %>%
  left_join(liab.ca_ %>% filter(age == age.r) %>% select(age, liab.ca.sum.1)) %>% 
  left_join(liab.disb.ca_ %>% filter(age == age.disb) %>% select(age, liab.disb.ca.sum.1 = liab.ca.sum.1)) %>% 
  group_by(start.year, ea) %>%
  
  
  # # Chnange variable names for 1976 tier
  # rename(pxT = pxT.t76,
  #        qxr.la = qxr.la.t76,
  #        qxr.LSC  = qxr.LSC.t76,
  #        bfactor = bf.non13) %>% 
  
  # Calculate salary and benefits
  mutate(
    yos= age - ea,
    
    #EEC = ifelse(yos > EEC.exempt.yos, 0, sx * EEC.rate),
    
    # years of service
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary

    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    
    cola.compound = cola.compound,
    COLA.scale = ifelse(cola.compound, (1 + cola)^(age - min(age)), 1 + cola * (age - min(age))),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
 
    
    Bx = na2zero(yos * bfactor * fas),            # accrued benefits, note that only Bx for ages above r.min are necessary under EAN.
    bx = lead(Bx) - Bx,                           # benefit accrual at age x

    # actuarial present value of future benefit, for $1's benefit in the initial year. 
    ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale),    # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    ax.disb.la  = get_tla(pxm.d, i, COLA.scale),     
    # ax.r = get_tla(pxm.r, i, COLA.scale),       # ax calculated with mortality table for retirees. 
    
    
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i, sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    #   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    #   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    axr = ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i), rep(0, max.age - r.vben + 1))),                 # Similar to axR, but based on r.vben.  For calculation of term benefits when costs are spread up to r.min.        
    axrs= ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i, sx[age<r.vben]), rep(0, max.age - r.vben + 1))),  # Similar to axRs, but based on r.vben. For calculation of term benefits when costs are spread up to r.min.
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )


# liab.active %>% select(start.year, ea, age, year, fas, b)
#                 #filter(start.year == 2015 , ea == 70) %>% 
#                 mutate( COLA.scale = (1 + cola)^(age - min(age)), 
#                 ax.LSC = get_tla(1 - qxm.LSC, i, COLA.scale))
# 
# 



#*************************************************************************************************************
#                        2.1  ALs and NCs of life annuity and contingent annuity for actives             #####                  
#*************************************************************************************************************

# Present value of future benefit given starting benefit
 # Group by start.year, ea, age.r 
 # for retirees who retire in or after year 1, starting benefit is given by Bx
 # for retirees who retire before year 1 (initial retirees), starting benefit is given by "benefit"
 # for each "start.year-ea-age.r" group,
    # 1. starting benefit is Bx/benefit with age = age.r,
    # 2. calculate future benefit (benefit for age >= age.r) according to COLA rules. 
    # 3. calculate actuarial present value for each age >= age.r.  

PV.annuity <- rbind(
  # grids for initial retirees in year 1
  # To simplified the calculation, it is assmed that all initial retirees entered the workforce at age r.min - 1 and 
  # retire right in year 1. This assumption will cause the retirement age and yos of some of the retirees not compatible with the eligiblility rules,
  # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial retirees/beneficiaries.
  
  # expand.grid(ea         = r.min - 1,
  #             age.r      = benefit_$age, # This ensures that year of retirement is year 1.
  #             start.year = init.year - (benefit_$age - (r.min - 1)),
  #             age        = range_age[range_age >= r.min]) %>%
  #   filter(age >= ea + 1 - start.year),
  
   expand.grid(
              #ea        = r.min - 1,
              age.r      = benefit_$age, # This ensures that year of retirement is year 1.
              #start.year = init.year - (benefit_$age - (r.min - 1)),
              age        = range_age[range_age >= r.min]) %>%
   mutate(ea = r.min - 1,
            start.year = init.year - (age.r - ea)) %>% 
   filter(# age >= ea + 1 - start.year,
          age >= age.r),
  
  # grids for who retire after year 1.
  expand.grid(ea         = range_ea[range_ea < r.max],
              age.r      = r.min:r.max,
              start.year = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age        = range_age[range_age >=r.min]) %>%
    filter(age   >= ea,
           age.r >= ea,
           age   >= age.r,
           start.year + (age.r - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea     >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.r,age")

PV.annuity <- PV.annuity[!duplicated(PV.annuity %>% select(start.year, ea, age, age.r))]

# PV.annuity %>% arrange(start.year, age.r, ea)

PV.annuity <-  
  merge(PV.annuity,
        select(liab.active, start.year, ea, age, Bx) %>% data.table(key = "ea,age,start.year"),
        all.x = TRUE, 
        by = c("ea", "age","start.year")) %>%
  arrange(start.year, ea, age.r) %>% 
  as.data.frame %>% 
  left_join(select(mortality.post.model_, age, age.r, pxm.post.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
  left_join(benefit_) 


#PV.annuity %>% filter(age == age.r) %>% head(20)
#PV.annuity %>% filter(start.year == 2000, age.r == 50, ea == 30) %>% head


PV.annuity %<>% 
  group_by(start.year, age.r, ea) %>% 
  mutate(
    year.r = start.year + age.r - ea, # year of retirement
    Bx     = ifelse(year.r > init.year, Bx, benefit),
    BwCOLA     = get_BwCOLA(Bx[age == age.r], min(age):max(age), cola, F, 300),
    APV.BwCOLA = get_rollingAPV(BwCOLA, pxm.post.W, i)  # rolling actuarial PV from the year of retirement
    ) 


# PV.annuity %>% filter(start.year == 1990, age.r == 60, ea == 34)



# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
# liab.active1 <- 
liab.active %<>% 
  left_join(PV.annuity %>% ungroup %>%  filter(age == age.r) %>% select(-age.r, -Bx, -benefit, -year.r)) %>% 
  mutate( gx.laca = ifelse((age >= r.age1 & yos >= r.yos1 ) | (age >= r.age2 & yos >= r.yos2), 1, 0),
          #gx.laca = 0,
  Bx.laca  = gx.laca * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
  # TCx.la   = lead(Bx.laca) * qxr.la * lead(ax.r.W) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
  # TCx.ca   = lead(Bx.laca) * qxr.ca * lead(liab.ca.sum.1) * v,  # term cost of contingent annuity at the internal retirement age x (start to claim benefit at age x + 1)
  # TCx.laca = TCx.la + TCx.ca,
  
  APV.BwCOLA = gx.laca * APV.BwCOLA, # set PV of benefit for 0 for ineligible age/yos.
  TCx.la     = lead(APV.BwCOLA) * qxr.la * v,
  TCx.laca   = TCx.la,
  
  PVFBx.laca  = c(get_PVFB(pxT[age <= r.max], v, TCx.laca[age <= r.max]), rep(0, max.age - r.max)),
  
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
  
  # NC and AL of EAN.CD
  NCx.EAN.CD.laca = ifelse(age < r.max, PVFBx.laca[age == min(age)]/ayx[age == r.max], 0),
  ALx.EAN.CD.laca = PVFBx.laca - NCx.EAN.CD.laca * axR,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.laca   = ifelse(age < r.max, sx * PVFBx.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
  PVFNC.EAN.CP.laca = NCx.EAN.CP.laca * axRs,
  ALx.EAN.CP.laca   = PVFBx.laca - PVFNC.EAN.CP.laca
  ) 



#*************************************************************************************************************
#                       2.2   ALs and benefits for retirees with life annuity                        #####                  
#*************************************************************************************************************


# Calculate AL and benefit payment for retirees having retired at different ages.
liab.la <- rbind(
  # grids for initial retirees in year 1
    # To simplified the calculation, it is assmed that all initial retirees entered the workforce at age r.min - 1 and 
    # retiree right in year 1. This assumption will cause the retirement age and yos of some of the retirees not compatible with the eligiblility rules,
    # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial retirees/beneficiaries.
  expand.grid(ea         = r.min - 1,
              age.r      = benefit_$age, # This ensures that year of retirement is year 1.
              start.year = init.year - (benefit_$age - (r.min - 1)),
              age        = range_age[range_age >= r.min]) %>%
  filter(age >= ea + 1 - start.year),

  # grids for who retire after year 1.
  expand.grid(ea         = range_ea[range_ea < r.max],
              age.r      = r.min:r.max,
              start.year = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age        = range_age[range_age >=r.min]) %>%
    filter(age   >= ea,
           age.r >= ea,
           age   >= age.r,
           start.year + (age.r - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea     >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.r,age")
 

liab.la <- liab.la[!duplicated(liab.la %>% select(start.year, ea, age, age.r ))]
 
 
liab.la <- merge(liab.la,
                 select(liab.active, start.year, ea, age, gx.laca) %>% data.table(key = "ea,age,start.year"),
                 all.x = TRUE, 
                 by = c("ea", "age","start.year")) %>%
           arrange(start.year, ea, age.r) %>% 
           as.data.frame %>% 
           left_join(select(PV.annuity, start.year, ea, age, age.r, BwCOLA, APV.BwCOLA) ) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
           left_join(benefit_)


liab.la %<>% as.data.frame  %>% # filter(start.year == -41, ea == 21, age.retire == 65) %>%
  # filter(ea == 54) %>%
  group_by(start.year, ea, age.r) %>%
  mutate(
    year   = start.year + age - ea,
    year.r = start.year + age.r - ea, # year of retirement
    #Bx.laca  = ifelse(is.na(Bx.laca), 0, Bx.laca),  # just for safety
    B.la   = na2zero(BwCOLA),        # annual benefit (life annuity) 
    ALx.la = na2zero(APV.BwCOLA)     # liability for remaining retirement benefit
    
    # B.la   = ifelse(year.r <= init.year,
    #                 benefit[year == init.year] * COLA.scale / COLA.scale[year == init.year],      # Benefits for initial retirees
    #                 Bx.laca[age == age.r] * COLA.scale / COLA.scale[age == age.r]),               # Benefits for retirees after year 1
    # ALx.la = B.la * ax.r.W.ret                                                                    # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA
  
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.r, age.r, start.year, B.la, ALx.la) %>% 
  arrange(age.r, start.year, ea, age)


# l.flexCOLA <- list(PV.annuity, liab.active, liab.la) 
# save(l.flexCOLA, file = "l.flexCOLA.RData")

# Spot check
# liab.la %>% filter(start.year == 2016, ea == 20, age.r == 51) %>% as.data.frame()


#*************************************************************************************************************
#                         3.1 AL and NC of deferred benefits for actives                        #####
#*************************************************************************************************************

# Calculate normal costs and liabilities of deferred retirement benefits
# Vested terms begin to receive deferred retirement benefit at r.vben, after which they are considered as retirees in the AV.(But still terms in the model)
# Notes on deferred retirement benefits for vested terms.
# 1. Note that the PVFB and AL are different at age r.min - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
#    at age r.max
# 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
#    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the
#    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balanced.
#
# CAUTION!: There will be a problem if actives entering after r.min can get vested, when PVFB is only amortized up to age r.min

liab.active %<>%
  mutate(gx.v = ifelse(yos >= v.yos, 1, 0),  # actives become vested after reaching v.yos years of yos
         
         Bx.v = ifelse(ea < r.vben, 
                       gx.v * bfactor * yos * fas, 0), # initial annuity amount when the vested term retires at age r.vben, when a employee is vested at a certain age.

         TCx.v   = ifelse(ea < r.vben, Bx.v * qxt * lead(px_r.vben_m) * v^(r.vben - age) * ax.r.W[age == r.vben], 0),             # term cost of vested termination benefits. We assume term rates are 0 after r.vben.
         PVFBx.v = ifelse(ea < r.vben, c(get_PVFB(pxT[age < r.vben], v, TCx.v[age < r.vben]), rep(0, max.age - r.vben + 1)), 0),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).

         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),

         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < r.vben, PVFBx.v[age == min(age)]/ayx[age == r.vben], 0), # for testing spreading NC.v up to r.vben
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,

         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < r.vben, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.vben]) * sx, 0),  # for testing spreading NC.v up to r.vben
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axrs
  ) 
  
# x <- liab.active %>% filter(start.year == 1, ea == 20)



#*************************************************************************************************************
#                       3.2 AL for vested terminatede members                        #####
#*************************************************************************************************************

# # Calculate AL and benefit payment for initial vested terms.

init_terminated_ %<>%  
  mutate(year = init.year,
         age.term = age - 1,         # assume all terms are terminated in init.year - 1.
         yos = age - ea,
         start.year = year - (age - ea))

init_terminated_


liab.term.init <- expand.grid(ea         = unique(init_terminated_$ea),
                              age.term   = unique(init_terminated_$age.term),
                              start.year = unique(init_terminated_$start.year),
                              age = range_age) %>%
  filter(start.year + age - ea >= 1,
         age >= ea,
         age.term >= ea) %>%
  left_join(init_terminated_ %>% select(ea, age.term, start.year, yos, benefit)) %>%
  left_join(select(liab.active, start.year, ea, age, COLA.scale, pxRm, px_r.vben_m)) %>%
  left_join(mortality.post.model_ %>% filter(age.r == r.vben) %>% select(age, ax.r.W.term = ax.r.W)) %>%
  group_by(start.year, ea, age.term) %>%

  mutate(
    year = start.year + age - ea,
    age.ben  = ifelse(age[year == init.year] > r.vben, age[year == init.year], r.vben), # Age at which term starts to receive benefit. 

    year.term = year[age == age.term],

    Bx.v  = benefit, #
    B.v   = ifelse(age.ben > r.vben, 0,   ifelse(age >= r.vben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.vben], 0)),  # Benefit payment after r.vben, for age.ben == r.vben
    B.v   = ifelse(age.ben == r.vben, B.v, ifelse(age >= age.ben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == age.ben], 0)), # for age.ben > r.vben
    ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.vben] * px_r.vben_m * v^(r.vben - age), # liab before receiving benefits
                   B.v * ax.r.W.term)) %>%                                                                                     # liab after receiving benefits      
  ungroup %>%
  select(ea, age, start.year, year, year.term, B.v, ALx.v) %>%
  filter(year %in% seq(init.year, len = nyear),
         year.term == init.year - 1)



# liab.term.init %>% filter(start.year == 2007, ea == 55, age.term == 63) %>% data.frame()


# Calculate AL and benefit payment for vested terms terminating at different ages.
# Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab.term <- expand.grid(start.year = (init.year - (r.vben - 1 - min.age)):(init.year + nyear - 1), # 2015
                         ea = range_ea[range_ea < r.vben], 
                         age = range_age, 
                         age.term = range_age[range_age < r.vben]) %>% 
  filter(start.year + max.age - ea >= init.year, 
         age >= ea, age.term >= ea,
         age >= age.term) %>% # drop redundant combinations of start.year and ea.
  data.table(key = "ea,age,start.year,age.term") 


liab.term <- merge(liab.term,
                   select(liab.active, start.year, year, ea, age, Bx.v, COLA.scale, pxRm, px_r.vben_m) %>% data.table(key = "ea,age,start.year"),
                   all.x = TRUE, by = c("ea", "age","start.year")) %>% as.data.frame %>% 
             left_join(mortality.post.model_ %>% filter(age.r == r.vben) %>% select(age, ax.r.W.term = ax.r.W))   # load present value of annuity for retirement age r.vben

liab.term %<>% as.data.frame %>%
  group_by(start.year, ea, age.term) %>%
  mutate(year.term = year[age == age.term],

         B.v   = ifelse(age >= r.vben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.vben], 0),  # Benefit payment after r.vben
         ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.vben] * px_r.vben_m * v^(r.vben - age),
                        B.v * ax.r.W.term)

  ) %>%
  ungroup  %>%
  # select(#-start.year, -age.term,
  #        -Bx.v, -ax.r.W, -COLA.scale, -pxRm) %>%

  select(-age.term, -Bx.v, -ax.r.W.term, -COLA.scale, -pxRm, - px_r.vben_m, -age.r) %>%
  filter(year %in% seq(init.year, len = nyear)) 


# liab.term %<>% mutate(B.v   = ifelse(year.term == init.year - 1, 0, B.v),
#                       ALx.v = ifelse(year.term == init.year - 1, 0, ALx.v))





liab.term <-  bind_rows(list(liab.term.init,                                  # Using rbind produces duplicated rows with unknown reasons. Use bind_rows from dplyr instead.
                             filter(liab.term, year.term != init.year - 1)))

# liab.term %>% filter(year == 2015)

# liab.term %>% filter(year.term == 2014, start.year == 1980) %>% head
# liab.term[!duplicated(liab.term %>% select(start.year, ea, age, year.term)),]
#   any(T)



#*************************************************************************************************************
#                        4.1  ALs and NCs of benefit for death before retirement, for actives                  #####                  
#*************************************************************************************************************
# QSS: qualified Surviving Spouse
pct.QSS <- pct.ca.F * pct.female + pct.ca.M * pct.male
pct.QSS



# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.death  = 1,
          Bx.death  = gx.death * Bx,
 
          TCx.death = lead(Bx.death) * qxm.pre * pct.QSS  * lead(ax.deathBen) * v, # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)

          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.death  = c(get_PVFB(pxT[age <= r.max], v, TCx.death[age <= r.max]), rep(0, max.age - r.max)),
          
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          
          # NC and AL of EAN.CD
          NCx.EAN.CD.death = ifelse(age < r.max, PVFBx.death[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.death = PVFBx.death - NCx.EAN.CD.death * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.death   = ifelse(age < r.max, sx * PVFBx.death[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.death = NCx.EAN.CP.death * axRs,
          ALx.EAN.CP.death   = PVFBx.death - PVFNC.EAN.CP.death
  ) 



#*************************************************************************************************************
#                       4.2   ALs and benefits for QSS for death benefit before retirement               #####                  
#*************************************************************************************************************

liab.death <- rbind(
  # grids for who die after year 1.
  expand.grid(ea           = range_ea[range_ea < r.max],
              age.death = min.age:r.max,
              start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age          = range_age) %>%
    filter(age   >= ea,
           age.death >= ea,
           age   >= age.death,
           start.year + (age.death - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.death,age")


liab.death <- merge(liab.death,
                 select(liab.active, start.year, ea, age, Bx.death, COLA.scale, gx.death, ax.deathBen) %>% data.table(key = "ea,age,start.year"),
                 all.x = TRUE, 
                 by = c("ea", "age","start.year")) %>%
  arrange(start.year, ea, age.death) %>% 
  as.data.frame 
  #%>% 
  # left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
  #left_join(benefit_)


liab.death %<>% as.data.frame  %>% 
  group_by(start.year, ea, age.death) %>%
  mutate(
    year       = start.year + age - ea,
    year.death = start.year + age.death - ea, # year of death of the active
    Bx.death   = ifelse(is.na(Bx.death), 0, Bx.death),  # just for safety
    B.death    = Bx.death[age == age.death] * COLA.scale / COLA.scale[age == age.death],               # Benefits for retirees after year 1
    ALx.death     = B.death * ax.deathBen                                                                # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA
    
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.death, age.death, start.year, B.death, ALx.death) %>% 
  arrange(age.death, start.year, ea, age)


# liab.death %>% ungroup %>% arrange(start.year, ea, year.death, age) %>%  head(100)



#*************************************************************************************************************
#                        5.1  ALs and NCs of disability benefit, for actives                  #####                  
#*************************************************************************************************************



# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.disb  = 1,
          Bx.disb  = gx.disb * Bx, 
          
          # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
          TCx.disb.la = lead(Bx.disb) * qxd.la * lead(ax.disb.la) * v, # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
          TCx.disb.ca = lead(Bx.disb) * qxd.ca * lead(liab.disb.ca.sum.1) * v,
          TCx.disb.laca = TCx.disb.la + TCx.disb.ca,
          
          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.disb  = c(get_PVFB(pxT[age <= r.max], v, TCx.disb.laca[age <= r.max]), rep(0, max.age - r.max)),
          
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          
          # NC and AL of EAN.CD
          NCx.EAN.CD.disb = ifelse(age < r.max, PVFBx.disb[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.disb = PVFBx.disb - NCx.EAN.CD.disb * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.disb   = ifelse(age < r.max, sx * PVFBx.disb[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.disb = NCx.EAN.CP.disb * axRs,
          ALx.EAN.CP.disb   = PVFBx.disb - PVFNC.EAN.CP.disb
  ) 



#*************************************************************************************************************
#                       5.2   ALs and benefits for disability benefit               #####                  
#*************************************************************************************************************

liab.disb.la <- rbind(
  # grids for initial retirees in year 1
  # To simplified the calculation, it is assmed that all initial disabled entered the workforce at age min.age and 
  # become disabled in year 1. This assumption will cause the age of disability and yos of some of the disabled not compatible with the eligiblility rules,
  # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial disabled.
  expand.grid(ea         = min.age,
              age.disb   = benefit.disb_$age, # This ensures that year of retirement is year 1.
              start.year = init.year - (benefit.disb_$age - min.age),
              age        = range_age) %>%
    filter(age >= ea + 1 - start.year),
  
  # grids for who die after year 1.
  expand.grid(ea           = range_ea[range_ea < r.max],
              age.disb     = min.age:r.max,
              start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age          = range_age) %>%
    filter(age   >= ea,
           age.disb >= ea,
           age   >= age.disb,
           start.year + (age.disb - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.disb,age")

liab.disb.la <- liab.disb.la[!duplicated(liab.disb.la %>% select(start.year, ea, age, age.disb ))]


liab.disb.la <- merge(liab.disb.la,
                    select(liab.active, start.year, ea, age, Bx.disb, COLA.scale, gx.disb, ax.disb.la) %>% data.table(key = "ea,age,start.year"),
                    all.x = TRUE, 
                    by = c("ea", "age","start.year")) %>%
  arrange(start.year, ea, age.disb) %>% 
  as.data.frame %>% 
  left_join(benefit.disb_)
#%>% 
# left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 



liab.disb.la %<>% as.data.frame  %>% 
  group_by(start.year, ea, age.disb) %>%
  mutate(
    year       = start.year + age - ea,
    year.disb  = start.year + age.disb - ea, # year of disability of the active
    Bx.disb    = ifelse(is.na(Bx.disb), 0, Bx.disb),  # just for safety
    B.disb.la     = ifelse(year.disb <= init.year,
                           benefit.disb[year == init.year] * COLA.scale / COLA.scale[year == init.year],  # Benefits for initial retirees
                           Bx.disb[age == age.disb] * COLA.scale / COLA.scale[age == age.disb]),          # Benefits for disability retirees after year 1
    ALx.disb.la   = B.disb.la * ax.disb.la                                                                # Liability for remaining diability benefits, PV of all future benefit adjusted with COLA
    
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.disb, age.disb, start.year, B.disb.la, ALx.disb.la) %>% 
  arrange(age.disb, start.year, ea, age)


# liab.disb %>% ungroup %>% arrange(start.year, ea, year.disb, age) %>%  head(100)





#*************************************************************************************************************
#                 # 6.  Choosing AL and NC variables corresponding to the chosen acturial methed             #####
#*************************************************************************************************************


liab.active %<>% ungroup %>% select(start.year, year, ea, age, everything())


ALx.laca.method   <- paste0("ALx.", actuarial_method, ".laca")
NCx.laca.method   <- paste0("NCx.", actuarial_method, ".laca")

ALx.death.method   <- paste0("ALx.", actuarial_method, ".death")
NCx.death.method   <- paste0("NCx.", actuarial_method, ".death")

ALx.disb.method   <- paste0("ALx.", actuarial_method, ".disb")
NCx.disb.method   <- paste0("NCx.", actuarial_method, ".disb")

ALx.v.method <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method <- paste0("NCx.", actuarial_method, ".v")



var.names <- c("sx", ALx.laca.method, NCx.laca.method, 
                     ALx.v.method, NCx.v.method, 
                     ALx.death.method, NCx.death.method,
                     ALx.disb.method, NCx.disb.method,
                     "PVFBx.laca", "PVFBx.v", "PVFBx.death", "PVFBx.disb", 
                     "Bx.laca", 
                     "Bx.disb", "COLA.scale")
liab.active %<>% 
  filter(year %in% seq(init.year, len = nyear)) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx.laca"   = ALx.laca.method,  "NCx.laca"   = NCx.laca.method, 
          "ALx.v"      = ALx.v.method,     "NCx.v"      = NCx.v.method,
          "ALx.death"  = ALx.death.method, "NCx.death"  = NCx.death.method,
          "ALx.disb"   = ALx.disb.method,  "NCx.disb"   = NCx.disb.method
           )   # Note that dplyr::rename_ is used. 




## Final outputs
  # liab.active
  # liab.la
  # liab.term
  # B.LSC

liab <- list(active = liab.active, 
             la = liab.la, 
             term = liab.term, 
             death = liab.death, 
             disb.la = liab.disb.la)

}


# liab <- get_indivLab(decrement.ucrp,
#                      salary,
#                      benefit,
#                      bfactor,
#                      init_terminated.t76)


