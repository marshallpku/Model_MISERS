# Simulation of the demograhics of UCRP

## Modifications on the original model
#1. Need to calculate the number of LSC claims(by ea, age) for each year. (Can be calculated after the loop) 
#2. Need to calculate the number of new retirees opting for contingent annuity(by ea, age) for each year. (Can be calculated after the loop) 
#3. The mortality for retirees are now retirement age dependent.


get_Population_allTiers_LAFPP <- function(
                           init_pop.t1_         = init_pop.t1,
                           entrants_dist.t1_    = entrants_dist.t1 ,
                           decrement.model.t1_  = list.decrements.t1$decrement.model,
                           mortality.post.model.t1_ = mortality.post.model.t1,
                           
                           init_pop.t2_         = init_pop.t2,
                           entrants_dist.t2_    = entrants_dist.t2 ,
                           decrement.model.t2_   = list.decrements.t2$decrement.model ,
                           mortality.post.model.t2_ = mortality.post.model.t2,
                           
                           init_pop.t3_         = init_pop.t3,
                           entrants_dist.t3_    = entrants_dist.t3 ,
                           decrement.model.t3_   = list.decrements.t3$decrement.model ,
                           mortality.post.model.t3_ = mortality.post.model.t3,
                           
                           init_pop.t4_         = init_pop.t4,
                           entrants_dist.t4_    = entrants_dist.t4 ,
                           decrement.model.t4_   = list.decrements.t4$decrement.model ,
                           mortality.post.model.t4_ = mortality.post.model.t4,
                           
                           init_pop.t5_             = init_pop.t5,
                           entrants_dist.t5_        = entrants_dist.t5 ,
                           decrement.model.t5_      = list.decrements.t5$decrement.model ,
                           mortality.post.model.t5_ = mortality.post.model.t5,
                           
                           init_pop.t6_         = init_pop.t6,
                           entrants_dist.t6_    = entrants_dist.t6 ,
                           decrement.model.t6_  = list.decrements.t6$decrement.model ,
                           mortality.post.model.t6_ = mortality.post.model.t6,
                           
                           paramlist_        = paramlist,
                           Global_paramlist_ = Global_paramlist){

## Inputs
# - range_ea:         all possible entry ages  
# - range_age:        range of age
# - nyear:            number of years in simulation
# - wf_growth:        growth rate of the size of workforce
# - no_entrance:      no new entrants into the workforce if set "TRUE". Overrides "wf_growth"
# - Decrement table:  from Model_Decrements.R  
# - Initial workforce for each type:
#    - init_pop$actives:   matrix, max ea by max age
#    - init_pop$retirees:  matrix, max ea by max age


## An array is created for each of the 6 status:
#  (1)Active     (dim = 3)
#  (2)Terminated (dim = 4)
#  (3)Retired    (dim = 4)
#  (4)Disabled   (dim = 3) later will be expanded to dim = 4, with additional dim being year.disb
#  (5)Dead       (dim = 3) We do not really need an array for dead, what's needed is only the total number of dead.  

# Run the section below when developing new features.   
  # init_pop.t1_         = init_pop.t1
  # entrants_dist.t1_    = entrants_dist.t1
  # decrement.model.t1_  = list.decrements.t1$decrement.model
  # mortality.post.model.t1_ = mortality.post.model.t1
  # 
  # init_pop.t2_         = init_pop.t2
  # entrants_dist.t2_    = entrants_dist.t2
  # decrement.model.t2_   = list.decrements.t2$decrement.model
  # mortality.post.model.t2_ = mortality.post.model.t2
  # 
  # init_pop.t3_         = init_pop.t3
  # entrants_dist.t3_    = entrants_dist.t3
  # decrement.model.t3_   = list.decrements.t3$decrement.model
  # mortality.post.model.t3_ = mortality.post.model.t3
  # 
  # init_pop.t4_         = init_pop.t4
  # entrants_dist.t4_    = entrants_dist.t4
  # decrement.model.t4_   = list.decrements.t4$decrement.model
  # mortality.post.model.t4_ = mortality.post.model.t4
  # 
  # init_pop.t5_         = init_pop.t5
  # entrants_dist.t5_    = entrants_dist.t5
  # decrement.model.t5_   = list.decrements.t5$decrement.model
  # mortality.post.model.t5_ = mortality.post.model.t5
  # 
  # init_pop.t6_         = init_pop.t6
  # entrants_dist.t6_    = entrants_dist.t6
  # decrement.model.t6_  = list.decrements.t6$decrement.model
  # mortality.post.model.t6_ = mortality.post.model.t6
  # 
  # paramlist_        = paramlist
  # Global_paramlist_ = Global_paramlist

# #   
#   

 assign_parmsList(Global_paramlist_, envir = environment())
 assign_parmsList(paramlist_,        envir = environment())  

 pct.QSS <- pct.ca.F * pct.female + pct.ca.M * pct.male

#*************************************************************************************************************
#                                     Creating arrays for each status, FOR ALL TIERS ####
#*************************************************************************************************************

## In each 3D array, dimension 1(row) represents entry age, dimension 2(column) represents attained age,
# dimension 3(depth) represents number of year, dimension 4(terms only) represents the termination year. 
wf_dim      <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, range_age, init.year:(init.year + nyear - 1))

# The array of terminated has 4 dimensions: ea x age x year x year of termination
wf_dim.term      <- c(length(range_ea), length(range_age), nyear, nyear + 1)
wf_dimnames.term <- list(range_ea, range_age, init.year:(init.year + nyear - 1), (init.year - 1) :(init.year + nyear - 1))


# The array of retirees has 4 dimensions: ea x age x year x year of retirement
wf_dim.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.la <- list(range_ea, range_age, init.year:(init.year + nyear - 1), init.year:(init.year + nyear - 1))


# The array of death beneficiearies has 4 dimensions: ea x age x year x year of death(of the active)
wf_dim.deathBen      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.deathBen <- list(range_ea, range_age, init.year:(init.year + nyear - 1), init.year:(init.year + nyear - 1))


# The array of disability retirees has 4 dimensions: ea x age x year x year of death(of the active)
wf_dim.disb.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.disb.la <- list(range_ea, range_age, init.year:(init.year + nyear - 1), init.year:(init.year + nyear - 1))



wf_active.t1 <- wf_active.t2 <- wf_active.t3 <- wf_active.t4 <- wf_active.t5 <- wf_active.t6  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_disb.t1   <- wf_disb.t2   <- wf_disb.t3  <- wf_disb.t4   <- wf_disb.t5   <- wf_disb.t6     <- array(0, wf_dim, dimnames = wf_dimnames) 
wf_dead.t1   <- wf_dead.t2   <- wf_dead.t3 <- wf_dead.t4   <- wf_dead.t5   <- wf_dead.t6      <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term.t1   <- wf_term.t2   <- wf_term.t3 <- wf_term.t4   <- wf_term.t5   <- wf_term.t6      <- array(0, wf_dim.term,    dimnames = wf_dimnames.term)
wf_la.t1     <- wf_la.t2     <- wf_la.t3   <- wf_la.t4     <- wf_la.t5     <- wf_la.t6        <- array(0, wf_dim.la, dimnames = wf_dimnames.la)
wf_deathBen.t1  <- wf_deathBen.t2  <- wf_deathBen.t3   <- wf_deathBen.t4    <- wf_deathBen.t5    <- wf_deathBen.t6  <- array(0, wf_dim.deathBen, dimnames = wf_dimnames.deathBen)
wf_disb.la.t1   <- wf_disb.la.t2   <- wf_disb.la.t3    <- wf_disb.la.t4     <- wf_disb.la.t5     <- wf_disb.la.t6   <- array(0, wf_dim.disb.la,  dimnames = wf_dimnames.disb.la)


newDeath.act.t1 <-  newDeath.act.t2 <-   newDeath.act.t3 <- newDeath.act.t4 <-  newDeath.act.t5 <-   newDeath.act.t6   <- numeric(nyear)
newDeath.ret.t1 <-  newDeath.ret.t2 <-   newDeath.ret.t3 <- newDeath.ret.t4 <-  newDeath.ret.t5 <-   newDeath.ret.t6   <- numeric(nyear)
newDeath.term.t1 <- newDeath.term.t2 <-  newDeath.term.t3 <- newDeath.term.t4 <- newDeath.term.t5 <-  newDeath.term.t6 <- numeric(nyear)

newDisb.act.t1 <- newDisb.act.t2 <- newDisb.act.t3 <- newDisb.act.t4 <- newDisb.act.t5 <- newDisb.act.t6 <- numeric(nyear)


#*************************************************************************************************************
#                                     Setting  initial population, TIER SPECIFIC ####
#*************************************************************************************************************

# Setting inital distribution of workforce and retirees.
# Note on initial retirees: It is assumed that all initial retirees entered the workforce at age 54 and retireed in year 1. 
# Altough this may produce yos greater than r.max - ea.min, it is irrelevant to the calculation since we do not care about initial retirees' yos.  
# 

# Tier 1
wf_active.t1[, , 1]   <- init_pop.t1_$actives 
wf_la.t1[, , 1, 1]    <- init_pop.t1_$retirees
wf_term.t1[, , 1, 1]  <- init_pop.t1_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.t1[, , 1, 1]  <- init_pop.t1_$disb

# Tier 2
wf_active.t2[, , 1]   <- init_pop.t2_$actives 
wf_la.t2[, , 1, 1]    <- init_pop.t2_$retirees
wf_term.t2[, , 1, 1]  <- init_pop.t2_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.t2[, , 1, 1]  <- init_pop.t2_$disb

# Tier 3
wf_active.t3[, , 1]   <- init_pop.t3_$actives 
wf_la.t3[, , 1, 1]    <- init_pop.t3_$retirees
wf_term.t3[, , 1, 1]  <- init_pop.t3_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.t3[, , 1, 1]  <- init_pop.t3_$disb

# Tier 4
wf_active.t4[, , 1]   <- init_pop.t4_$actives 
wf_la.t4[, , 1, 1]    <- init_pop.t4_$retirees
wf_term.t4[, , 1, 1]  <- init_pop.t4_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.t4[, , 1, 1]  <- init_pop.t4_$disb

# Tier 5
wf_active.t5[, , 1]   <- init_pop.t5_$actives 
wf_la.t5[, , 1, 1]    <- init_pop.t5_$retirees
wf_term.t5[, , 1, 1]  <- init_pop.t5_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.t5[, , 1, 1]  <- init_pop.t5_$disb

# Tier 6
wf_active.t6[, , 1]   <- init_pop.t6_$actives 
wf_la.t6[, , 1, 1]    <- init_pop.t6_$retirees
wf_term.t6[, , 1, 1]  <- init_pop.t6_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.t6[, , 1, 1]  <- init_pop.t6_$disb



#*************************************************************************************************************
#                                     Defining population dynamics, TIER SPECIFIC  ####
#*************************************************************************************************************

## Transition matrices ####

# Assume the actual decrement rates are the same as the rates in decrement tables.
# Later we may allow the actual decrement rates to differ from the assumed rates. 


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = decrement_wf) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transtion matrix
  df %<>% select_("age", "ea", qx) %>% ungroup %>% spread_("ea", qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}


# select decrement tables

decrement_wf.t1 <- decrement.model.t1_ %>% mutate_each(funs(na2zero)) # just for safety 
decrement_wf.t2 <- decrement.model.t2_ %>% mutate_each(funs(na2zero)) # just for safety 
decrement_wf.t3 <- decrement.model.t3_ %>% mutate_each(funs(na2zero)) # just for safety 
decrement_wf.t4 <- decrement.model.t4_ %>% mutate_each(funs(na2zero)) # just for safety 
decrement_wf.t5 <- decrement.model.t5_ %>% mutate_each(funs(na2zero)) # just for safety 
decrement_wf.t6 <- decrement.model.t6_ %>% mutate_each(funs(na2zero)) # just for safety 


# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transtition matrices. 




## For Tier 1

  # Where do the active go
  p_active2term.t1    <- make_dmat("qxt",     decrement_wf.t1)
  p_active2disb.t1    <- make_dmat("qxd",     decrement_wf.t1)
  p_active2disb.la.t1 <- make_dmat("qxd.la",  decrement_wf.t1)
  p_active2dead.t1    <- make_dmat("qxm.pre", decrement_wf.t1)
  p_active2deathBen.t1<- make_dmat("qxm.pre", decrement_wf.t1) * pct.QSS
  p_active2disb.t1    <- make_dmat("qxd",     decrement_wf.t1)
  p_active2retired.t1 <- make_dmat("qxr",     decrement_wf.t1)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t1      <- make_dmat("qxr.la",  decrement_wf.t1)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t1    <- make_dmat("qxm.term", decrement_wf.t1) 
  
  # Where do the death beneficiaries go
  p_deathBen2dead.t1 <- make_dmat("qxm.deathBen", decrement_wf.t1)
  
  # Where do the disability retirees go
  p_disb.la2dead.t1 <- make_dmat("qxm.d", decrement_wf.t1)
  
  
## For Tier 2
  
  # Where do the active go
  p_active2term.t2    <- make_dmat("qxt",     decrement_wf.t2)
  p_active2disb.t2    <- make_dmat("qxd",     decrement_wf.t2)
  p_active2disb.la.t2 <- make_dmat("qxd.la",  decrement_wf.t2)
  p_active2dead.t2    <- make_dmat("qxm.pre", decrement_wf.t2)
  p_active2deathBen.t2<- make_dmat("qxm.pre", decrement_wf.t2) * pct.QSS
  p_active2disb.t2    <- make_dmat("qxd",     decrement_wf.t2)
  p_active2retired.t2 <- make_dmat("qxr",     decrement_wf.t2)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t2      <- make_dmat("qxr.la",  decrement_wf.t2)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t2    <- make_dmat("qxm.term", decrement_wf.t2) # need to modify later
  
  # Where do the death beneficiaries go
  p_deathBen2dead.t2 <- make_dmat("qxm.deathBen", decrement_wf.t2)
  
  # Where do the disability retirees go
  p_disb.la2dead.t2 <- make_dmat("qxm.d", decrement_wf.t2)
  
  
## For Tier 3
  
  # Where do the active go
  p_active2term.t3    <- make_dmat("qxt",     decrement_wf.t3)
  p_active2disb.t3    <- make_dmat("qxd",     decrement_wf.t3)
  p_active2disb.la.t3 <- make_dmat("qxd.la",  decrement_wf.t3)
  p_active2dead.t3    <- make_dmat("qxm.pre", decrement_wf.t3)
  p_active2deathBen.t3<- make_dmat("qxm.pre", decrement_wf.t3) * pct.QSS
  p_active2disb.t3    <- make_dmat("qxd",     decrement_wf.t3)
  p_active2retired.t3 <- make_dmat("qxr",     decrement_wf.t3)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t3      <- make_dmat("qxr.la",  decrement_wf.t3)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t3    <- make_dmat("qxm.term", decrement_wf.t3) # need to modify later
  
  # Where do the death beneficiaries go
  p_deathBen2dead.t3 <- make_dmat("qxm.deathBen", decrement_wf.t3)
  
  # Where do the disability retirees go
  p_disb.la2dead.t3 <- make_dmat("qxm.d", decrement_wf.t3)
  
  
  ## For Tier 4
  
  # Where do the active go
  p_active2term.t4    <- make_dmat("qxt",     decrement_wf.t4)
  p_active2disb.t4    <- make_dmat("qxd",     decrement_wf.t4)
  p_active2disb.la.t4 <- make_dmat("qxd.la",  decrement_wf.t4)
  p_active2dead.t4    <- make_dmat("qxm.pre", decrement_wf.t4)
  p_active2deathBen.t4<- make_dmat("qxm.pre", decrement_wf.t4) * pct.QSS
  p_active2disb.t4    <- make_dmat("qxd",     decrement_wf.t4)
  p_active2retired.t4 <- make_dmat("qxr",     decrement_wf.t4)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t4      <- make_dmat("qxr.la",  decrement_wf.t4)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t4    <- make_dmat("qxm.term", decrement_wf.t4) # need to modify later
  
  # Where do the death beneficiaries go
  p_deathBen2dead.t4 <- make_dmat("qxm.deathBen", decrement_wf.t4)
  
  # Where do the disability retirees go
  p_disb.la2dead.t4 <- make_dmat("qxm.d", decrement_wf.t4)
  
## For Tier 5
  
  # Where do the active go
  p_active2term.t5    <- make_dmat("qxt",     decrement_wf.t5)
  p_active2disb.t5    <- make_dmat("qxd",     decrement_wf.t5)
  p_active2disb.la.t5 <- make_dmat("qxd.la",  decrement_wf.t5)
  p_active2dead.t5    <- make_dmat("qxm.pre", decrement_wf.t5)
  p_active2deathBen.t5<- make_dmat("qxm.pre", decrement_wf.t5) * pct.QSS
  p_active2disb.t5    <- make_dmat("qxd",     decrement_wf.t5)
  p_active2retired.t5 <- make_dmat("qxr",     decrement_wf.t5)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t5      <- make_dmat("qxr.la",  decrement_wf.t5)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t5    <- make_dmat("qxm.term", decrement_wf.t5) # need to modify later

  
  # Where do the death beneficiaries go
  p_deathBen2dead.t5 <- make_dmat("qxm.deathBen", decrement_wf.t5)
  
  # Where do the disability retirees go
  p_disb.la2dead.t5 <- make_dmat("qxm.d", decrement_wf.t5)
  
  
  ## For Tier 6
  
  # Where do the active go
  p_active2term.t6    <- make_dmat("qxt",     decrement_wf.t6)
  p_active2disb.t6    <- make_dmat("qxd",     decrement_wf.t6)
  p_active2disb.la.t6 <- make_dmat("qxd.la",  decrement_wf.t6)
  p_active2dead.t6    <- make_dmat("qxm.pre", decrement_wf.t6)
  p_active2deathBen.t6<- make_dmat("qxm.pre", decrement_wf.t6) * pct.QSS
  p_active2disb.t6    <- make_dmat("qxd",     decrement_wf.t6)
  p_active2retired.t6 <- make_dmat("qxr",     decrement_wf.t6)      # This include all three types of retirement: LSC, contingent annuity, and life annuity.
  p_active2la.t6      <- make_dmat("qxr.la",  decrement_wf.t6)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.t6    <- make_dmat("qxm.term", decrement_wf.t6) # need to modify later
  
  # Where do the death beneficiaries go
  p_deathBen2dead.t6 <- make_dmat("qxm.deathBen", decrement_wf.t6)
  
  # Where do the disability retirees go
  p_disb.la2dead.t6 <- make_dmat("qxm.d", decrement_wf.t6)
  

  
  
# Where do the retirees go, FOR ALL TIERS 
  # Before we find better approach, the age.r dependent mortality for retirees are given in a data frame containing all combos 
  # of year, year.r, ea, and age that exist in wf_la. 

  p_la2dead.t1 <- p_la2dead.t2 <-p_la2dead.t3 <-p_la2dead.t4 <-p_la2dead.t5 <-p_la2dead.t6 <-
    expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
    #filter(age >= ea) %>% 
    mutate(age.r = age - (year - year.r)) %>% 
    left_join(mortality.post.model.t1_ %>% select(age.r, age, qxm.post.W)) %>%
    mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
    arrange(year, year.r, age, ea)

  # p_la2dead.t2 <- 
  #   expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
  #   #filter(age >= ea) %>% 
  #   mutate(age.r = age - (year - year.r)) %>% 
  #   left_join(mortality.post.model.t2_ %>% select(age.r, age, qxm.post.W)) %>%
  #   mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
  #   arrange(year, year.r, age, ea)
  # 
  # p_la2dead.t3 <- 
  #   expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
  #   #filter(age >= ea) %>% 
  #   mutate(age.r = age - (year - year.r)) %>% 
  #   left_join(mortality.post.model.t3_ %>% select(age.r, age, qxm.post.W)) %>%
  #   mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
  #   arrange(year, year.r, age, ea)
  # 
  # p_la2dead.t4 <- 
  #   expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
  #   #filter(age >= ea) %>% 
  #   mutate(age.r = age - (year - year.r)) %>% 
  #   left_join(mortality.post.model.t4_ %>% select(age.r, age, qxm.post.W)) %>%
  #   mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
  #   arrange(year, year.r, age, ea)
  # 
  # p_la2dead.t5 <- 
  #   expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
  #   #filter(age >= ea) %>% 
  #   mutate(age.r = age - (year - year.r)) %>% 
  #   left_join(mortality.post.model.t5_ %>% select(age.r, age, qxm.post.W)) %>%
  #   mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
  #   arrange(year, year.r, age, ea)
  # 
  # p_la2dead.t6 <- 
  #   expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
  #   #filter(age >= ea) %>% 
  #   mutate(age.r = age - (year - year.r)) %>% 
  #   left_join(mortality.post.model.t6_ %>% select(age.r, age, qxm.post.W)) %>%
  #   mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
  #   arrange(year, year.r, age, ea)

 # identical(p_la2dead.t1, p_la2dead.t6)
  
  
#*************************************************************************************************************
#                                     Creating a function to calculate new entrants ####
#*************************************************************************************************************

# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, dist, no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after  decrement.  
  # delta: growth rate of workforce
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(r.max - 1)
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0*(1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1 # number of workers need to hire
  ne <- size_hire*dist               # vector, number of new entrants by age
  
  # Create the new entrant matrix 
  NE <- wf0; NE[ ,] <- 0
  
  if (no.entrants){ 
    return(NE) 
  } else {
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
  } 
}

# test the function 
# wf0 <- wf_active[, , 1]
# wf1 <- wf_active[, , 1]*(1 - p_active2term)
# sum(wf0, na.rm = T) - sum(wf1, na.rm = T)
# sum(calc_entrants(wf0, wf1, 0), na.rm = T)


#*************************************************************************************************************
#                               Creating a function to calculate new entrants, FOR ALL TIERS ####
#*************************************************************************************************************

# define function for determining the number of new entrants 
calc_entrants_allTiers <- function(wf0.t1,  wf0.t2,  wf0.t3, wf0.t4,  wf0.t5,  wf0.t6, 
                                   wf1.t1,  wf1.t2,  wf1.t3, wf1.t4,  wf1.t5,  wf1.t6,
                                   dist.t1, dist.t2, dist.t3, dist.t4, dist.t5, dist.t6,
                                   newEnt_byTier,
                                   delta, 
                                   no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after  decrement.  
  # delta: growth rate of workforce
  # newEnt_byTier: named vector, proportion of new entrants entering each tier. names must be "t76", "t13", "tm13"
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(r.max - 1) # FOR ALL TIERS
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0.t1 <- sum(wf0.t1[,as.character(working_age)], na.rm = TRUE)
  size1.t1 <- sum(wf1.t1[,as.character(working_age)], na.rm = TRUE)
  
  size0.t2 <- sum(wf0.t2[,as.character(working_age)], na.rm = TRUE)
  size1.t2 <- sum(wf1.t2[,as.character(working_age)], na.rm = TRUE)
  
  size0.t3 <- sum(wf0.t3[,as.character(working_age)], na.rm = TRUE)
  size1.t3 <- sum(wf1.t3[,as.character(working_age)], na.rm = TRUE)
  
  size0.t4 <- sum(wf0.t4[,as.character(working_age)], na.rm = TRUE)
  size1.t4 <- sum(wf1.t4[,as.character(working_age)], na.rm = TRUE)
  
  size0.t5 <- sum(wf0.t5[,as.character(working_age)], na.rm = TRUE)
  size1.t5 <- sum(wf1.t5[,as.character(working_age)], na.rm = TRUE)
  
  size0.t6 <- sum(wf0.t6[,as.character(working_age)], na.rm = TRUE)
  size1.t6 <- sum(wf1.t6[,as.character(working_age)], na.rm = TRUE)
  
  
  # computing new entrants
  size_target <- (size0.t1 + size0.t2 + size0.t3 + size0.t4 + size0.t5 + size0.t6) * (1 + delta)   # size of the workforce next year
  size_hire   <- size_target - (size1.t1 + size1.t2 + size1.t3 + size1.t4 + size1.t5 + size1.t6)  # number of workers need to hire
  
  ne.t1  <- size_hire * newEnt_byTier["t1"]  * dist.t1 # vector, number of new entrants by age
  ne.t2  <- size_hire * newEnt_byTier["t2"]  * dist.t2 
  ne.t3  <- size_hire * newEnt_byTier["t3"]  * dist.t3 
  ne.t4  <- size_hire * newEnt_byTier["t4"]  * dist.t4 
  ne.t5  <- size_hire * newEnt_byTier["t5"]  * dist.t5 
  ne.t6  <- size_hire * newEnt_byTier["t6"]  * dist.t6 
  
  
  
  # Create the new entrant matrix 
  NE.t1 <- wf0.t1  ;  NE.t1[ ,]  <- 0
  NE.t2 <- wf0.t2  ;  NE.t2[ ,]  <- 0
  NE.t3 <- wf0.t3  ;  NE.t3[ ,]  <- 0
  NE.t4 <- wf0.t4  ;  NE.t4[ ,]  <- 0
  NE.t5 <- wf0.t5  ;  NE.t5[ ,]  <- 0
  NE.t6 <- wf0.t6  ;  NE.t6[ ,]  <- 0
  
  if (no.entrants){ 
    return(NE = list(NE.t1 = NE.t1, NE.t2 = NE.t2, NE.t3 = NE.t3, NE.t4 = NE.t4, NE.t5 = NE.t5, NE.t6 = NE.t6)) 
  
  } else {
    NE.t1[, rownames(NE.t1)] <- diag(ne.t1) # place ne on the matrix of new entrants
    NE.t2[, rownames(NE.t2)] <- diag(ne.t2)
    NE.t3[, rownames(NE.t3)] <- diag(ne.t3)
    NE.t4[, rownames(NE.t4)] <- diag(ne.t4)
    NE.t5[, rownames(NE.t5)] <- diag(ne.t5)
    NE.t6[, rownames(NE.t6)] <- diag(ne.t6)
    return(NE = list(NE.t1 = NE.t1, NE.t2 = NE.t2, NE.t3 = NE.t3, NE.t4 = NE.t4, NE.t5 = NE.t5, NE.t6 = NE.t6)) 
  } 
}  

# # test the function 
#   wf0.t76  <- wf_active.t76[, , 1]
#   wf0.t13  <- wf_active.t13[, , 1]
#   wf0.tm13 <- wf_active.tm13[, , 1]
#   
#   wf1.t76  <- wf_active.t76[, , 1] *(1 - p_active2term.t76)
#   wf1.t13  <- wf_active.t13[, , 1] *(1 - p_active2term.t13)
#   wf1.tm13 <- wf_active.tm13[, , 1]*(1 - p_active2term.tm13)
#   
#   (sum(wf0.t76 + wf0.t13 + wf0.tm13, na.rm = T) - sum(wf1.t76 + wf1.t13 + wf1.tm13, na.rm = T))
#   
#   NE <- calc_entrants_allTiers(wf0.t76, wf0.t13, wf0.tm13,
#                              wf1.t76,  wf1.t13,  wf1.tm13, 
#                              entrants_dist.t76, entrants_dist.t13, entrants_dist.tm13,
#                              c(t76 = 0, t13 = 0.8, tm13 = 0.2), 0)
#   sapply(NE, sum) %>% sum
#   
  
  


#*************************************************************************************************************
#                                     Simulating the evolution of population  ####
#*************************************************************************************************************


# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
# A square matrix with the dimension length(range_age)
# created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 



# Now the next slice of the array (array[, , i + 1]) is defined
# wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
# i runs from 2 to nyear. 

for (j in 1:(nyear - 1)){
     # j <-  1  
  
  #*******************************************
  # Stage 1 Seperations by type in each tier *
  #*******************************************
  
  ## Tier 1
    # compute the inflow to and outflow
    active2term.t1    <- wf_active.t1[, , j] * p_active2term.t1  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.t1    <- wf_active.t1[, , j] * p_active2dead.t1
    active2retired.t1 <- wf_active.t1[, , j] * p_active2retired.t1    # This will be used to calculate the number of actives leaving the workforce
    active2la.t1      <- wf_active.t1[, , j] * p_active2la.t1          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.t1<- wf_active.t1[, , j] * p_active2deathBen.t1
    active2disb.t1    <- wf_active.t1[, , j] * p_active2disb.t1
    active2disb.la.t1 <- wf_active.t1[, , j] * p_active2disb.la.t1
    
    # Where do the terminated_vested go
    term2dead.t1  <- wf_term.t1[, , j, ] * as.vector(p_term2dead.t1)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.t1   <- wf_la.t1[, , j, ] * (p_la2dead.t1 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
    

    # Where do the QSSs of death benefit go
    deathBen2dead.t1  <- wf_deathBen.t1[, , j, ] * as.vector(p_deathBen2dead.t1)
    
    # Where do the disability retirees go
    disb.la2dead.t1  <- wf_disb.la.t1[, , j, ] * as.vector(p_disb.la2dead.t1)
    
    
  ## Tier 2
    # compute the inflow to and outflow
    active2term.t2    <- wf_active.t2[, , j] * p_active2term.t2  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.t2    <- wf_active.t2[, , j] * p_active2dead.t2
    active2retired.t2 <- wf_active.t2[, , j] * p_active2retired.t2    # This will be used to calculate the number of actives leaving the workforce
    active2la.t2      <- wf_active.t2[, , j] * p_active2la.t2          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.t2<- wf_active.t2[, , j] * p_active2deathBen.t2
    active2disb.t2    <- wf_active.t2[, , j] * p_active2disb.t2
    active2disb.la.t2 <- wf_active.t2[, , j] * p_active2disb.la.t2
    
    # Where do the terminated_vested go
    term2dead.t2  <- wf_term.t2[, , j, ] * as.vector(p_term2dead.t2)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.t2   <- wf_la.t2[, , j, ] * (p_la2dead.t2 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
    
 
    # Where do the QSSs of death benefit go
    deathBen2dead.t2  <- wf_deathBen.t2[, , j, ] * as.vector(p_deathBen2dead.t2)
    
    # Where do the disability retirees go
    disb.la2dead.t2  <- wf_disb.la.t2[, , j, ] * as.vector(p_disb.la2dead.t2)
    
  ## Tier 3
    # compute the inflow to and outflow
    active2term.t3    <- wf_active.t3[, , j] * p_active2term.t3  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.t3    <- wf_active.t3[, , j] * p_active2dead.t3
    active2retired.t3 <- wf_active.t3[, , j] * p_active2retired.t3    # This will be used to calculate the number of actives leaving the workforce
    active2la.t3      <- wf_active.t3[, , j] * p_active2la.t3          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.t3<- wf_active.t3[, , j] * p_active2deathBen.t3
    active2disb.t3    <- wf_active.t3[, , j] * p_active2disb.t3
    active2disb.la.t3 <- wf_active.t3[, , j] * p_active2disb.la.t3
    
    # Where do the terminated_vested go
    term2dead.t3  <- wf_term.t3[, , j, ] * as.vector(p_term2dead.t3)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.t3   <- wf_la.t3[, , j, ] * (p_la2dead.t3 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
    
    # Where do the QSSs of death benefit go
    deathBen2dead.t3  <- wf_deathBen.t3[, , j, ] * as.vector(p_deathBen2dead.t3)
    
    # Where do the disability retirees go
    disb.la2dead.t3  <- wf_disb.la.t3[, , j, ] * as.vector(p_disb.la2dead.t3)
  
  
  ## Tier 4
    # compute the inflow to and outflow
    active2term.t4    <- wf_active.t4[, , j] * p_active2term.t4  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.t4    <- wf_active.t4[, , j] * p_active2dead.t4
    active2retired.t4 <- wf_active.t4[, , j] * p_active2retired.t4    # This will be used to calculate the number of actives leaving the workforce
    active2la.t4      <- wf_active.t4[, , j] * p_active2la.t4          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.t4<- wf_active.t4[, , j] * p_active2deathBen.t4
    active2disb.t4    <- wf_active.t4[, , j] * p_active2disb.t4
    active2disb.la.t4 <- wf_active.t4[, , j] * p_active2disb.la.t4
    
    # Where do the terminated_vested go
    term2dead.t4  <- wf_term.t4[, , j, ] * as.vector(p_term2dead.t4)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.t4   <- wf_la.t4[, , j, ] * (p_la2dead.t4 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
    
  
    # Where do the QSSs of death benefit go
    deathBen2dead.t4  <- wf_deathBen.t4[, , j, ] * as.vector(p_deathBen2dead.t4)
    
    # Where do the disability retirees go
    disb.la2dead.t4  <- wf_disb.la.t4[, , j, ] * as.vector(p_disb.la2dead.t4)
    
  ## Tier 5
    # compute the inflow to and outflow
    active2term.t5    <- wf_active.t5[, , j] * p_active2term.t5  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.t5    <- wf_active.t5[, , j] * p_active2dead.t5
    active2retired.t5 <- wf_active.t5[, , j] * p_active2retired.t5    # This will be used to calculate the number of actives leaving the workforce
    active2la.t5      <- wf_active.t5[, , j] * p_active2la.t5          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.t5<- wf_active.t5[, , j] * p_active2deathBen.t5
    active2disb.t5    <- wf_active.t5[, , j] * p_active2disb.t5
    active2disb.la.t5 <- wf_active.t5[, , j] * p_active2disb.la.t5
    
    # Where do the terminated_vested go
    term2dead.t5  <- wf_term.t5[, , j, ] * as.vector(p_term2dead.t5)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.t5   <- wf_la.t5[, , j, ] * (p_la2dead.t5 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  
    # Where do the QSSs of death benefit go
    deathBen2dead.t5  <- wf_deathBen.t5[, , j, ] * as.vector(p_deathBen2dead.t5)
    
    # Where do the disability retirees go
    disb.la2dead.t5  <- wf_disb.la.t5[, , j, ] * as.vector(p_disb.la2dead.t5)
    
  ## Tier 6
    # compute the inflow to and outflow
    active2term.t6    <- wf_active.t6[, , j] * p_active2term.t6  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.t6    <- wf_active.t6[, , j] * p_active2dead.t6
    active2retired.t6 <- wf_active.t6[, , j] * p_active2retired.t6    # This will be used to calculate the number of actives leaving the workforce
    active2la.t6      <- wf_active.t6[, , j] * p_active2la.t6          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.t6<- wf_active.t6[, , j] * p_active2deathBen.t6
    active2disb.t6    <- wf_active.t6[, , j] * p_active2disb.t6
    active2disb.la.t6 <- wf_active.t6[, , j] * p_active2disb.la.t6
    
    # Where do the terminated_vested go
    term2dead.t6  <- wf_term.t6[, , j, ] * as.vector(p_term2dead.t6)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.t6   <- wf_la.t6[, , j, ] * (p_la2dead.t6 %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
  
    # Where do the QSSs of death benefit go
    deathBen2dead.t6  <- wf_deathBen.t6[, , j, ] * as.vector(p_deathBen2dead.t6)
  
    # Where do the disability retirees go
    disb.la2dead.t6  <- wf_disb.la.t6[, , j, ] * as.vector(p_disb.la2dead.t6)

  #***********************************
  # Stage 2 Seperations in each tier *
  #***********************************
  
  ### Total inflow and outflow for each status
  
  # Calculating new entrants
  out_active.t1   <- active2term.t1 + active2disb.t1 + active2retired.t1 + active2dead.t1 
  out_active.t2   <- active2term.t2 + active2disb.t2 + active2retired.t2 + active2dead.t2
  out_active.t3   <- active2term.t3 + active2disb.t3 + active2retired.t3 + active2dead.t3
  out_active.t4   <- active2term.t4 + active2disb.t4 + active2retired.t4 + active2dead.t4 
  out_active.t5   <- active2term.t5 + active2disb.t5 + active2retired.t5 + active2dead.t5
  out_active.t6   <- active2term.t6 + active2disb.t6 + active2retired.t6 + active2dead.t6
  
  
  new_entrants_allTiers <- 
  calc_entrants_allTiers(wf_active.t1[, , j], 
                         wf_active.t2[, , j], 
                         wf_active.t3[, , j], 
                         wf_active.t4[, , j], 
                         wf_active.t5[, , j], 
                         wf_active.t6[, , j],
                         
                         
                         wf_active.t1[, , j]  - out_active.t1, 
                         wf_active.t2[, , j]  - out_active.t2,
                         wf_active.t3[, , j]  - out_active.t3,
                         wf_active.t4[, , j]  - out_active.t4, 
                         wf_active.t5[, , j]  - out_active.t5,
                         wf_active.t6[, , j]  - out_active.t6,
                         
                
                         entrants_dist.t1_,
                         entrants_dist.t2_,
                         entrants_dist.t3_,
                         entrants_dist.t4_,
                         entrants_dist.t5_,
                         entrants_dist.t6_,

                         
                         newEnt_byTier,
                         
                         wf_growth,
                         
                         no.entrants = no_entrance) # new entrants
  
  # 
  # sum(out_active.t76) + 
  # sum(out_active.t13) +
  # sum(out_active.tm13)
  # 
  # sum(new_entrants_allTiers$NE.t76) + 
  # sum(new_entrants_allTiers$NE.t13) +
  # sum(new_entrants_allTiers$NE.tm13)
  # 
           
  # new_entrants_allTiers %>% names
  # new_entrants <- calc_entrants(wf_active[, , j], wf_active[, , j] - out_active, wf_growth, dist = .entrants_dist, no.entrants = no_entrance) # new entrants
  
  
  ## Tier 1
    out_term.t1 <- term2dead.t1    # This is a 3D array 
    in_term.t1  <- active2term.t1  # This is a matrix
    
    out_la.t1 <- la2dead.t1        # This is a 3D array (ea x age x year.retire)
    in_la.t1  <- active2la.t1      # This is a matrix
    
    out_deathBen.t1 <- deathBen2dead.t1        # This is a 3D array (ea x age x year.retire)
    in_deathBen.t1  <- active2deathBen.t1    # This is a matrix
    
    out_disb.la.t1 <- disb.la2dead.t1        # This is a 3D array (ea x age x year.retire)
    in_disb.la.t1  <- active2disb.la.t1    # This is a matrix
    
    
    in_dead.t1 <- active2dead.t1 +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.t1, c(1,2), sum) + apply(la2dead.t1, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.t1, c(1,2), sum)  

    
  ## Tier 2
    out_term.t2 <- term2dead.t2    # This is a 3D array 
    in_term.t2  <- active2term.t2  # This is a matrix
    
    out_la.t2 <- la2dead.t2        # This is a 3D array (ea x age x year.retire)
    in_la.t2  <- active2la.t2      # This is a matrix
    
    out_deathBen.t2 <- deathBen2dead.t2        # This is a 3D array (ea x age x year.retire)
    in_deathBen.t2  <- active2deathBen.t2    # This is a matrix
    
    out_disb.la.t2 <- disb.la2dead.t2        # This is a 3D array (ea x age x year.retire)
    in_disb.la.t2  <- active2disb.la.t2    # This is a matrix
    
    in_dead.t2 <- active2dead.t2 +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.t2, c(1,2), sum) + apply(la2dead.t2, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.t2, c(1,2), sum) 
  
  ## Tier 3
    out_term.t3 <- term2dead.t3    # This is a 3D array 
    in_term.t3  <- active2term.t3  # This is a matrix
    
    out_la.t3 <- la2dead.t3        # This is a 3D array (ea x age x year.retire)
    in_la.t3  <- active2la.t3      # This is a matrix
    
    out_deathBen.t3 <- deathBen2dead.t3        # This is a 3D array (ea x age x year.retire)
    in_deathBen.t3  <- active2deathBen.t3    # This is a matrix
    
    out_disb.la.t3 <- disb.la2dead.t3        # This is a 3D array (ea x age x year.retire)
    in_disb.la.t3  <- active2disb.la.t3    # This is a matrix
    
    in_dead.t3 <- active2dead.t3 +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.t3, c(1,2), sum) + apply(la2dead.t3, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.t3, c(1,2), sum)
    
    
  ## Tier 4
    out_term.t4 <- term2dead.t4    # This is a 3D array 
    in_term.t4  <- active2term.t4  # This is a matrix
    
    out_la.t4 <- la2dead.t4        # This is a 3D array (ea x age x year.retire)
    in_la.t4  <- active2la.t4      # This is a matrix
    
    out_deathBen.t4 <- deathBen2dead.t4        # This is a 3D array (ea x age x year.retire)
    in_deathBen.t4  <- active2deathBen.t4    # This is a matrix
    
    out_disb.la.t4 <- disb.la2dead.t4        # This is a 3D array (ea x age x year.retire)
    in_disb.la.t4  <- active2disb.la.t4    # This is a matrix
    
    in_dead.t4 <- active2dead.t4 +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.t4, c(1,2), sum) + apply(la2dead.t4, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.t4, c(1,2), sum)
    
    
  ## Tier 5
    out_term.t5 <- term2dead.t5    # This is a 3D array 
    in_term.t5  <- active2term.t5  # This is a matrix
    
    out_la.t5 <- la2dead.t5        # This is a 3D array (ea x age x year.retire)
    in_la.t5  <- active2la.t5      # This is a matrix
    
    out_deathBen.t5 <- deathBen2dead.t5        # This is a 3D array (ea x age x year.retire)
    in_deathBen.t5  <- active2deathBen.t5    # This is a matrix
    
    out_disb.la.t5 <- disb.la2dead.t5        # This is a 3D array (ea x age x year.retire)
    in_disb.la.t5  <- active2disb.la.t5    # This is a matrix
    
    in_dead.t5 <- active2dead.t5 +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.t5, c(1,2), sum) + apply(la2dead.t5, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.t5, c(1,2), sum)

    
  ## Tier 6
    out_term.t6 <- term2dead.t6    # This is a 3D array 
    in_term.t6  <- active2term.t6  # This is a matrix
    
    out_la.t6 <- la2dead.t6        # This is a 3D array (ea x age x year.retire)
    in_la.t6  <- active2la.t6      # This is a matrix
    
    out_deathBen.t6 <- deathBen2dead.t6        # This is a 3D array (ea x age x year.retire)
    in_deathBen.t6  <- active2deathBen.t6    # This is a matrix
    
    out_disb.la.t6 <- disb.la2dead.t6        # This is a 3D array (ea x age x year.retire)
    in_disb.la.t6  <- active2disb.la.t6    # This is a matrix
    
    in_dead.t6 <- active2dead.t6 +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.t6, c(1,2), sum) + apply(la2dead.t6, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.t6, c(1,2), sum)
    

  
  #*********************************************
  # Stage 3  Calculate workforce for next year. 
  #*********************************************  
  
  ## Tier 1
    wf_active.t1[, , j + 1]  <- (wf_active.t1[, , j] - out_active.t1) %*% A + new_entrants_allTiers$NE.t1
    
    wf_term.t1[, , j + 1, ]      <- apply((wf_term.t1[, , j, ] - out_term.t1), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.t1[, , j + 1, j + 1] <- in_term.t1 %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.t1[, ,j + 1, ]       <- apply((wf_la.t1[, , j, ] - out_la.t1), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.t1[, , j + 1, j + 1] <- in_la.t1 %*% A
    
    wf_dead.t1[, ,   j + 1]    <- (wf_dead.t1[, , j] + in_dead.t1) %*% A
    
    wf_deathBen.t1[, , j + 1, ]      <- apply((wf_deathBen.t1[, , j, ] - out_deathBen.t1), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.t1[, , j + 1, j + 1] <- in_deathBen.t1 %*% A
    
    wf_disb.la.t1[, , j + 1, ]      <- apply((wf_disb.la.t1[, , j, ] - out_disb.la.t1), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.t1[, , j + 1, j + 1] <- in_disb.la.t1 %*% A
    
    newDeath.act.t1[j]  <- sum(active2dead.t1)
    newDeath.ret.t1[j]  <- sum(la2dead.t1)
    # newDeath.term[j] <- sum()
    
    newDisb.act.t1[j] <- sum(active2disb.t1)
  
  
  
  ## Tier 2
    wf_active.t2[, , j + 1]  <- (wf_active.t2[, , j] - out_active.t2) %*% A + new_entrants_allTiers$NE.t2
    
    wf_term.t2[, , j + 1, ]      <- apply((wf_term.t2[, , j, ] - out_term.t2), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.t2[, , j + 1, j + 1] <- in_term.t2 %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.t2[, ,j + 1, ]       <- apply((wf_la.t2[, , j, ] - out_la.t2), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.t2[, , j + 1, j + 1] <- in_la.t2 %*% A
    
    wf_dead.t2[, ,   j + 1]    <- (wf_dead.t2[, , j] + in_dead.t2) %*% A
    
    wf_deathBen.t2[, , j + 1, ]      <- apply((wf_deathBen.t2[, , j, ] - out_deathBen.t2), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.t2[, , j + 1, j + 1] <- in_deathBen.t2 %*% A
    
    wf_disb.la.t2[, , j + 1, ]      <- apply((wf_disb.la.t2[, , j, ] - out_disb.la.t2), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.t2[, , j + 1, j + 1] <- in_disb.la.t2 %*% A
    
    newDeath.act.t2[j]  <- sum(active2dead.t2)
    newDeath.ret.t2[j]  <- sum(la2dead.t2)
    # newDeath.term[j] <- sum()
    
    newDisb.act.t2[j] <- sum(active2disb.t2)

      
  ## Tier 3
    wf_active.t3[, , j + 1]  <- (wf_active.t3[, , j] - out_active.t3) %*% A + new_entrants_allTiers$NE.t3
    
    wf_term.t3[, , j + 1, ]      <- apply((wf_term.t3[, , j, ] - out_term.t3), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.t3[, , j + 1, j + 1] <- in_term.t3 %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.t3[, ,j + 1, ]       <- apply((wf_la.t3[, , j, ] - out_la.t3), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.t3[, , j + 1, j + 1] <- in_la.t3 %*% A
    
    wf_dead.t3[, ,   j + 1]    <- (wf_dead.t3[, , j] + in_dead.t3) %*% A
    
    wf_deathBen.t3[, , j + 1, ]      <- apply((wf_deathBen.t3[, , j, ] - out_deathBen.t3), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.t3[, , j + 1, j + 1] <- in_deathBen.t3 %*% A
    
    wf_disb.la.t3[, , j + 1, ]      <- apply((wf_disb.la.t3[, , j, ] - out_disb.la.t3), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.t3[, , j + 1, j + 1] <- in_disb.la.t3 %*% A
    
    newDeath.act.t3[j]  <- sum(active2dead.t3)
    newDeath.ret.t3[j]  <- sum(la2dead.t3)
    # newDeath.term[j] <- sum()
    
    newDisb.act.t3[j] <- sum(active2disb.t3)
  
    
  ## Tier 4
    wf_active.t4[, , j + 1]  <- (wf_active.t4[, , j] - out_active.t4) %*% A + new_entrants_allTiers$NE.t4
    
    wf_term.t4[, , j + 1, ]      <- apply((wf_term.t4[, , j, ] - out_term.t4), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.t4[, , j + 1, j + 1] <- in_term.t4 %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.t4[, ,j + 1, ]       <- apply((wf_la.t4[, , j, ] - out_la.t4), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.t4[, , j + 1, j + 1] <- in_la.t4 %*% A
    
    wf_dead.t4[, ,   j + 1]    <- (wf_dead.t4[, , j] + in_dead.t4) %*% A
    
    wf_deathBen.t4[, , j + 1, ]      <- apply((wf_deathBen.t4[, , j, ] - out_deathBen.t4), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.t4[, , j + 1, j + 1] <- in_deathBen.t4 %*% A
    
    wf_disb.la.t4[, , j + 1, ]      <- apply((wf_disb.la.t4[, , j, ] - out_disb.la.t4), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.t4[, , j + 1, j + 1] <- in_disb.la.t4 %*% A
    
    newDeath.act.t4[j]  <- sum(active2dead.t4)
    newDeath.ret.t4[j]  <- sum(la2dead.t4)
    # newDeath.term[j] <- sum()
    
    newDisb.act.t4[j] <- sum(active2disb.t4)
    
    
  ## Tier 5
    wf_active.t5[, , j + 1]  <- (wf_active.t5[, , j] - out_active.t5) %*% A + new_entrants_allTiers$NE.t5
    
    wf_term.t5[, , j + 1, ]      <- apply((wf_term.t5[, , j, ] - out_term.t5), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.t5[, , j + 1, j + 1] <- in_term.t5 %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.t5[, ,j + 1, ]       <- apply((wf_la.t5[, , j, ] - out_la.t5), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.t5[, , j + 1, j + 1] <- in_la.t5 %*% A
    
    wf_dead.t5[, ,   j + 1]    <- (wf_dead.t5[, , j] + in_dead.t5) %*% A
    
    wf_deathBen.t5[, , j + 1, ]      <- apply((wf_deathBen.t5[, , j, ] - out_deathBen.t5), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.t5[, , j + 1, j + 1] <- in_deathBen.t5 %*% A
    
    wf_disb.la.t5[, , j + 1, ]      <- apply((wf_disb.la.t5[, , j, ] - out_disb.la.t5), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.t5[, , j + 1, j + 1] <- in_disb.la.t5 %*% A
    
    newDeath.act.t5[j]  <- sum(active2dead.t5)
    newDeath.ret.t5[j]  <- sum(la2dead.t5)
    # newDeath.term[j] <- sum()
    
    newDisb.act.t5[j] <- sum(active2disb.t5)
  
      
  ## Tier 6
    wf_active.t6[, , j + 1]  <- (wf_active.t6[, , j] - out_active.t6) %*% A + new_entrants_allTiers$NE.t6
    
    wf_term.t6[, , j + 1, ]      <- apply((wf_term.t6[, , j, ] - out_term.t6), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.t6[, , j + 1, j + 1] <- in_term.t6 %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.t6[, ,j + 1, ]       <- apply((wf_la.t6[, , j, ] - out_la.t6), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.t6[, , j + 1, j + 1] <- in_la.t6 %*% A
    
    wf_dead.t6[, ,   j + 1]    <- (wf_dead.t6[, , j] + in_dead.t6) %*% A
    
    wf_deathBen.t6[, , j + 1, ]      <- apply((wf_deathBen.t6[, , j, ] - out_deathBen.t6), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.t6[, , j + 1, j + 1] <- in_deathBen.t6 %*% A
    
    wf_disb.la.t6[, , j + 1, ]      <- apply((wf_disb.la.t6[, , j, ] - out_disb.la.t6), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.t6[, , j + 1, j + 1] <- in_disb.la.t6 %*% A
    
    newDeath.act.t6[j]  <- sum(active2dead.t6)
    newDeath.ret.t6[j]  <- sum(la2dead.t6)
    # newDeath.term[j] <- sum()
    
    newDisb.act.t6[j] <- sum(active2disb.t6)


}



#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************

## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

get_df.wf_active <- function(df){
    df <- adply(df, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
    rename(year = X1) %>%
    gather(age, number.a, -ea, -year) %>% 
    mutate(year = f2n(year), age = as.numeric(age)) %>% 
    filter(age >= ea)
}
 wf_active.t1 <- get_df.wf_active(wf_active.t1)
 wf_active.t2 <- get_df.wf_active(wf_active.t2)
 wf_active.t3 <- get_df.wf_active(wf_active.t3)
 wf_active.t4 <- get_df.wf_active(wf_active.t4)
 wf_active.t5 <- get_df.wf_active(wf_active.t5)
 wf_active.t6 <- get_df.wf_active(wf_active.t6)

 
get_df.wf_la <- function(df){
  df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)),
                   number.la = as.vector(df)) %>% 
    filter(age >= ea)
}
 wf_la.t1  <- get_df.wf_la(wf_la.t1)
 wf_la.t2  <- get_df.wf_la(wf_la.t2)
 wf_la.t3  <- get_df.wf_la(wf_la.t3)
 wf_la.t4  <- get_df.wf_la(wf_la.t4)
 wf_la.t5  <- get_df.wf_la(wf_la.t5)
 wf_la.t6  <- get_df.wf_la(wf_la.t6)


get_df.wf_term <- function(df){
    df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.term = (init.year-1):(init.year + nyear - 1)),
                     number.v = as.vector(df)) %>% 
    filter(age >= ea)
}
 wf_term.t1  <- get_df.wf_term(wf_term.t1)
 wf_term.t2  <- get_df.wf_term(wf_term.t2)
 wf_term.t3  <- get_df.wf_term(wf_term.t3)
 wf_term.t4  <- get_df.wf_term(wf_term.t4)
 wf_term.t5  <- get_df.wf_term(wf_term.t5)
 wf_term.t6  <- get_df.wf_term(wf_term.t6)

 
get_df.wf_deathBen <- function(df){
   df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.death = init.year:(init.year + nyear - 1)),
                    number.deathBen = as.vector(df)) %>% 
     filter(age >= ea)
 }
 wf_deathBen.t1  <- get_df.wf_deathBen(wf_deathBen.t1)
 wf_deathBen.t2  <- get_df.wf_deathBen(wf_deathBen.t2)
 wf_deathBen.t3  <- get_df.wf_deathBen(wf_deathBen.t3)
 wf_deathBen.t4  <- get_df.wf_deathBen(wf_deathBen.t4)
 wf_deathBen.t5  <- get_df.wf_deathBen(wf_deathBen.t5)
 wf_deathBen.t6  <- get_df.wf_deathBen(wf_deathBen.t6)
 
 
get_df.wf_disb.la <- function(df){
   df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.disb = init.year:(init.year + nyear - 1)),
                    number.disb.la = as.vector(df)) %>% 
     filter(age >= ea)
 }
 wf_disb.la.t1  <- get_df.wf_disb.la(wf_disb.la.t1)
 wf_disb.la.t2  <- get_df.wf_disb.la(wf_disb.la.t2)
 wf_disb.la.t3  <- get_df.wf_disb.la(wf_disb.la.t3)
 wf_disb.la.t4  <- get_df.wf_disb.la(wf_disb.la.t4)
 wf_disb.la.t5  <- get_df.wf_disb.la(wf_disb.la.t5)
 wf_disb.la.t6  <- get_df.wf_disb.la(wf_disb.la.t6)
 
 
 
 # wf_deathBen.t1 %>% filter(number.deathBen !=0)

# summarize term across termination year. Resulting data frame will join .Liab$active as part of the output. 
# term_reduced <- wf_term %>% group_by(year, age) %>% summarise(number.v = sum(number.v, na.rm = TRUE))




#*************************************************************************************************************
#                                     Number of members opting for contingent annuity   ####
#*************************************************************************************************************

get_wf_new.ca <- function(df_actives, decrement){
  wf_new.ca <- df_actives %>% left_join(decrement %>% select(age, ea, qxr.ca)) %>% 
               mutate(new_ca  = number.a * qxr.ca,
                      year = year + 1,
                      age  = age + 1)
}

wf_new.ca.t1  <- get_wf_new.ca(wf_active.t1, decrement_wf.t1)
wf_new.ca.t2  <- get_wf_new.ca(wf_active.t2, decrement_wf.t2)
wf_new.ca.t3  <- get_wf_new.ca(wf_active.t3, decrement_wf.t3)
wf_new.ca.t4  <- get_wf_new.ca(wf_active.t4, decrement_wf.t4)
wf_new.ca.t5  <- get_wf_new.ca(wf_active.t5, decrement_wf.t5)
wf_new.ca.t6  <- get_wf_new.ca(wf_active.t6, decrement_wf.t6)


get_wf_new.disb.ca <- function(df_actives, decrement){
  wf_new.disb.ca <- df_actives %>% left_join(decrement %>% select(age, ea, qxd.ca)) %>% 
    mutate(new_disb.ca  = number.a * qxd.ca,
           year = year + 1,
           age  = age + 1)
}

wf_new.disb.ca.t1  <- get_wf_new.disb.ca(wf_active.t1, decrement_wf.t1)
wf_new.disb.ca.t2  <- get_wf_new.disb.ca(wf_active.t2, decrement_wf.t2)
wf_new.disb.ca.t3  <- get_wf_new.disb.ca(wf_active.t3, decrement_wf.t3)
wf_new.disb.ca.t4  <- get_wf_new.disb.ca(wf_active.t4, decrement_wf.t4)
wf_new.disb.ca.t5  <- get_wf_new.disb.ca(wf_active.t5, decrement_wf.t5)
wf_new.disb.ca.t6  <- get_wf_new.disb.ca(wf_active.t6, decrement_wf.t6)



# Final outputs

pop <- list(   
     pop.t1 = list(active  = wf_active.t1,  term = wf_term.t1, disb.la = wf_disb.la.t1, la = wf_la.t1, deathBen = wf_deathBen.t1, dead = wf_dead.t1, new_ca = wf_new.ca.t1, new_disb.ca = wf_new.disb.ca.t1),
     pop.t2 = list(active  = wf_active.t2,  term = wf_term.t2, disb.la = wf_disb.la.t2, la = wf_la.t2, deathBen = wf_deathBen.t2, dead = wf_dead.t2, new_ca = wf_new.ca.t2, new_disb.ca = wf_new.disb.ca.t2),
     pop.t3 = list(active  = wf_active.t3,  term = wf_term.t3, disb.la = wf_disb.la.t3, la = wf_la.t3, deathBen = wf_deathBen.t3, dead = wf_dead.t3, new_ca = wf_new.ca.t3, new_disb.ca = wf_new.disb.ca.t3),
     pop.t4 = list(active  = wf_active.t4,  term = wf_term.t4, disb.la = wf_disb.la.t4, la = wf_la.t4, deathBen = wf_deathBen.t4, dead = wf_dead.t4, new_ca = wf_new.ca.t4, new_disb.ca = wf_new.disb.ca.t4),
     pop.t5 = list(active  = wf_active.t5,  term = wf_term.t5, disb.la = wf_disb.la.t5, la = wf_la.t5, deathBen = wf_deathBen.t5, dead = wf_dead.t5, new_ca = wf_new.ca.t5, new_disb.ca = wf_new.disb.ca.t5),
     pop.t6 = list(active  = wf_active.t6,  term = wf_term.t6, disb.la = wf_disb.la.t6, la = wf_la.t6, deathBen = wf_deathBen.t6, dead = wf_dead.t6, new_ca = wf_new.ca.t6, new_disb.ca = wf_new.disb.ca.t6)
)

return(pop)

}


#pop <- get_Population_allTiers()





# pop$term %>% filter(year == 2016) %>% select(number.v) %>% sum



# # Spot check the results
# wf_active %>% group_by(year) %>% summarise(n = sum(number.a)) %>% mutate(x = n == 1000) %>% data.frame # OK
# wf_active %>% filter(year == 2025) %>% spread(age, number.a)
# 
# 
# wf_la %>% group_by(year) %>% summarise(n = sum(number.la)) %>% data.frame  
# 
# wf_la %>% filter(year.r == 2016, year == 2018, age==65) %>% mutate(number.la_next = number.la * 0.9945992) %>% 
#   left_join(wf_la %>% filter(year.r == 2016, year == 2019, age==66) %>% select(year.r, ea, number.la_true = number.la)) %>% 
#   mutate(diff = number.la_true - number.la_next) # looks ok.
# 
# mortality.post.ucrp %>% filter(age.r == 63)
# 
# 
# 
# 
# # check retirement
# wf_active %>% filter(year == 2020, ea == 30) %>% select(-year) %>% 
# left_join(wf_la     %>% filter(year == 2021, year.r == 2021, ea == 30)) %>% 
# left_join(wf_LSC.ca %>% filter(year == 2021, ea == 30) %>% select(year, ea, age, new_LSC, new_ca)) %>% 
# left_join(decrement_wf %>% filter(ea == 30) %>% select(ea, age, qxr, qxr.la, qxr.ca, qxr.LSC)) %>% 
# filter(age >= 49 & age <=75) %>% 
# mutate(diff.la = lag(number.a *qxr.la) - number.la,
#        diff.ca = lag(number.a *qxr.ca) - new_ca,
#        diff.LSC= lag(number.a *qxr.LSC) - new_LSC,
#        diff.r  = lag(number.a *qxr) - (new_ca + new_LSC + number.la))
#   # looks ok.
#





# wf_active %>% group_by(year) %>% summarise(n = sum(number.a))
# 
# wf_active %>% head
# 






