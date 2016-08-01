
# Inputs:
  # "Data_inputs/LAFPP_PlanInfo.RData", for decrements
  # "Data_inputs/LAFPP_MemberData.RData", for gender and occupation ratios


# Final outputs
 # 1. decrement.model
 # 2. mortality.post.model


get_decrements <- function(Tier_select,
                           .Global_paramlist = Global_paramlist,
                           .paramlist = paramlist){

# Tier_select <- "t1"
# 
# .Global_paramlist = Global_paramlist
# .paramlist = paramlist

assign_parmsList(.Global_paramlist, envir = environment())
assign_parmsList(.paramlist,        envir = environment())



#*************************************************************************************************************
#                                Prepare mortality tables for UCRP                        #####                  
#*************************************************************************************************************

#Note on qxm.deathBen
  # qxm.deathBen applies to beneficiaires of benenfits for death before retirement.
  # For now (Jun 26, 2016), qxm.deathBen is a weighted average of mortalities of survivors.
  # Weights are constant over time: 95% female survivors and 5% male survivors. For now, we do not take into 
    # account the effect difference in mortality of males and females on gender ratio over time.  
  # Wives are 3 years younger than husbands. 

mortality.model <- data.frame(age = range_age) %>% 
  left_join(mortality_MISERS) %>% 
  mutate(qxm.pre = qxm.pre.male  * pct.male + qxm.pre.female  * pct.female,   # mortality for actives
         qxm.d   = (qxm.d.male    * pct.male + qxm.d.female  * pct.female) * 1,
         
         qxm.deathBen = ifelse(age > max(age - 3), 1, lead(qxm.post.female, 3)) * pct.male + 
                        ifelse(age < min(age + 3), qxm.post.male[age == min(age)],lag(qxm.post.male, 3)) * pct.female,
         qxm.deathBen = ifelse(age == max(age), 1, qxm.deathBen)
         ) %>% 
  select(age, qxm.pre, qxm.post.male, qxm.post.female, qxm.d, qxm.deathBen)
 mortality.model



## Compute present values of life annuity(with cola) at each retirement age, using uni-sex mortality with age dependent weights
  # Why using age dependent weights:
    # Using the age dependent weights, the number of total members calculated using aggregate members and weighted mortality would be the same
    # as that obtained by calculating the members for males and females separately and summing them up. This is required by the the actuarially 
    # equivalence between life annuity and contingent annuity. 
 

mortality.post.model <- expand.grid(age = range_age, 
                                    age.r = min(range_age.r):max.age) %>% 
  left_join(mortality.model) %>%
  filter(age >= age.r) %>% 
  group_by(age.r) %>%  
  mutate(
    pxm.post.male   = 1 - qxm.post.male,
    pxm.post.female = 1 - qxm.post.female,
    
    pRxm.male     = pct.male   * ifelse(age == min(age), 1, lag(cumprod(pxm.post.male))),  # proportion of male left in each year after retirement
    pRxm.female   = pct.female * ifelse(age == min(age), 1, lag(cumprod(pxm.post.female))),# same thing, for female
    
    w.male   = pRxm.male / (pRxm.male + pRxm.female),
    w.female = pRxm.female / (pRxm.male + pRxm.female),
    
    qxm.post.W = qxm.post.male * w.male + qxm.post.female * w.female, # dynamically weighted mortality
    pxm.post.W = 1 - qxm.post.W,
    
    COLA.scale = ifelse(cola.compound, (1 + cola)^(row_number() - 1), 1 + cola * (row_number() - 1)),
    
    B =  COLA.scale,
    ax.r.W     =  get_tla(pxm.post.W, i, COLA.scale),
    liab.la.W = B * ax.r.W    # "la" for life annuity. liability for $1's benefit payment at retirement. 
  )  %>% 
  mutate_each(funs(ifelse(is.nan(.), 0, .))) %>% 
  select(age, qxm.post.W, pxm.post.W, ax.r.W)


# Construct mortality rate for terms: 
 # before r.vben: qxm.pre
 # after r.vben:  qxm.post.W with age.r == r.vben

mortality.model %<>% left_join(mortality.post.model %>% ungroup %>%  
                               filter(age.r == r.vben) %>% 
                               select(age, qxm.post.term = qxm.post.W)) %>% 
                     mutate(qxm.term = ifelse(age < r.vben, qxm.pre, qxm.post.term)) %>% 
                     select(-qxm.post.term)



# disability rate                            
disbrates.model <- disbRates %>%  
  mutate(qxd = qxd.nonduty + qxd.duty) %>% 
  select(age, qxd)



# term rates
termrates.model <- termRates %>% 
  select(ea, age, yos, qxt)
                                


# retirement rates
retrates.model  <- retRates %>% 
  mutate(qxr.t1 = qxr.corrections * prop.occupation["t1", "pct.correction"] + qxr.others * prop.occupation["t1", "pct.others"] + 
                  qxr.conservation * prop.occupation["t1", "pct.conservation"]
         ) %>% 
  select(age, matches(paste0(Tier_select, "$") )) %>% 
  rename_("qxr" = paste0("qxr.", Tier_select))

retrates.model                                     

#*************************************************************************************************************
#                      2. Putting together decrements and calculate surviving rates  ####
#*************************************************************************************************************

# Create decrement table and calculate probability of survival
decrement.model <- expand.grid(age = range_age, ea = range_ea) %>% 
  mutate(yos = age - ea) %>% 
  filter(age >= ea) %>% 
  left_join(mortality.model) %>%                  # mortality 
  left_join(termrates.model)  %>%                 # termination
  left_join(disbrates.model)  %>%                 # disability
  left_join(retrates.model) %>%                   # early retirement
  select(ea, age, everything()) %>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.) %>% 
  group_by(ea) 

decrement.model



## Imposing restrictions 
decrement.model %<>% mutate(
  # 1. Coerce termination rates to 0 when eligible for early retirement or reaching r.full(when we assume terms start to receive benefits). 
  qxt = ifelse((age >= r.age1 & yos >= r.yos1 ) |  # retirement rule 1 
               (age >= r.age2 & yos >= r.yos2 ) |  # retirement rule 2
                age >= r.vben, 0, qxt), 

  # 2. Coerce retirement rates to 0 when age greater than r.max
  # Assume retirement rates applies only when they are applicable (according to Bob North.
  qxr = ifelse((age >= r.age1 & yos >= r.yos1 ) | (age >= r.age2 & yos >= r.yos2), qxr, 0),
  qxr = ifelse(age > r.max, 0, qxr)
)
  
decrement.model



# Adjustment to the decrement table:
  # Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
  # For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
  # which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
  # whether they will die at r.max)      

pct.ca <- pct.ca.M * pct.male + pct.ca.F * pct.female
pct.la <- 1 - pct.ca

decrement.model %<>% group_by(ea) %>%  
  mutate(qxr = ifelse(age == r.max - 1,
                             1 - qxt - qxm.pre - qxd, 
                             lead(qxr) * (1 - qxt - qxm.pre - qxd)), # Total probability of retirement
         qxr.la = ifelse(age == r.max, 0 , qxr * pct.la),  # Prob of opting for life annuity
         qxr.ca = ifelse(age == r.max, 0 , qxr * pct.ca),   # Prob of opting for contingent annuity
         
         qxd.la = ifelse(age == r.max, 0 , qxd * pct.la),  # Prob of opting for life annuity
         qxd.ca = ifelse(age == r.max, 0 , qxd * pct.ca)
         
         # qxd.la = ifelse(age == r.max, 0 , qxd * 1),  # Prob of opting for life annuity
         # qxd.ca = ifelse(age == r.max, 0 , qxd * 0)
         
)   
         

######!!!! need to construct retirement age dependent mortality for life annuitants.
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  #mutate(qxm.la.r   = qxm.r) 



# Calculate various survival probabilities




decrement.model %<>% 
  mutate( pxm.pre = 1 - qxm.pre,
          pxm.deathBen = 1 - qxm.deathBen,
          pxm.d = 1 - qxm.d,
          
          pxT     = 1 - qxt - qxd - qxm.pre - qxr,                            
          
          pxRm        = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm.pre))), # prob of surviving up to r.max, mortality only
          px_r.full_m = order_by(-age, cumprod(ifelse(age >= r.vben, 1, pxm.pre))), # Should be deleted later
          px_r.vben_m = order_by(-age, cumprod(ifelse(age >= r.vben, 1, pxm.pre)))
          
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
  ) %>% 
  mutate_each(funs(na2zero))


list(decrement.model = decrement.model,
     mortality.post.model = mortality.post.model)

}


#get_decrements("t5")












