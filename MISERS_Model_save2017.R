# Running this script after running the main program saves the 2017 results that are needed to run a simulation starting from 2017.


# 2017 results
 # Salary by age and ea
 # retirement benefit by starting year, ea, ret year, and age
 # disability benefit by starting year, ea, disb year, and age
 # Initial demographics
  # actives by age and ea, 
  # retirees by starting year, ea, 



# Inputs
 # pop
 # liab


# Initial actives for 2017
init_actives_all_2017 <- 
  pop$active %>% filter(year == 2017) %>% 
  left_join(liab$active %>% filter(year == 2017) %>% select(year, ea, age, sx)) %>% 
  filter(age < 75) %>% 
  rename(nactives = number.a,
         salary = sx) %>% 
  mutate(planname = "Actives_t1_AV2014_adj_fillin",
         yos = age - ea,
         nactives = na2zero(nactives),
         salary   = na2zero(salary)) %>% 
  select(planname, age, yos, ea, nactives, salary) %>% 
  arrange(ea, age)


# Initial retirees for 2017
init_retirees.la_all_2017 <-  
  pop$la %>% filter(year == 2017) %>% 
  left_join(liab$la %>% filter(year == 2017) %>% select(year, ea, age, year.r,B.la)) %>% 
  rename(nretirees.la = number.la,
         benefit = B.la) %>%
  group_by(age) %>% 
  mutate(nretirees.la = na2zero(nretirees.la),
         benefit = na2zero(benefit)) %>%
  summarize(benefit      = sum(nretirees.la * benefit)/sum(nretirees.la),
            nretirees.la = sum(nretirees.la)
            ) %>% 
  mutate(planname = "RetBen_t1_CRR2013_fillin") %>% 
  filter(!is.nan(benefit)) %>% 
  select(planname, age, nretirees.la, benefit)


init_retirees.la_all_2017
init_retirees.la_all %>% head



# Initial disabled for 2017
init_disb.la_all_2017 <-  
  pop$disb.la %>% filter(year == 2017) %>% 
  left_join(liab$disb.la %>% filter(year == 2017) %>% select(year, ea, age, year.disb, B.disb.la)) %>% 
  rename(ndisb.la = number.disb.la,
         benefit = B.disb.la) %>%
  group_by(age) %>% 
  mutate(ndisb.la = na2zero(ndisb.la),
         benefit = na2zero(benefit)) %>%
  summarize(benefit  = sum(ndisb.la * benefit)/sum(ndisb.la),
            ndisb.la = sum(ndisb.la)
            ) %>% 
  filter(!is.nan(benefit)) %>% 
  mutate(planname = "Disb_t1_fillin") %>% 
  select(planname, age, ndisb.la, benefit)

init_disb.la_all_2017
init_disb.la_all %>% head



# Initial disabled for 2017
init_disb.la_all_2017 <-  
  pop$disb.la %>% filter(year == 2017) %>% 
  left_join(liab$disb.la %>% filter(year == 2017) %>% select(year, ea, age, year.disb, B.disb.la)) %>% 
  rename(ndisb.la = number.disb.la,
         benefit = B.disb.la) %>%
  group_by(age) %>% 
  mutate(ndisb.la = na2zero(ndisb.la),
         benefit = na2zero(benefit)) %>%
  summarize(benefit  = sum(ndisb.la * benefit)/sum(ndisb.la),
            ndisb.la = sum(ndisb.la)
            ) %>% 
  filter(!is.nan(benefit)) %>% 
  mutate(planname = "Disb_t1_fillin") %>% 
  select(planname, age, ndisb.la, benefit)

init_disb.la_all_2017
init_disb.la_all %>% head


# Initial terms in 2017

init_terms_all_2017 <- 
  pop$term %>% filter(year == 2017, year.term %in% 2015:2016) %>%
  mutate(age.vben = 60, year.vben = year + (60 - age)) %>% 
  left_join(liab$term %>% 
            filter(year.term %in% 2015:2016, age == 60) %>% 
            select(year.term, ea, age.vben = age, year.vben = year, B.v)) %>%  
     rename(nterms = number.v,
            benefit = B.v) %>% 
     filter(nterms !=0) %>% 
     group_by(age) %>% 
     mutate(nterms   = na2zero(nterms),
            benefit  = na2zero(benefit)) %>%
     summarize(benefit  = sum(nterms * benefit)/sum(nterms),
               nterms = sum(nterms)) %>% 
     filter(!is.nan(benefit)) %>% 
  mutate(planname = "Terms_t1_fillin",
         ea = 20) %>% 
  select(planname, age, ea, nterms, benefit)
  
  
init_terms_all_2017
init_terms_all %>% head()


# save(liab, pop, file = "save2017_model2015data.RData")
load("save2017_model2015data.RData")

save(init_actives_all_2017, 
     init_retirees.la_all_2017, 
     init_disb.la_all_2017, 
     init_terms_all_2017,
     file = "save2017_closed.RData")




