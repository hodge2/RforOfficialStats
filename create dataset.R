library(haven)

ZZHR62FL <- read_dta("C:/Users/un144/Downloads/ZZHR62DT (1)/ZZHR62FL.DTA")


HR_sample = sample_n(ZZHR62FL,100,replace=FALSE)
## HR_top = top_n(ZZHR62FL,50) # this did not work, maybe too many columns
HR_top = ZZHR62FL[1:50,]

for (i in 1:7654) {attributes(HR_top[[i]]) = attributes(ZZHR62FL[[i]])}

rm(ZZHR62FL)

save(HR_top, file="HR_top.RData")
save(HR_sample, file="HR_sample.RData")


# find vars you want based on labels
HR_vars=matrix(NA,7654,2)
for (i in 1:7654) {HR_vars[i,2] = attributes(HR_top[[i]])$label}
HR_vars[,1] = names(HR_top)[1:7654]
HR_vars[str_detect(HR_vars[,2], "age"),]


save(HR_vars, file="HR_vars.RData")

#create dataset for ages and birthdates of children
gathered_HR1 = HR_top %>% 
  select(hhid, hv008, starts_with("hv105_")) %>%
  rename(interview_date_cmc = hv008) %>%
  gather("member_id","age",-hhid, -interview_date_cmc) %>%
  filter(!is.na(age)) %>%
  separate(member_id, c("var","member_id"), "_") %>%
  mutate(member_id = as.numeric(member_id)) %>%
  arrange(hhid,member_id)

gathered_HR0 = HR_top %>% 
  select(hhid,starts_with("hc0_")) %>%
  gather("child_id","member_id",-hhid) %>%
  filter(!is.na(member_id)) %>%
  mutate(child_id = as.numeric(str_sub(child_id,-2))) %>%
  arrange(hhid,child_id)

gathered_HR2 = HR_top %>% 
  select(hhid,starts_with("hc16_")) %>%
  gather("child_id","birth_day",-hhid) %>%
  filter(!is.na(birth_day)) %>%
  mutate(child_id = as.numeric(str_sub(child_id,-2))) %>%
  arrange(hhid,child_id)

gathered_HR3 = HR_top %>% 
  select(hhid,starts_with("hc30_")) %>%
  gather("child_id","birth_mnth",-hhid) %>%
  filter(!is.na(birth_mnth)) %>%
  mutate(child_id = as.numeric(str_sub(child_id,-2))) %>%
  arrange(hhid,child_id)

gathered_HR4 = HR_top %>% 
  select(hhid,starts_with("hc31_")) %>%
  gather("child_id","birth_yr",-hhid) %>%
  filter(!is.na(birth_yr)) %>%
  mutate(child_id = as.numeric(str_sub(child_id,-2))) %>%
  arrange(hhid,child_id)


HR_top_child_age = full_join(full_join(gathered_HR0,gathered_HR2),full_join(gathered_HR3,gathered_HR4))%>%
  full_join(gathered_HR1) %>%
  select(-member_id,-var) %>%
  filter(!is.na(birth_mnth)) %>%
  arrange(hhid, child_id)


save(HR_top_child_age, file="HR_top_child_age.RData")