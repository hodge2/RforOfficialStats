age_group <- function(age){
  group = rep("",length(age))
  group[age<6] = "less than 5"
  group[age>=6 & age<11] <- "6 - 10"
  group[age>=11 & age<21] <- "11 - 20" 
  group[age>=21 & age<31] <- "21 - 30" 
  group[age>=31 & age<41] <- "31 - 40" 
  group[age>=41] <- "over 40" 
  return(group)
}

library(tidyverse)

HR_top = rename(HR_top, hh_age = hv220)
HR_top = mutate(HR_top , hh_age_code = age_group(hh_age))
HR_top$hh_age_code

The example dataset contains a second set of  In the next example the end goal is to create a new dataset with one row for each individual and four columns as follows `HH_number` a unique number that identifies the household, `member_id` a unique id within each household that identifies the member, `relationship2head` the relationship to the head of household, `member_age` the age of the individual. We create this dataset with a series of gathers and spreads. 

The first step is to gather both the column beginning with hv101 those beginning with hv105. The result is a dataset where one column that has a mixture of ages and and relationships. You can think of this as over gathering. 

```{r reshape1, exercise=TRUE, exercise.lines = 20}
gathered_HR2 = HR_top %>% 
  rename(HH_number = hv009) %>%
  select(HH_number,starts_with("hv105_"),starts_with("hv101_")) %>%
  gather("member_id","member_age",starts_with("hv105_")) %>%
  gather("member_id","member_age",starts_with("hv105_"))
View(gather_HR2)
```


drinking_h20 <- data.frame(
  urban_rural = as.Date('2009-01-01') + 0:9,
  h20_location = rnorm(10, 0, 1),
  minutes = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)


time to get to water source (minutes) - hv204
source of drinking water - hv201
location of source for water - hv235
- 1: IN OWN DWELLING
- 2: IN OWN YARD/PLOT
- 3: ELSEWHERE
Urban / rural - HV025 

HR_sample %>% 
  transmute(source_h20 = as_factor(hv201), minutes_fetch_h20 = hv204, urban_rural = as_factor(hv025)) %>%
  filter(minutes_fetch_h20 < 900) %>%
  group_by(source_h20, urban_rural) %>%
  summarise(avg_min = mean(minutes_fetch_h20))

  
  
  df1 %>% 
    mutate(Sector = case_when(
      sector=="Public, 4-year or above" & flagship==1 ~ "PublicFlag",
      sector=="Public, 4-year or above" & flagship==0 ~ "Public",
      sector=="Private not-for-profit" ~ "PrivateNP",
      sector=="Private for-profit, 4-year or above" ~ "Private4P"),
      Sector = factor(Sector, levels=c("Public","PublicFlag","PrivateNP","Private4P"))
    )
  
  
  df1 %>%
    mutate(Sector = recode_factor(sector,
                                  "Public, 4-year or above" = "Public",
                                  "Private not-for-profit" = "PrivateNP",
                                  "Private for-profit, 4-year or above" = "Private4P"))

