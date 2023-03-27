# IMPORTANT: for a fully commented version of this program
# please check the "docs" folder on the TomPoll/SIR-pancreatic-cancer-BDIPMN
# directory on GitHub. You will find a Markdown (.rmd) file and an rendered
# PDF (.pdf) file (RECOMMENDED)



library(readr)
library(knitr)
library(tidyr)
library(dplyr)
library(lubridate)
library(kableExtra)
library(tidyverse)
library(formatR)

# The first thing that we need is to read the dataset, and assigning it 
# to the variable df. For the purpose of this markdown a randomly
# generated dataset will be used

df <- tibble(
  DOB = ymd(paste0(round(seq(1930, 1980, length=2000)), "/1/1")), 
  DSTART = DOB + runif(35, 1, 7500),
  DLAST = DSTART + runif(35, 100, 7500),
  Institution = rep(c("SGH", "B", "C", "D", "E"),400),
  ID = c(1:2000),
  Sex = sample(c("MALE", "FEMALE"), size = 2000, replace = TRUE),
  OBS = sample(c(0,1), size = 2000, replace = TRUE, prob = c(0.997, 0.003)))

# With the prob argument of the OBS variable, with have set the rate of observation 
# at 0.3% as an example, changing this number alter the final SIR significantly

# Crude rate (per 100.000) for male per age group
RaM <- tibble(group_1=0.02, group_2=0.02, group_3=0.04, group_4=0.02, 
              group_5=0.12, group_6=0.19, group_7=0.43, group_8=1.0, 
              group_9=2.7, group_10=5.7, group_11=11.5, group_12=20.1, 
              group_13=33.3, group_14=48.1, group_15=62.0, group_16=82.4, 
              group_17=98.2, group_18=106.9)

# Crude rate (per 100.000) for female per age group
RaF <- tibble(group_1=0.02, group_2=0, group_3=0.02, group_4=0.09, 
              group_5=0.17, group_6=0.27, group_7=0.38, group_8=0.83, 
              group_9=1.8, group_10=3.8, group_11=7.5, group_12=13.2, 
              group_13=22.5, group_14=35.7, group_15=52.4, group_16=67.4, 
              group_17=79.4, group_18=93.2)

# Crude rate (per 100.000) for male in SINGAPORE per age group
sRaM <- tibble(group_1=0, group_2=0, group_3=0, group_4=0.2, group_5=0, 
               group_6=0.3, group_7=0.3, group_8=1.6, group_9=3.0, 
               group_10=3.1, group_11=7.9, group_12=17.7, group_13=21.0, 
               group_14=45.1, group_15=50.8, group_16=61.0, group_17=72.5, 
               group_18=71.8)

# Crude rate (per 100.000) for female in SINGAPORE per age group
sRaF <- tibble(group_1=0, group_2=0, group_3=0, group_4=0.2, group_5=0, 
               group_6=0.1, group_7=0.3, group_8=0.6, group_9=2.0, 
               group_10=4.4, group_11=4.7, group_12=10.4, group_13=16.4, 
               group_14=26.6, group_15=38.2, group_16=63.6, group_17=52.5, 
               group_18=83.7)

# The following is to calculate person-years of patients
# from all institution excepts Singapore
person_year <- df %>%
  filter(Institution != "SGH") %>%
  rowwise() %>%
  mutate(s = list(seq.Date(DSTART, DLAST, by="days"))) %>% 
  unnest(s) %>% 
  # create a new column with the corresponding age group
  # for every date in seq.Date
  mutate(age = s-DOB, 
         group = as.numeric(floor((age/365.25)/5)+1), 
         group = ifelse(group > 18, 18, group)) %>% 
  group_by(ID, DOB, DSTART, DLAST, group, Sex, OBS) %>% 
  summarise(n = round(n()/365.25, 2)) %>% 
  ungroup %>% 
  arrange(group) %>%
  pivot_wider(names_from = "group", names_prefix = "group_", values_from="n", values_fill=0.00)

# The following is to calculate person-years of patients
# from Singapore
singapore_person_year <- df %>%
  filter(Institution == "SGH") %>%
  rowwise() %>%
  mutate(s = list(seq.Date(DSTART, DLAST, by="days"))) %>% 
  unnest(s) %>% 
  # create a new column with the corresponding age group
  # for every date in seq.Date
  mutate(age = s-DOB, 
         group = as.numeric(floor((age/365.25)/5)+1), 
         group = ifelse(group > 18, 18, group)) %>% 
  group_by(ID, DOB, DSTART, DLAST, group, Sex, OBS) %>% 
  summarise(n = round(n()/365.25, 2)) %>% 
  ungroup %>% 
  arrange(group) %>%
  pivot_wider(names_from = "group", names_prefix = "group_", values_from="n", values_fill=0.00)

totalpy <- person_year %>%
  select(starts_with("group_")) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(., cols = starts_with('group_'))
totpy <- sum(totalpy$value)

sin_totalpy <- singapore_person_year %>%
  select(starts_with("group_")) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(., cols = starts_with('group_'))
sin_totpy <- sum(totalpy$value)

py <- totpy+sin_totpy

#the following chunk is to generate a random sample
#of the person year table
kable(person_year[1:6, 1:10], booktabs = TRUE, align = "l", caption = "Person year ") %>%
  kableExtra::footnote(general = "A random sample of 6 cases is reported") %>%
  kable_styling(latex_options = "HOLD_position") 

fem_pry <- person_year %>%
  filter(Sex == "FEMALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))

men_pry <- person_year %>%
  filter(Sex == "MALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))

singapore_fem_pry <- singapore_person_year %>%
  filter(Sex == "FEMALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))

singapore_men_pry <- singapore_person_year %>%
  filter(Sex == "MALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))

FemaleEx <- bind_rows(RaF, fem_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_female_expected = (rowSums(., na.rm = TRUE)/100000))

MaleEx <- bind_rows(RaM, men_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_male_expected = (rowSums(., na.rm = TRUE)/100000))

singapore_FemaleEx <- bind_rows(sRaF, singapore_fem_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_female_expected = (rowSums(., na.rm = TRUE)/100000))

singapore_MaleEx <- bind_rows(sRaM, singapore_men_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_male_expected = (rowSums(., na.rm = TRUE)/100000))

mEx <- MaleEx$total_male_expected
mObs <- men_pry$OBS

fEx <- FemaleEx$total_female_expected
fObs <- fem_pry$OBS

sin_mEx <- singapore_MaleEx$total_male_expected
sin_mObs <- singapore_men_pry$OBS

sin_fEx <- singapore_FemaleEx$total_female_expected
sin_fObs <- singapore_fem_pry$OBS

totEx <- mEx+fEx+sin_mEx+sin_fEx
totObs <- mObs+fObs+sin_mObs+sin_fObs

SIR <- totObs/totEx
CIl <- SIR*(((1-(1/(9*totObs)))-(1.96/(3*sqrt(totObs))))^3)
CIu <- SIR*((totObs+1)/totObs)*((1-(1/(9*(totObs+1)))+(1.96/(3*sqrt(totObs+1))))^3)

mSIR <- (mObs+sin_mObs) / (mEx+sin_mEx)
mCIl <- mSIR*(((1-(1/(9*(mObs+sin_mObs))))-(1.96/(3*sqrt(mObs+sin_mObs))))^3)
mCIu <- mSIR*(((mObs+sin_mObs)+1)/(mObs+sin_mObs))*((1-(1/(9*((mObs+sin_mObs)+1)))+(1.96/(3*sqrt((mObs+sin_mObs)+1))))^3)

fSIR <- (fObs+sin_fObs) / (fEx+sin_fEx)
fCIl <- fSIR*(((1-(1/(9*(fObs+sin_fObs))))-(1.96/(3*sqrt(fObs+sin_fObs))))^3)
fCIu <- fSIR*(((fObs+sin_fObs)+1)/(fObs+sin_fObs))*((1-(1/(9*((fObs+sin_fObs)+1)))+(1.96/(3*sqrt((fObs+sin_fObs)+1))))^3)